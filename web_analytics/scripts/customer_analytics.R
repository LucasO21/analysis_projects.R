# LEAD ANALYSIS ----
# *** ----

# *****************************************************************************
# **** ----
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("web_analytics", "scripts"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)

# * Source ----
source(file = "../functions/ppc_functions.R")

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

# * Leads Data ----
leads_tbl <- readxl::read_excel(
    path  = "../data/Marketing+Analytics+Case+Study.xlsm",
    sheet = "Lead Data",
    skip  = 3
) %>% 
    clean_names() %>% 
    mutate(sign_up_date = ymd(sign_up_date)) %>% 
    mutate(date_of_1st_purchase = ymd(date_of_1st_purchase)) %>% 
    
    # add days to purchase column
    mutate(
        days_to_purchase = difftime(
            time1 = sign_up_date, 
            time2 =  date_of_1st_purchase, 
            units = "days"
        ) %>% as.numeric %>% abs()
    ) %>% 
    
    # add number of touch points column
    mutate(touch_points = case_when(
        purch_30d_post_web == 1   ~ 1,
        purch_30d_post_email == 1 ~ 2,
        purch_15d_post_email == 1 ~ 3,
        TRUE                      ~ NA
    ))

leads_tbl %>% glimpse()
leads_tbl %>% View()

# * Survey Data ----
survey_tbl <- readxl::read_excel(
    path  = "../data/Marketing+Analytics+Case+Study.xlsm",
    sheet = "Survey Data",
    skip  = 3
) %>% 
    clean_names() %>% 
    select(-c(x8:x11)) %>% 
    
    # shorten names of survey questions
    `colnames<-`(
        c(
            "lead_source", 
            "age", 
            "age_group", 
            "recommend", 
            "hear_about",
            "re_purchase", 
            "re_purchase_reason"
        )
    ) %>% 
    
    # add age groups column
    mutate(age_group = case_when(
        age %>% between(18, 25) ~ "18 - 25",
        age %>% between(26, 35) ~ "26 - 35",
        age %>% between(36, 45) ~ "36 - 45",
        age %>% between(46, 55) ~ "46 - 55",
        TRUE                    ~ "55+"
    ))

survey_tbl %>% glimpse()

# * Campaign Data ----
campaign_tbl <- get_campaign_data()


# *****************************************************************************
# **** ----
# CUSTOMER STATS ----
# *****************************************************************************

# * Customer Metrics Table ----
customer_metrics_tbl <- leads_tbl %>% 
    filter(total_1yr_purch_net > 0) %>% 
    select(lead_source, days_to_purchase, total_1yr_purch_net, touch_points) %>% 
    mutate(total = "Total") %>%
    pivot_longer(
        cols      = -c(days_to_purchase, total_1yr_purch_net, touch_points),
        names_to  = "name",
        values_to = "campaign"
    ) %>% 
    summarise(
        count_of_customers = n(),
        avg_days_to_purch  = mean(days_to_purchase, na.rm = TRUE),
        avg_1yr_revenue    = mean(total_1yr_purch_net, na.rm = TRUE),
        touch_points       = mean(touch_points, na.rm = TRUE),
        .by = campaign
    ) %>% 
    arrange(campaign) %>% 
    
    # average customer age
    left_join(
        survey_tbl %>% 
            summarise(avg_age = mean(age, na.rm = TRUE), .by = lead_source) %>% 
            add_row(lead_source = "Total", avg_age = mean(survey_tbl$age)),
        by = c("campaign" = "lead_source")
    ) %>% 
    
    # customer acquisition cost
    left_join(
        get_campaign_metrics(
            data    = campaign_tbl,
            metric  = "cost",
            level   = "campaign"
        )
    ) %>% 
    mutate(customer_acq_cost = total_cost / count_of_customers) %>% 
    select(-total_cost) %>% 
    
    # lead to customer cvr
    left_join(
        get_campaign_metrics(
            data   = campaign_tbl,
            metric = "conversions",
            level  = "campaign"
        )
    ) %>% 
    mutate(lead_cvr = count_of_customers / total_conversions) %>% 
    select(-total_conversions)

customer_metrics_tbl


#' - Facebook has more leads that convert to customers.
#' - Thus facebook has lower cac.
#' - Facebook customers spend on average $9 more than Adwords. Not significant. 


# *****************************************************************************
# **** ----
# SURVEY DATA DEEP DIVE ----
# *****************************************************************************

# * Customer Demographics (Total) ----
survey_tbl %>% 
    count(age_group) %>% 
    mutate(pct = n / sum(n))

# * Customer Demographics ----
age_demographics_tbl <- survey_tbl %>% 
    select(lead_source, age_group) %>% 
    mutate(total = "Total") %>% 
    pivot_longer(
        cols      = -(age_group),
        names_to  = "name",
        values_to = "campaign"
    ) %>% 
    summarise(count = n(), .by = c(campaign, age_group))

# * Raw Volume ----
age_demographics_tbl %>% 
    pivot_wider(
        names_from  = age_group,
        values_from = count
    ) %>% 
    arrange(campaign)

# * Within Age Group PCT ----
age_demographics_tbl %>% 
    filter(!campaign == "Total") %>% 
    group_by(age_group) %>% 
    mutate(total = count/sum(count)) %>% 
    ungroup() %>% 
    select(-count) %>% 
    pivot_wider(
        names_from = age_group,
        values_from = total
    )
    

# * Within Campaign Percent ----
age_demographics_tbl %>% 
    group_by(campaign) %>% 
    mutate(pct = count/sum(count)) %>% 
    ungroup() %>% 
    select(-count) %>% 
    pivot_wider(
        names_from  = age_group,
        values_from = pct
    ) %>% 
    arrange(campaign)

# - Observations:
# - Facebook tends to attract more millennial than adwords.
# - Forbes survey found that only 1% of millennial surveyed said they will trust 
#   a brand based on an ad. Millennial believe that advertising is all spin and 
#   not authentic.
#   - Millennials are more trusting of their friends, influencers and social network. 
# - SHOULD WE KILL ADWORDS? Since facebook is outperforming adwords in all metrics.
# - Adwords is bringing in a different set of customers. Good diversification.
# - Adwords might be more expensive but we should keep it.
# - People still rely on google and still tend to click on ads. 


# *****************************************************************************
# **** ----
# NPS ANALYSIS ----
# *****************************************************************************
nps_pct_tbl <- survey_tbl %>% 
    select(lead_source, recommend) %>% 
    mutate(total = "Total") %>% 
    pivot_longer(
        cols = -c(recommend),
        names_to = "name",
        values_to = "campaign"
    ) %>% 
    count(campaign, recommend) %>% 
    group_by(campaign) %>% 
    mutate(pct = n/sum(n)) %>% 
    ungroup()

nps_pct_tbl %>% print(n = 50)
    
nps_pct_tbl %>% 
    filter(recommend <= 6) %>% 
    summarise(below_6 = sum(pct), .by = campaign) %>% 
    left_join(
        nps_pct_tbl %>% 
            filter(recommend > 8) %>% 
            summarise(above_9 = sum(pct), .by = campaign)
    ) %>% 
    mutate(nps = above_9 - below_6)
    

# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************

# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************

# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
