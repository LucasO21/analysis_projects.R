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
            "first_contact",
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
    )) %>% 
    
    # mutate
    mutate(re_purchase = re_purchase %>% fct_relevel(
        "Very likely", "Likely", "Not likely", "Not sure", "Never again"
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

#' - Observation:
#' - Facebook has much higher nps than adwords.
#' - This could be because the majority of our customers come from facebook.
#' - Additionally facebook customers are much younger than adwords customers.
    

# *****************************************************************************
# **** ----
# SURVEY ANALYSIS ----
# *****************************************************************************

# * Survey Analysis ----
survey_tbl %>% 
    select(lead_source, hear_about) %>% 
    mutate(total = "Total") %>% 
    pivot_longer(
        cols = -c(hear_about),
        names_to = "name",
        values_to = "campaign"
    ) %>% 
    count(campaign, hear_about) %>% 
    mutate(pct = n/sum(n), .by = campaign) %>% 
    select(-n) %>% 
    pivot_wider(names_from = campaign, values_from = pct)


#' - Observation:
#' - Facebook is the best channel at word of mouth, given that it is a social network.
#' - Adwords is better for online search


# * Function ----
get_pivot_table <- function(data, select_cols = NULL, unpivot_cols = NULL,
                            group_cols = NULL, return = "volume", format = FALSE) {
    
    
    # rlang setup
    select_cols   <- rlang::enquos(select_cols)
    unpivot_cols  <- rlang::enquo(unpivot_cols)
    group_cols    <- rlang::enquos(group_cols)
    
    # group calc
    group_tbl <- data %>% 
        select(!!!select_cols) %>% 
        mutate(total = "Total") %>%   
        pivot_longer(
            cols      = -!!unpivot_cols,
            names_to  = "name",
            values_to = "campaign"
        ) %>% 
        count(campaign, !!unpivot_cols) %>% 
        mutate(pct = n/sum(n), .by = campaign) 
    
    # total calc
    # total_tbl <- group_tbl %>% 
    #     summarise(n = sum(n), .by = campaign) %>% 
    #     mutate(pct = n/sum(n))
    
    
    # final calc
    if (return == "volume") {
        result_tbl <- group_tbl %>% 
            select(-pct) %>% 
            pivot_wider(names_from = campaign, values_from = n, values_fill = 0) %>% 
            adorn_totals("row", name = "Grand Total")
        
    } else if (return == "percent") {
        result_tbl <- group_tbl %>% 
            select(-n) %>% 
            pivot_wider(names_from = campaign, values_from = pct, values_fill = 0) %>% 
            adorn_totals("row", name = "Grand Total")
        
        if (format) {
            result_tbl <- result_tbl %>% 
                mutate_at(2:4, ~format(scales::percent(., accuracy = 0.01), nsmall = 4))
        } else {
                result_tbl <- result_tbl
            }
    }
    
    return(result_tbl)
}

# * First Contact Table ----
first_contact_tbl <- get_pivot_table(
    data         = survey_tbl,
    select_cols  = c(lead_source, first_contact),
    unpivot_cols = first_contact,
    return       = "percent"
)


# * Re - Purchase Table ----
re_purchase_tbl <- get_pivot_table(
    data         = survey_tbl,
    select_cols  = c(lead_source, re_purchase),
    unpivot_cols = re_purchase,
    return       = "percent"
)

# * Repeat Customers ----
repeat_customers_tbl <- re_purchase_tbl %>% 
    filter(re_purchase %in% c("Very likely", "Likely")) %>% 
    bind_rows(
        tibble(
            re_purchase = "Repeat",
            AdWords     = sum(re_purchase_tbl[1:2, ]$AdWords),
            Facebook    = sum(re_purchase_tbl[1:2, ]$Facebook),
            Total       = sum(re_purchase_tbl[1:2, ]$Total),
        )
    )
    

#' - Observation:
#' - 62% of millennial said that if a brand engages with them on social networks,
#' - they are more likely to become a loyal customer.
#' - Since facebook is successful, maybe we should utilize other social networks. 



# *****************************************************************************
# **** ----
# CUSTOMER LIFETIME VALUE (CLV) ----
# *****************************************************************************

# * CLV Analysis ----

discount_rate <- 0.10

gross_margin <- 0.75

customer_metrics_tbl %>% 
    select(campaign, avg_1yr_revenue, customer_acq_cost) %>% 
    
    # gross profit
    mutate(gross_profit = avg_1yr_revenue * gross_margin) %>% 
    
    # customer retention rate
    left_join(
        repeat_customers_tbl %>% 
            filter(re_purchase == "Repeat") %>% 
            select(!re_purchase) %>% 
            gather() %>% 
            rename(customer_retention_rate = value),
        by = c("campaign" = "key")
    ) %>% 
    
    # clv gross
    mutate(
        clv_gross = gross_profit * (
            customer_retention_rate / (1 + discount_rate - customer_retention_rate)
        )
    ) %>% 
    
    # clv net
    mutate(clv_net = clv_gross - customer_acq_cost)

#' - Observation:
#' - Facebook has higher clv because facebook has better customer retention. 


#' - Recommendation:
#' - Create more campaigns on facebook,linkedin, and tiktok and gain more followers.
#' - It is cheaper to gain leads organically than paid ads.
#' - Tailor promotions to increase customer retention.
#' - Continue to analyze ad data monthly. 
    
    


# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************




