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
    
    # days to purchase
    mutate(
        days_to_purchase = difftime(
            time1 = sign_up_date, 
            time2 =  date_of_1st_purchase, 
            units = "days"
        ) %>% as.numeric %>% abs()
    ) %>% 
    
    # number of touch points
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
    `colnames<-`(c("lead_source", "age", "age_group", "recommend", "hear_about",
                   "re_purchase", "re_purchase_reason"))

survey_tbl %>% glimpse()

# * Campaign Data ----
campaign_tbl <- get_campaign_data()


# *****************************************************************************
# **** ----
# CUSTOMER STATS ----
# *****************************************************************************

# * Functions ----

# - Customer Metrics

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
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
