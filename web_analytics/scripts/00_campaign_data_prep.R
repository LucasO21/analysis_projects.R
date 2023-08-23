# MARKETING ANALYTICS ----
# DATA PREP SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("web_analytics", "scripts"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)

# *****************************************************************************
# **** ----
# CAMPAIGN DATA ----
# *****************************************************************************

# - Import the campaign data (PPC Analysis sheet), format and save

# * Load Campaign Data ----
get_campaign_data <- function() {
    
    # column names
    colnames <- c(
        "date", 
        "campaign_id", 
        "impressions", 
        "clicks", 
        "cost", 
        "ctr", 
        "conversions", "cvr"
    )
    
    # ad words tbl
    adwords_tbl <- readxl::read_excel(
        path  = "../data/data_raw/Marketing+Analytics+Case+Study.xlsm",
        sheet = "PPC Data",
        skip  = 3
    ) %>% 
        clean_names() %>% 
        select(date, starts_with("ad_words")) %>% 
        `colnames<-`(colnames) %>% 
        mutate(campaign = "AdWords")
    
    # facebook tbl
    facebook_tbl <- readxl::read_excel(
        path  = "../data/data_raw/Marketing+Analytics+Case+Study.xlsm",
        sheet = "PPC Data",
        skip  = 3
    ) %>% 
        clean_names() %>% 
        select(date, starts_with("facebook")) %>% 
        `colnames<-`(colnames) %>% 
        mutate(campaign = "Facebook")
    
    # combined tbl
    combined_tbl <- bind_rows(
        adwords_tbl, 
        facebook_tbl
    ) %>% 
        mutate(date = ymd(date))
    
    return(combined_tbl)
    
}

campaign_tbl <- get_campaign_data()

campaign_tbl %>% glimpse()


# Save Campaign Data ----
campaign_tbl %>% write_rds("../data/data_clean/campaign_data.rds")



# *****************************************************************************
# **** ----
# LEADS DATA ----
# *****************************************************************************

# - Import the lead data (Lead Data sheet), format and save

leads_tbl <- readxl::read_excel(
    path  = "../data/data_raw/Marketing+Analytics+Case+Study.xlsm",
    sheet = "Lead Data",
    skip  = 3
) %>% 
    clean_names() %>% 
    mutate(sign_up_date = ymd(sign_up_date)) %>% 
    mutate(date_of_1st_purchase = ymd(date_of_1st_purchase)) %>% 
    
    # add days to purchase column
    mutate(
        days_to_purchase = difftime(
            time1        = sign_up_date, 
            time2        =  date_of_1st_purchase, 
            units        = "days"
        ) %>% as.numeric %>% abs()
    ) %>% 
    
    # add number of touch points column
    mutate(touch_points = case_when(
        purch_30d_post_web   == 1 ~ 1,
        purch_30d_post_email == 1 ~ 2,
        purch_15d_post_email == 1 ~ 3,
        TRUE                      ~ NA
    ))

# * Save Leads Data ----
leads_tbl %>% write_rds("../data/data_clean/leads_data.rds")



# *****************************************************************************
# **** ----
# SURVEY DATA ----
# *****************************************************************************

# * Load Survey Data ----
survey_tbl <- readxl::read_excel(
    path  = "../data/data_raw/Marketing+Analytics+Case+Study.xlsm",
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
            "repeat_purchase", 
            "repeat_purchase_reason"
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
    mutate(repeat_purchase = repeat_purchase %>% fct_relevel(
        "Very likely", "Likely", "Not likely", "Not sure", "Never again"
    ))

survey_tbl %>% glimpse()

# * Save Survey Data ----
survey_tbl %>% write_rds("../data/data_clean/survey_data.rds")



# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
