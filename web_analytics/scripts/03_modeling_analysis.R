# MARKETING ANALYTICS ----
# MODELING / PROJECTIONS ----
# *** ----

# *****************************************************************************
# **** ----
# ABOUT ----
# *****************************************************************************

#' - In this script, we want to;
#' - Estimate cac and clv for new add channels such as facebook and linkedin.
#' - Compare cac and clv for these new channels with facebook and adwords. 
#' - First we'll create some assumptions, then use those assumptions to model out. 

# - Other Resources;
# - https://influencermarketinghub.com/how-much-do-tiktok-ads-cost/


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
source(file = "../functions/customer_analysis_functions.R")

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

# * Leads Data ----
leads_tbl <- read_rds("../data/data_clean/leads_data.rds")

# * Survey Data ----
survey_tbl <- read_rds("../data/data_clean/survey_data.rds")

# * Campaign Data ----
campaign_tbl <- read_rds("../data/data_clean/campaign_data.rds")


# *****************************************************************************
# **** ----
# ASSUMPTION TABLE 1 ----
# *****************************************************************************

# * New Ad Channels Data ----
new_ad_channels_tbl <- tibble(
    campaign        = c("Linkedin", "Spotify", "TikTok"),
    total_clicks    = c(800, 100000, 1000),
    total_cost      = c(6000, 17000, 2000),
    total_leads     = c(75, 1500, 500),
    total_customers = c(0.28 * 75, 0.28 * 1500, 0.28 * 500),
    avg_1yr_revenue = c(335, 330, 335),
    retention_rate  = c(0.70, 0.62, 0.68)
)

ppc_metrics_tbl %>% 
    select(campaign, total_clicks, total_leads, total_cost) %>% 
    left_join(
        customer_metrics_tbl %>% 
            select(campaign, count_of_customers, avg_1yr_revenue)
    ) %>% 
    left_join(
        repeat_customers_tbl %>% 
            filter(repeat_purchase == "Repeat") %>% 
            select(-repeat_purchase) %>% 
            gather(key = "campaign", value = "retention_rate")
    ) %>% 
    
    # remove total row
    filter(campaign != "Total") %>% 



# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************




# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************



# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************



