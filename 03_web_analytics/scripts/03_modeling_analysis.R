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
setwd(here::here("03_web_analytics", "scripts"))

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

# * Repeat Customers Data ----
customer_retention_tbl <- read_rds("../data/data_clean/customer_retention_data.rds")

# * Customer Metrics ----
customer_metrics_tbl <- get_customer_analysis_final_output(
    data_leads  = leads_tbl,
    data_survey = survey_tbl,
    output      = "customer_metrics"
)

# * PPC Metrics ----
ppc_metrics_tbl <- get_ppc_final_output(
    data   = campaign_tbl,
    level  = "campaign",
    output = "metrics"
)

# * Customer Retention ----
customer_retention_tbl <- get_customer_analysis_final_output(
    data_leads  = leads_tbl,
    data_survey = survey_tbl,
    output      = "customer_retention"
)


# *****************************************************************************
# **** ----
# NEW CHANNEL ESTIMATES ----
# *****************************************************************************

# - Create a table with estimates (assumptions) for clicks, leads, cost, etc
#   from other channels like linkedin, spotify and tiktok, assuming we decide to
#   create ad campaigns for those platforms.

# * New Ad Channels Data ----
new_ad_channels_tbl <- tibble(
    campaign        = c("Linkedin", "Spotify", "TikTok"),
    total_clicks    = c(800, 100000, 4000),
    total_leads     = c(75, 1500, 150),
    total_cost      = c(6000, 17000, 6000),
    total_customers = c(0.40 * 75, 0.28 * 1500, 0.28 * 500),
    avg_1yr_revenue = c(335, 330, 335),
    retention_rate  = c(0.70, 0.62, 0.68)
)


# *****************************************************************************
# **** ----
# CLV ESTIMATION ----
# *****************************************************************************

# - Estimate clv for these new platforms based on assumptions above.

# CLV Estimates ----
gross_margin  <- 0.75
discount_rate <- 0.10

new_channels_clv_estimate_tbl <- ppc_metrics_tbl %>% 
    select(campaign, total_clicks, total_leads, total_cost) %>% 
    left_join(
        customer_metrics_tbl %>% 
            select(campaign, total_customers, avg_1yr_revenue)
    ) %>% 
    left_join(customer_retention_tbl) %>% 
    
    # remove total row
    filter(campaign != "Total") %>% 
    
    # bind rows with new ad channels data
    bind_rows(new_ad_channels_tbl) %>% 
    
    # lead conversion rate
    mutate(ctc_cvr = total_customers / total_clicks) %>% 
    
    # cac
    mutate(cac = total_cost / total_customers) %>% 
    
    # clv estimates
    mutate(gross_profit = avg_1yr_revenue * gross_margin) %>% 
    mutate(clv_gross = gross_profit * (
        retention_rate / (1 + discount_rate - retention_rate)
        )) %>% 
    mutate(clv_net = clv_gross - cac)


# - Observation:
# - We might as well advertise on these platforms since we will potentially make
# - money from them. 


# *****************************************************************************
# **** ----
# NEW CUSTOMERS ESTIMATE PER $1000 ----
# *****************************************************************************

# - Based on assumptions above, for every $1000 spent, how many new customers 
#   can we acquire?

budget <- 1000

new_customer_per_1000_tbl <- new_channels_clv_estimate_tbl %>% 
    mutate(cpc = total_cost / total_clicks) %>% 
    mutate(est_clicks = budget / cpc) %>% 
    mutate(est_customers = ctc_cvr * est_clicks)

new_customer_per_1000_tbl %>% glimpse()


# *****************************************************************************
# **** ----
# CASH FLOW ASSUMPTIONS ----
# *****************************************************************************
new_customer_per_1000_tbl %>% glimpse()

#' - Here we'll be modeling with 2 scenarios for new ad campaigns;
#' - In scenario 0, we will not take on any additional marketing cost.
#' - In scenario 1, we'll hire a new marketing manager to manage campaign ads.


# * Scenario Inputs ----

# ** Marketing Costs ----
scenario                  <- 1 # hire new marketing manager
marketing_manager_salary  <- 80000
current_salary_allocation <- 0.20
social_media_tools_cost   <- 250 # $250 per month
channel_list <- c("AdWords", "Facebook", "Linkedin", "Spotify", "TikTok")


# ** Paid Customer Acquisition ----
paid_assumptions_inputs_tbl <- tibble(
    campaign            = channel_list,
    monthly_ad_spend    = c(2000, 4000, 2000, 5000, 4000),
    est_ctc_cvr_uplift  = c(0.02, 0.03, 0, 0, 0),
    est_retention_rate_uplift = c(0.02, 0.03, 0.02, 0.02, 0.03)
)


paid_assumptions_tbl <- new_customer_per_1000_tbl %>% 
    select(campaign, cpc, retention_rate, ctc_cvr) %>% 
    left_join(paid_assumptions_inputs_tbl) %>% 
    
    # estimated clicks
    mutate(est_clicks = monthly_ad_spend / cpc) %>% 
    
    # estimated uplift in click to customer cvr based on scenario
    mutate(est_new_ctc_cvr = case_when(
        scenario == 0 ~ ctc_cvr,
        TRUE          ~ ctc_cvr + est_ctc_cvr_uplift
    )) %>% 
    
    # estimated uplift in customer retention rate
    mutate(est_new_customers = est_new_ctc_cvr * est_clicks) %>% 
    mutate(est_new_retention_rate = case_when(
        scenario == 0 ~ retention_rate,
        TRUE          ~ retention_rate + est_retention_rate_uplift
    )) 
    
    

    









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

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************



