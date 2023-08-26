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

# * CLV Inputs Table ----
clv_estimate_inputs_tbl <- customer_metrics_tbl %>% 
    left_join(customer_retention_tbl) %>% 
    left_join(
        ppc_metrics_tbl %>% 
            select(campaign, total_clicks, total_leads, total_cost)
    )


# *****************************************************************************
# **** ----
# NEW CHANNEL ESTIMATES ----
# *****************************************************************************

# - Create a table with estimates (assumptions) for clicks, leads, cost, etc
#   from other channels like linkedin, spotify and tiktok, assuming we decide to
#   create ad campaigns for those platforms.

# * New Ad Channels Data ----
# new_ad_channels_tbl <- tibble(
#     campaign        = c("Linkedin", "Spotify", "TikTok"),
#     total_clicks    = c(800, 100000, 4000),
#     total_leads     = c(75, 1500, 150),
#     total_cost      = c(6000, 17000, 6000),
#     total_customers = c(0.40 * 75, 0.28 * 1500, 0.28 * 500),
#     avg_1yr_revenue = c(335, 330, 335),
#     retention_rate  = c(0.70, 0.62, 0.68)
# )

get_paid_assumptions <- function(paid_campaigns,
                                 paid_clicks,
                                 paid_leads,
                                 paid_cost,
                                 paid_customers,
                                 paid_revenue,
                                 paid_retention) {
    
    ret <- tibble(
        campaign        = paid_campaigns,
        total_clicks    = paid_clicks,
        total_leads     = paid_leads,
        total_cost      = paid_cost,
        total_customers = paid_customers,
        avg_1yr_revenue = paid_revenue,
        retention_rate  = paid_retention
    )
    
    return(ret)
}

paid_assumptions_tbl <- get_paid_assumptions(
    paid_campaign  = c("Linkedin", "Spotify", "TikTok"),
    paid_clicks    = c(800, 100000, 4000),
    paid_leads     = c(75, 1500, 150),
    paid_cost      = c(6000, 17000, 6000),
    paid_customers = c(0.40 * 75, 0.28 * 1500, 0.28 * 500),
    paid_revenue   = c(335, 330, 335),
    paid_retention = c(0.70, 0.62, 0.68)
)


# *****************************************************************************
# **** ----
# CLV ESTIMATION ----
# *****************************************************************************

# - Estimate clv for these new platforms based on assumptions above.

# CLV Estimates ----
clv_estimate_new_ad_channels_tbl <- ppc_metrics_tbl %>% 
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

data_clv_estimates <- clv_estimate_inputs_tbl
data_paid_assumptions <- paid_assumptions_tbl

get_paid_clv_estimates <- function(data_clv_estimates, data_paid_assumptions,
                                   budget = 1000) {
    
    # data = clv estimates input table
    
    ret1 <- data_clv_estimates %>% 
        select(campaign, total_clicks, total_leads, total_cost, total_customers,
               avg_1yr_revenue, retention_rate) %>% 
        
        # remove total row
        filter(campaign != "Total") %>% 
        
        # bind rows with new ad channels data
        bind_rows(data_paid_assumptions) %>% 
        
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
    
    # cost per $1000 spend
    ret2 <- ret1 %>% 
        mutate(cpc = total_cost / total_clicks) %>% 
        mutate(est_clicks = budget / cpc) %>% 
        mutate(est_customers = ctc_cvr * est_clicks)
    
    return(ret2)
    
}

clv_estimate_paid_tbl <- get_paid_clv_estimates(
    data_clv_estimates    = clv_estimate_inputs_tbl,
    data_paid_assumptions = paid_assumptions_tbl
)


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

new_customer_per_spend_tbl <- clv_estimate_new_paid_channels_tbl %>% 
    mutate(cpc = total_cost / total_clicks) %>% 
    mutate(est_clicks = budget / cpc) %>% 
    mutate(est_customers = ctc_cvr * est_clicks)

new_customer_per_spend_tbl %>% glimpse()


# *****************************************************************************
# **** ----
# CASH FLOW ASSUMPTIONS ----
# *****************************************************************************
new_customer_per_spend_tbl %>% glimpse()

#' - Here we'll be modeling with 2 scenarios for new ad campaigns;
#' - In scenario 0, we will not take on any additional marketing cost.
#' - In scenario 1, we'll hire a new marketing manager to manage campaign ads.
#'   Thus, there will be certain additional marketing costs.


# * Scenario Inputs ----

# ** Marketing Costs ----
scenario                  <- 1 # hire new marketing manager
marketing_manager_salary  <- 80000
current_salary_allocation <- 0.20
social_media_tools_cost   <- 250 # $250 per month
ad_channel_list <- c("AdWords", "Facebook", "Linkedin", "Spotify", "TikTok")


# ** Paid Customer Acquisition ----
paid_assumptions_up <- tibble(
    campaign              = channel_list,
    monthly_ad_spend      = c(2000, 4000, 2000, 5000, 4000),
    ctc_cvr_uplift        = c(0.02, 0.03, 0, 0, 0),
    retention_rate_uplift = c(0.02, 0.03, 0.02, 0.02, 0.03)
)


paid_ad_estimate_tbl <- new_customer_per_spend_tbl %>% 
    select(campaign, cpc, retention_rate, ctc_cvr) %>% 
    left_join(paid_ad_assumptions_inputs_tbl) %>% 
    
    # estimated clicks
    mutate(clicks = monthly_ad_spend / cpc) %>% 
    
    # estimated uplift in click to customer cvr based on scenario
    mutate(ctc_cvr = case_when(
        scenario == 0 ~ ctc_cvr,
        TRUE          ~ ctc_cvr + ctc_cvr_uplift
    )) %>% 
    
    # estimated uplift in customer retention rate
    mutate(new_customers = ctc_cvr * clicks) %>% 
    mutate(retention_rate = case_when(
        scenario == 0 ~ retention_rate,
        TRUE          ~ retention_rate + retention_rate_uplift
    )) 

paid_campaign_list   = channel_list
paid_monthly_spend   = c(2000, 4000, 2000, 5000, 4000)
paid_uplift_ctc_cvr  = c(0.02, 0.03, 0, 0, 0)
paid_uplift_ret_rate = c(0.02, 0.03, 0.02, 0.02, 0.03)

get_paid_uplift_estimates <- function(data,
                                      scenario = 1,
                                      marketing_manager_salary = 80000,
                                      marketing_manager_salary_allocation = 0.20,
                                      social_media_tools_cost = 250,
                                      paid_campaign_list = NULL,
                                      paid_monthly_spend = NULL,
                                      paid_uplift_ctc_cvr = NULL,
                                      paid_uplift_ret_rate = NULL
                                      ) {
    
    # data = clv estimate for new paid
    
    # paid estimates ad spend and uplift
    paid_assumptions_uplift_tbl <- tibble(
        campaign         = paid_campaign_list,
        monthly_ad_spend = paid_monthly_spend,
        ctc_cvr_uplift   = paid_uplift_ctc_cvr,
        ret_rate_uplift  = paid_uplift_ret_rate
    )
    
    # paid new customer estimates
    paid_ad_estimate_tbl <- data %>%
        select(campaign, cpc, retention_rate, ctc_cvr) %>%
        left_join(paid_assumptions_uplift_tbl) %>%

        # estimated clicks
        mutate(clicks = monthly_ad_spend / cpc) %>%

        # estimated uplift in click to customer cvr based on scenario
        mutate(ctc_cvr = case_when(
            scenario == 0 ~ ctc_cvr,
            TRUE          ~ ctc_cvr + ctc_cvr_uplift
        )) %>%

        # estimated uplift in customer retention rate
        mutate(new_customers = ctc_cvr * clicks) %>%
        mutate(retention_rate = case_when(
            scenario == 0 ~ retention_rate,
            TRUE          ~ retention_rate + ret_rate_uplift
        ))
    
    return(paid_ad_estimate_tbl)
    
}

paid_uplift_estimate_tbl <- get_paid_uplift_estimates(
    data = clv_estimate_new_paid_channels_tbl,
    paid_campaign_list   = c("AdWords", "Facebook", "Linkedin", "Spotify", "TikTok"),
    paid_monthly_spend   = c(2000, 4000, 2000, 5000, 4000),
    paid_uplift_ctc_cvr  = c(0.02, 0.03, 0, 0, 0),
    paid_uplift_ret_rate = c(0.02, 0.03, 0.02, 0.02, 0.03)
    
)


# ** Organic Customer Acquisition ----
organic_assumption_inputs_tbl <- tibble(
    campaign        = c("Facebook Organic", "Linkedin Organic", "Twitter Organic"),
    followers       = c(2538, 528, 190),
    growth_no_mgr   = c(0.01, 0.01, 0.01),
    growth_with_mgr = c(0.08, 0.08, 0.08),
    new_flwr_cvr    = c(0.08, 0.08, 0.05),
    avg_1yr_revenue = c(335, 335, 330),
    retention_rate  = c(0.73, 0.73, 0.73)
)

organic_estimates_tbl <- organic_assumption_inputs_tbl %>% 
    mutate(growth = case_when(
        scenario == 0 ~ growth_no_mgr,
        TRUE          ~ growth_no_mgr + growth_with_mgr
    ))
    

# ** Inputs For Projections ----
projection_inputs_tbl <- clv_estimate_new_ad_channels_tbl %>% 
    
    # inputs needed for paid channels
    select(campaign, avg_1yr_revenue) %>% 
    left_join(
        paid_ad_estimate_tbl %>% 
            select(campaign, retention_rate, new_customers) 
    ) %>% 
    
    
    # inputs needed for organic channels
    bind_rows(
        organic_estimates_tbl %>% 
            select(campaign, avg_1yr_revenue, retention_rate)
    ) %>% 
    left_join(
        organic_estimates_tbl %>% 
            select(campaign, followers, growth)
    )
  

# *****************************************************************************
# **** ----
# PROJECTION ESTIMATE: SOCIAL MEDIA FOLLOWERS ----
# *****************************************************************************

# * Create Projection Dates ----
get_projection_dates <- function(start_date, end_date, by = "months") {
    
    # list of months to project
    months_list <- seq(
        from = ymd(start_date), 
        to   = ymd(end_date), 
        by   = by
    )
    
    # - Calculate the last day of each month
    last_days <- months_list %>% 
        floor_date("month") %>% 
        ceiling_date("month") - days(1)
    
    dates_tbl <- tibble(report_month = last_days)
    
    return(dates_tbl)
    
}

projection_dates_tbl <- get_projection_dates("2023-01-01", "2024-12-01")


# * Social Media Follower Estimates ----
get_social_media_followers <- function(data_inputs, campaign_name, n = 23) {
    
    pattern <- campaign_name %>% str_replace("_", " ") %>% str_to_title()
    
    initial_value <- subset(data_inputs, grepl(pattern, campaign))$followers
    
    growth <- subset(data_inputs, grepl(pattern, campaign))$growth
    
    ret <- tibble({{campaign_name}}:= initial_value * (1 + growth)^ (0:n)) 
    
    return(ret)
}

social_media_followers_tbl <- bind_cols(
    projection_dates_tbl,
    
    # facebook organic
    get_social_media_followers(
        data_inputs   = projection_inputs_tbl,
        campaign_name = "facebook_organic"
    ),
    
    # linkedin organic
    get_social_media_followers(
        data_inputs   = projection_inputs_tbl,
        campaign_name = "linkedin_organic"
    ),
    
    # twitter organic
    get_social_media_followers(
        data_inputs   = projection_inputs_tbl,
        campaign_name = "twitter_organic"
    )
    
)


# * Social Media Revenue & Customers ----

# - Function: Get Projection Input Value
get_projection_input <- function(data = projection_inputs_tbl, campaign_name,
                                 input, digits = 0) {
    
    ret <- data %>% 
        filter(campaign == campaign_name) %>% 
        pull({{input}}) %>% 
        round(digits = digits)
    
    return(ret)
}

get_projection_input(campaign_name = "Facebook", input = new_customers)

# - Function: Get Customer & Revenue Projections
get_projection_paid_customers_and_revenue <- function(campaign_name, 
                                                      churn_month = 12,
                                                      start_date  = "2023-01-01",
                                                      end_date    = "2024-12-31",
                                                      by          = "months",
                                                      input,
                                                      digits       = 0) {
    
    # date projections tbl
    dates <- get_projection_dates(start_date, end_date, by)
    
    # campaign name setup
    name <- campaign_name %>% str_replace(" ", "_") %>% str_to_lower()
    
    # inputs setup
    new_customers <- round(
        subset(data_inputs, campaign == campaign_name)$new_customers, 
        digits = 0
    )
    
    input_customers <- get_projection_input(campaign_name = campaign_name, input = new_customers)
    
    retention_rate <- round(
        subset(data_inputs, campaign == campaign_name)$retention_rate,
        digits = 2
    )
    
    avg_1yr_revenue <- round(
        subset(data_inputs, campaign == campaign_name)$avg_1yr_revenue,
        digits = 0
    )
    
    # revenue projection
    revenue_projection_tbl <- data_dates %>% 
        bind_cols(tibble(new_customers = c(NA, rep(new_customers, 23)))) %>% 
        mutate(churn = lag(new_customers, 12)) %>% 
        mutate(churn = case_when(
            is.na(churn) ~ 0,
            TRUE         ~ -round((churn * (1 - retention_rate)), 0)
        )) %>% 
        mutate(
            total_customers = cumsum(
                ifelse(is.na(new_customers + churn), 0, new_customers + churn)
            )
        ) %>% 
        mutate(revenue = (avg_1yr_revenue / 12) * total_customers) %>% 
        mutate(new_customers = ifelse(is.na(new_customers), 0, new_customers)) %>% 
        mutate(campaign_name:= {{name}})
    
    # return
    return(revenue_projection_tbl)
    
}

get_ad_campaign_revenue_projections(campaign_name = "Facebook")


ad_channel_list %>% 
    map(.f = function(list) {
        get_ad_campaign_revenue_projections(
            data_dates    = projection_dates_tbl,
            data_inputs   = projection_inputs_tbl,
            campaign_name = list
        )
    })




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



