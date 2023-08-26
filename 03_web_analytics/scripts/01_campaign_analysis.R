# MARKETING ANALYSIS ----
# CAMPAIGN ANALYSIS SCRIPT ----
# *** ----

# *****************************************************************************
# **** ----
# ABOUT ----
# *****************************************************************************

#' - In this script, we want to answer some key questions
#' - 1. Which PPC source has the highest/lowest CTR?
#' - 2. Which PPC source has the highest conversions of clicks to leads?
#' - 3. Why might FB have a higher conversion rate?
#' - 4. Based on this information, is one PPC ad type better than the other?


# *****************************************************************************
# **** ----
# GLOSSARY ----
# *****************************************************************************
#' - ctr: click through rate
#' - cpc: cost per click
#' - ctl_cvr: click to lead conversion rate
#' - cpl: cost per lead
#' - cac: customer acquisition cost


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


# *****************************************************************************
# **** ----
# PPC DATA IMPORT ----
# *****************************************************************************

ppc_tbl <- read_rds("../data/data_clean/campaign_data.rds")

ppc_tbl %>% glimpse()

# *****************************************************************************
# **** ----
# PPC ANALYSIS ----
# *****************************************************************************

# * Metrics by Campaign ----

# ** Function ----

# - Aggregates
get_ppc_metrics <- function(data, metric = "cost", level = "campaign", 
                            campaign = NULL) {
    
    # rlang setup
    metric <- rlang::sym(metric)
    level  <- rlang::sym(level)
    
    # filtering setup
    if (is.null(campaign)) {
        campaign_name = campaign
    } else if (campaign == "AdWords") {
        campaign_name = "AdWords"
    } else {
        campaign_name = "Facebook"
    }
    
    # data prep
    if (is.null(campaign)) {
        data <- data
    } else {
        data <- data %>% filter(campaign == campaign_name)
    }
    ret <- data %>% 
        select(!!level, !!metric) %>% 
        mutate(total = "Total") %>% 
        pivot_longer(cols = -!!metric, names_to = "key", values_to = "campaign") %>% 
        summarise(!!paste0("total_", metric):= sum(!!metric), .by = campaign) %>% 
        arrange(campaign)
    
    # return
    return(ret)
    
}

# get_ppc_metrics(ppc_tbl, level = "campaign_id", campaign = "Facebook")
# get_ppc_metrics(ppc_tbl, metric = "cost", level = "campaign")


# - Conversion Metrics Function
get_ppc_metrics_table <- function(data, level = "campaign", campaign = NULL) {
    
    # aggregates
    cost_tbl   <- get_ppc_metrics(data, "cost", level, campaign)
    
    impr_tbl   <- get_ppc_metrics(data, "impressions", level, campaign)
    
    clicks_tbl <- get_ppc_metrics(data, "clicks", level, campaign)
    
    leads_tbl  <- get_ppc_metrics(data, "conversions", level, campaign)
    
    
    # calculation
    ppc_metrics_tbl <- cost_tbl %>% 
        left_join(impr_tbl) %>% 
        left_join(clicks_tbl) %>% 
        left_join(leads_tbl) %>% 
        rename(total_leads = total_conversions) %>% 
        mutate(
            ctr     = total_clicks / total_impressions,
            cpc     = total_cost / total_clicks,
            ctl_cvr = total_leads / total_clicks
        )
    
    # return
    return(ppc_metrics_tbl)
    
}

# * Metrics by Campaign (Overall) ----
ppc_metrics_tbl <- get_ppc_metrics_table(ppc_tbl, "campaign")

#' Observations:
#' - 1. Overall ctr is inline with industry average.
#' - 2. Ad words here has lower CTR than Facebook and below industry avg, which is unusual.
#' - 3. Total cost per click is inline with industry average.
#' - 4. We spent more on facebook to get clicks than ad words. 
#' - 5. We got more clicks from facebook but we also spent more to get those clicks. 
#' - 6. We also had higher lead cvr from facebook.


#' Questions:
#' - 1. Why was facebook more successful at getting clicks than ad words?
#' - 2. Was the extra money spent on facebook worth it?


# * Metrics by Campaign ID ----
#   - Using the functions above, we calculate the metrics by individual ads

# ** Adwords ----
get_ppc_metrics_table(
    data     = ppc_tbl,
    level    = "campaign_id",
    campaign = "AdWords"
)

# ** Facebook ----
get_ppc_metrics_table(
    data     = ppc_tbl,
    level    = "campaign_id",
    campaign = "Facebook"
)

#' Observation:
#' - 1. No outliers in ctr, cpc or cvr for either adwords or facebook.
#' - So why is facebook till dominating adwords. Is it worth it?
#' - Next we calculate the average cost per lead (or cac)


# * Cost Per Lead ----
#   - Assuming we have a fixed budget, which platform will have the cac?
#   - To answer this, we do a little bit of modeling

# - Budget ($)
budget <- 1000

cost_per_lead_tbl <- ppc_metrics_tbl %>% 
    select(campaign, ctr, cpc, ctl_cvr) %>% 
    mutate(clicks = budget / cpc) %>% 
    mutate(leads = ctl_cvr * clicks) %>% 
    mutate(cpl = budget / leads)

# - Cost Per Lead Function 
get_cost_per_lead <- function(data, budget) {
    
    # uses ppc metrics data
    cost_per_lead_tbl <- data %>% 
        select(campaign, ctr, cpc, ctl_cvr) %>% 
        mutate(clicks = budget / cpc) %>% 
        mutate(leads = ctl_cvr * clicks) %>% 
        mutate(cpl = budget / leads)
    
    return(cost_per_lead_tbl)
    
}

get_cost_per_lead(ppc_metrics_tbl, 1000)


# *****************************************************************************
# **** ----
# FINAL OUTPUT ----
# *****************************************************************************

# - We want 1 final table with ppc metrics and cost per lead

get_ppc_final_output <- function(data, budget = 1000, level = "campaign",
                                 output = "cpl") {
    
    metrics <- get_ppc_metrics_table(data, level = "campaign") 
        
    cpl <- get_cost_per_lead(data = metrics, budget = budget)
    
    if (output == "cpl") {
        ret <- cpl
    } else {
        ret <- metrics
    }
    
    return(ret)
    
}

get_ppc_final_output(data = ppc_tbl, output = "metrics")

# Output:
# get_ppc_metrics >> get_ppc_metrics_table >> get_cost_per_lead

#' Observation:
#' - Adwords will generate more clicks than facebook .
#' - However since facebook has higher cvr, facebook will generate more leads.
#' - Adwords cost $5.00 for every lead compared with facebook.
#' - At a lower selling point, cpl really matters compares with a higher selling product.
#' - Overall facebook has the lower acquisition cost. 
#' - We still need to analyze what happens after we acquire a lead. 
#' 
#' Questions:
#' - What happens after we acquire a lead?
#' - Do facebook leads convert to sales at a higher rate than leads from adwords?
#' - How much do facebook customers spend vs adwords customers?



# *****************************************************************************
# **** ----
# SAVE FUNCTIONS ----
# *****************************************************************************

# * Save Functions ----
dump(
    list = c(
        "get_ppc_metrics",
        "get_ppc_metrics_table",
        "get_cost_per_lead",
        "get_ppc_final_output"
    ),
    file = "../functions/ppc_functions.R",
    append = FALSE
)



# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
