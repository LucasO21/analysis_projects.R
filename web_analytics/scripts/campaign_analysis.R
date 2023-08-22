# PPC ANALYSIS SCRIPT:
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

# *****************************************************************************
# **** ----
# PPC Analysis ----
# *****************************************************************************

# * Data Import ----

# ** Function ----
get_campaign_data <- function() {
    
    # column names
    colnames <- c("date", "campaign_id", "impressions", "clicks", "cost", "ctr", 
                  "conversions", "cvr")
    
    # ad words tbl
    adwords_tbl <- readxl::read_excel(
        path  = "../data/Marketing+Analytics+Case+Study.xlsm",
        sheet = "PPC Data",
        skip  = 3
    ) %>% 
        clean_names() %>% 
        select(date, starts_with("ad_words")) %>% 
        `colnames<-`(colnames) %>% 
        mutate(campaign = "AdWords")
    
    # facebook tbl
    facebook_tbl <- readxl::read_excel(
        path  = "../data/Marketing+Analytics+Case+Study.xlsm",
        sheet = "PPC Data",
        skip  = 3
    ) %>% 
        clean_names() %>% 
        select(date, starts_with("facebook")) %>% 
        `colnames<-`(colnames) %>% 
        mutate(campaign = "Facebook")
    
    # combined tbl
    combined_tbl <- bind_rows(adwords_tbl, facebook_tbl) %>% 
        mutate(date = ymd(date))
    
    return(combined_tbl)
    
}

campaign_tbl <- get_campaign_data()

campaign_tbl %>% glimpse()


# * Key Question ----

#' - 1. Which PPC source has the highest/lowest CTR?
#' - 2. Which PPC source has the highest conversions of clicks to leads?
#' - 3. Why might FB have a higher conversion rate?
#' - 4. Based on this information, is one PPC ad type better than the other?


# * Metrics by Ad Campaign ----

# ** Function ----

# - Aggregates
get_campaign_metrics <- function(data, metric = "cost", level = "campaign", campaign = NULL) {
    
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

# get_campaign_metrics(campaign_tbl, level = "campaign_id", campaign = "Facebook")
# get_campaign_metrics(campaign_tbl, metric = "cost", level = "campaign")


# - Conversion Metrics Function
get_campaign_metrics_table <- function(data, level = "campaign", campaign = NULL) {
    
    # aggregates
    cost_tbl <- get_campaign_metrics(data, "cost", level, campaign)
    
    impr_tbl <- get_campaign_metrics(data, "impressions", level, campaign)
    
    clicks_tbl <- get_campaign_metrics(data, "clicks", level, campaign)
    
    leads_tbl <- get_campaign_metrics(data, "conversions", level, campaign)
    
    
    # ret
    ret <- cost_tbl %>% 
        left_join(impr_tbl) %>% 
        left_join(clicks_tbl) %>% 
        left_join(leads_tbl) %>% 
        rename(total_leads = total_conversions) %>% 
        mutate(
            ctr      = total_clicks / total_impressions,
            cpc      = total_cost / total_clicks,
            cvr = total_leads / total_clicks
        )
    
    # return
    return(ret)
    
}

# * Metrics by Campaign (Overall) ----
campaign_metrics_tbl <- get_campaign_metrics_table(campaign_tbl, "campaign")

#' Observations:
#' - 1. Overall ctr is inline with industry average.
#' - 2. Ad words here has lower CTR than Facebook and below industry average, which is unusual.
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
aw_metrics_by_ad_tbl <- get_conversion_metics(
    data     = campaign_tbl,
    level    = "campaign_id",
    campaign = "Ad Words"
)

# ** Facebook ----
fb_metrics_by_ad_tbl <- get_conversion_metics(
    data     = campaign_tbl,
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

lead_acquisition_cost_tbl <- campaign_metrics_tbl %>% 
    select(campaign, ctr, cpc, cvr) %>% 
    mutate(clicks = budget / cpc) %>% 
    mutate(leads = cvr * clicks) %>% 
    mutate(lac = budget / leads)

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
        "get_campaign_data",
        "get_campaign_metrics",
        "get_campaign_metrics_table"
    ),
    file = "../functions/ppc_functions.R",
    append = FALSE
)


# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
