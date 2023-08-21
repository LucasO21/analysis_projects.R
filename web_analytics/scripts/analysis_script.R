# SCRIPT TOPIC:
# SCRIPT NOTES:
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
colnames <- c("date", "campaign", "impressions", "clicks", "cost", "ctr", 
              "conversions", "cvr")

adwords_tbl <- readxl::read_excel(
    path  = "../data/Marketing+Analytics+Case+Study.xlsm",
    sheet = "PPC Data",
    skip  = 3
) %>% 
    clean_names() %>% 
    select(date, starts_with("ad_words")) %>% 
    `colnames<-`(colnames)

facebook_tbl <- readxl::read_excel(
    path  = "../data/Marketing+Analytics+Case+Study.xlsm",
    sheet = "PPC Data",
    skip  = 3
) %>% 
    clean_names() %>% 
    select(date, starts_with("facebook")) %>% 
    `colnames<-`(colnames)
    
    
campaign_tbl <- bind_rows(adwords_tbl, facebook_tbl) %>% 
    mutate(date = ymd(date))

campaign_tbl %>% glimpse()


# * Key Question ----

#' - 1. Which PPC source has the highest/lowest CTR?
#' - 2. Which PPC source has the highest conversions of clicks to leads?
#' - 3. Why might FB have a higher conversion rate?
#' - 4. Based on this information, is one PPC ad type better than the other?


# * PPC Analysis Total ----

# ** Cost Table ----
cost_tbl <- ppc_tbl %>% 
    select(ends_with("cost")) %>% 
    rowwise() %>%
    mutate(total = sum(c_across(everything()))) %>% 
    gather() %>% 
    summarise(total_cost = sum(value), .by = key) %>% 
    mutate(key = key %>% str_remove_all("_cost"))

# ** Impressions Table ----
impressions_tbl <- ppc_tbl %>% 
    select(ends_with("impressions")) %>% 
    rowwise() %>%
    mutate(total = sum(c_across(everything()))) %>% 
    gather() %>% 
    summarise(total_impressions = sum(value), .by = key) %>% 
    mutate(key = key %>% str_remove_all("_impressions"))

# ** Clicks Table ----
clicks_tbl <- ppc_tbl %>% 
    select(ends_with("clicks")) %>% 
    rowwise() %>%
    mutate(total = sum(c_across(everything()))) %>% 
    gather() %>% 
    summarise(total_clicks = sum(value), .by = key) %>% 
    mutate(key = key %>% str_remove_all("_clicks"))

# ** Leads Table ----
leads_tbl <- ppc_tbl %>% 
    select(ends_with("conversions")) %>% 
    rowwise() %>%
    mutate(total = sum(c_across(everything()))) %>% 
    gather() %>% 
    summarise(total_leads = sum(value), .by = key) %>% 
    mutate(key = key %>% str_remove_all("_conversions"))

# ** PPC Table ----
cost_tbl %>% left_join(impressions_tbl) %>% 
    left_join(clicks_tbl) %>% 
    left_join(leads_tbl) %>% 
    mutate(
        ctr      = total_clicks / total_impressions,
        cpc      = total_cost / total_clicks,
        lead_cvr = total_leads / total_clicks
    )

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


# * PPC Analysis by Campaign ----

# ** Function ----
get_ad_metrics <- function(data, campaign, metric) {
    
    # campaign setup
    if (campaign == "ad_words")  campaign_name = "ad_words_campaign_id"
    if (campaign == "facebook")  campaign_name = "facebook_campaign_id"
    
    # group_cols setup
    # if (campaign == "ad_words")  group_cols = c("ad_words_campaign_id")
    # if (campaign == "facebook")  group_cols = c("facebook_campaign_id")
    
    # rlang setup
    campaign_name <- rlang::sym(campaign_name)
    #group_cols    <- rlang::enquo(group_cols)
    
    # calculation
    ret_tbl <- data %>% 
        select(!!campaign_name, matches(paste0("^", campaign, ".*", metric, "$"))) %>% 
        #rowwise() %>%
        #mutate(total = sum(c_across(ends_with(metric)))) %>% 
        #pivot_longer(cols = -!!campaign_name, names_to = "name", values_to = "value") %>% 
        #summarise(!!paste0("total_", metric):= sum(value), .by = !!group_cols) %>% 
        #mutate(name = name %>% str_remove_all(paste0("_", metric))) %>% 
        group_by(!!campaign_name) %>% 
        summarise(!!paste0("total_", metric):= sum(c_across(where(is.numeric)))) %>% 
        ungroup()
    
    return(ret_tbl)
}


get_ad_metrics_by_ad <- function(campaign) {
    
    if (campaign == "adwords") campaign = "ad_words"
    
    cost_tbl   <- get_ad_metrics(ppc_tbl, campaign = campaign, "cost")
    impr_tbl   <- get_ad_metrics(ppc_tbl, campaign = campaign, "impressions")
    clicks_tbl <- get_ad_metrics(ppc_tbl, campaign = campaign, "clicks")
    leads_tbl  <- get_ad_metrics(ppc_tbl, campaign = campaign, "conversions")
    
    metrics_tbl <- cost_tbl %>% 
        left_join(impr_tbl) %>% 
        left_join(clicks_tbl) %>% 
        left_join(leads_tbl) %>%
        mutate(
            ctr = total_clicks / total_impressions,
            cpc = total_cost / total_clicks,
            cvr = total_conversions / total_clicks
        )
    
    return(metrics_tbl)
}


# ** Stats by Ad ----
aw_metrics_by_ad_tbl <- get_ad_metrics_by_ad("ad_words")

fb_metrics_by_ad_tbl <- get_ad_metrics_by_ad("facebook")





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
