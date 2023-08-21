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
ppc_tbl <- readxl::read_excel(
    path  = "../data/Marketing+Analytics+Case+Study.xlsm",
    sheet = "PPC Data",
    skip  = 3
) %>% 
    as_tibble %>% 
    clean_names() %>% 
    mutate(date = ymd(date))

ppc_tbl %>% glimpse()


# * Key Question ----

#' - 1. Which PPC source has the highest/lowest CTR?
#' - 2. Which PPC source has the highest conversions of clicks to leads?
#' - 3. Why might FB have a higher conversion rate?
#' - 4. Based on this information, is one PPC ad type better than the other?


# * Total CTR, CVR CVR % ----
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
