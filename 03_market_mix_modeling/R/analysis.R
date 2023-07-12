# SCRIPT TOPIC:
# SCRIPT NOTES:
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("03_market_mix_modeling", "R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

# *****************************************************************************
# DATA IMPORT ----
# *****************************************************************************
data_tbl <- read_xlsx("../data/Project_Data.xlsx") %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(date = as.Date(date))

data_tbl %>% glimpse()

data_tbl %>% View()

data_tbl %>% sapply(function(x) sum(is.na(x)))


# *****************************************************************************
# EXPLORATORY DATA ANALYSIS ----
# *****************************************************************************

# * Correlation Analysis ----
corr_tbl <- data_tbl %>% 
    select(-date) %>% 
    cor() %>% 
    round(digits = 2) %>% 
    reshape2::melt() %>% 
    as_tibble() %>% 
    ggplot(aes(Var1, Var2, fill = value))+
    geom_tile()+
    geom_text(aes(label = value), color = "white", fontface = "bold")+
    scale_fill_gradient(low = "lightblue", high = "darkblue")+
    theme_bw()+
    theme(
        axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1)
    )

# * Account Sub vs TV GRP ----
data_tbl %>% 
    select(date, accounts_subscriptions, tv_grp) %>% 
    pivot_longer(c(accounts_subscriptions, tv_grp)) %>% 
    ggplot(aes(date, value, color = name))+
    geom_line(linewidth = 1)

data_tbl %>% 
    select(date, accounts_subscriptions, tv_grp, google_brand_paid_search_clicks,
           meta_impressions, you_tube_impressions) %>% 
    pivot_longer(!c(date, accounts_subscriptions)) %>% 
    ggplot(aes(date, value, color = name))+
    geom_line(linewidth = 1)+
    facet_wrap(~ name, scales = "free_x")
    

data_tbl %>% 
    ggplot(aes(accounts_subscriptions, tv_grp))+
    geom_point()


# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************
