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
library(tidyverse)

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

get_line_plot <- function(data, var) {
    df <- data %>% 
        select(date, accounts_subscriptions, rlang::sym(var)) %>% 
        pivot_longer(c(accounts_subscriptions, rlang::sym(var)))
    
    p <-  df %>% 
        ggplot(aes(date, value, color = name))+
        geom_line(linewidth = 1)+
        theme_bw()+
        scale_x_date(date_breaks = "4 months")+
        theme(
            legend.position = "bottom"
        )+
        scale_color_discrete(name = "")
        
    
    return(p)
}

get_scatter_plot <- function(data, var) {
    p <- data %>% 
        ggplot(aes(accounts_subscriptions, rlang::sym(var)))+
        geom_point(size = 1.5)+
        geom_smooth(method = loess, se = FALSE)
    
    return(p)
}

data_tbl %>% 
    ggplot(aes(accounts_subscriptions, tv_grp))+
    geom_point(size = 1.5)+
    geom_smooth(method = loess, se = FALSE)
    
    

get_line_plot(data_tbl, var = "tv_grp")

get_line_plot(data_tbl, var = "meta_impressions")


# Dual Axis Line Plot ----
p1 <- data_tbl %>% 
    mutate(meta_impressions = meta_impressions/1000) %>% 
    ggplot(aes(x = date))+
    geom_line(aes(y = accounts_subscriptions, color = "Subscriptions"), linewidth = 1)+
    geom_line(aes(y = meta_impressions, color = "Meta Impressions"), linewidth = 1) +
    scale_color_manual(
        values = c("blue", "red"),
        labels = c("Subscriptions", "Meta Impressions"),
        name = ""
    )+
    scale_y_continuous(
        name = "Subscriptions",
        sec.axis = sec_axis(
            ~ .,
            name = "Meta Impressions",
            breaks = seq(0, 10000, 2500),
            labels = scales::comma
        )
    )+
    theme_bw()+
    theme(
        legend.position = c(0.95, 0.9),
        legend.justification = c(1, 1)
    )


get_line_plot(data_tbl, var = "dates_school_holidays")

p2 <- data_tbl %>% 
    ggplot(aes(accounts_subscriptions, meta_impressions))+
    geom_point(size = 1.5)+
    geom_smooth(method = loess)

gt <- arrangeGrob(p1, p2)

p1/p2



# *****************************************************************************
# REGRESSION (RAW DATA) ----
# *****************************************************************************

# Regression ----
lm_fit <- lm(
    formula = accounts_subscriptions ~ .,
    data    = data_tbl %>% select(-date)
)

lm_summary <- broom::tidy(lm_fit) %>% 
    mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no"))
    
lm_params <- broom::glance(lm_fit)



# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************
