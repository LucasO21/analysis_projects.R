# MARKET MIX MODELING ----
# MANUAL ANALYSIS SCRIPT ----
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
mmm_tbl <- read_xlsx("../data/Project_Data.xlsx") %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(date = as.Date(date)) %>% 
    setNames(names(.) %>% str_replace_all("impressions", "imp"))

mmm_tbl %>% glimpse()

mmm_tbl %>% View()

mmm_tbl %>% sapply(function(x) sum(is.na(x)))


# *****************************************************************************
# EXPLORATORY DATA ANALYSIS ----
# *****************************************************************************

# * Correlation Analysis ----
corr_tbl <- mmm_tbl %>% 
    select(-date) %>% 
    cor() %>% 
    round(digits = 2) 

corr_tbl %>% 
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

corr_subscriptions <- corr_tbl[, "accounts_subscriptions"]


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

get_line_plot(mmm_tbl, var = "tv_grp")


# * Dual Axis Plot ----
mmm_tbl %>% 
    mutate(meta_imp = meta_imp/1000) %>% 
    ggplot(aes(x = date))+
    geom_line(aes(y = accounts_subscriptions, color = "Subscriptions"), linewidth = 1)+
    geom_line(aes(y = meta_imp, color = "Meta Impressions"), linewidth = 1) +
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


get_line_plot(mmm_tbl, var = "dates_school_holidays")


# *****************************************************************************
# REGRESSION 1: RAW DATA ----
# *****************************************************************************

# * Regression ----
lm_fit <- lm(
    formula = accounts_subscriptions ~ .,
    data    = mmm_tbl %>% select(-date)
)

# * Regression Output ----
lm_summary <- broom::tidy(lm_fit) %>% 
    mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no"))
    
lm_params <- broom::glance(lm_fit)


# *****************************************************************************
# ADSTOCK DATA TRANSFORMATION ----
# *****************************************************************************

# - Adstock (carry over effect of media advertising on the consumer)
# - Diminishing returns (also on media advertising)
# - Using optimal values from udemy course

# * Functions ----
get_adstock <- function(data, var, lambda, default_name = TRUE) {
    
    adstock <- numeric(nrow(data))
    
    for (i in 1:nrow(data)) {
        if (i == 1) {
            adstock[i] = data[[var]][i]
        } else {
            adstock[i] = data[[var]][i] + (1 - lambda) * adstock[i - 1]
        }
    }
    
    if (!default_name) {
        ret <- adstock %>% 
            as_tibble() %>% 
            `colnames<-`(c(paste0({{var}}, "_ad_", lambda)))
    } else {
        ret <- adstock %>% 
            as_tibble() %>% 
            `colnames<-`(c(paste0({{var}}, "_trans")))
    }
    
    
    return(ret)
}


# get_adstock_plot <- function(data, var, legend_position = "bottom",
#                              scale_y = FALSE, scale = 1000, alpha = 0.8) {
#     
#     df <- data %>% 
#         select(date, starts_with({{var}})) %>% 
#         pivot_longer(!date) %>% 
#         mutate(name = fct_reorder(name, value) %>% fct_rev)
#     
#     if (scale_y) {
#         p <- df %>% 
#             ggplot(aes(x = date, y = value/scale, fill = name))
#     } else {
#         p <- df %>% 
#             ggplot(aes(x = date, y = value, fill = name))
#     }
#     
#     p <- p+
#         geom_area(position = "identity", alpha = alpha, color = "grey30", linewidth = 0.3)+
#         scale_fill_brewer(palette = "Blues", name = "", direction = -1)+
#         scale_x_date(date_breaks = "4 months")+
#         theme_bw()+
#         theme(
#             # legend.position = c(0.95, 0.9),
#             # legend.justification = c(1, 1),
#             legend.position = legend_position,
#             legend.key.size = unit(0.5, 'cm')
#         )+
#         guides(fill = guide_legend(reverse = TRUE))
#     
#     return(p)
#     
# }

get_transformed_data <- function(data) {
    
    trans_tbl <- data %>% 
        select(-c(tv_grp, you_tube_imp, meta_imp, influencers_views)) %>% 
        bind_cols(
            get_adstock(data, "you_tube_imp", 0.9),
            get_adstock(data, "tv_grp", 0.5),
            get_adstock(data, "meta_imp", 0.5),
            get_adstock(data, "influencers_views", 0.5)
        ) %>% 
        mutate(you_tube_imp_trans = you_tube_imp_trans ^ 0.3) %>% 
        mutate(meta_imp_trans = meta_imp_trans ^ 0.9) %>% 
        mutate(tv_grp_trans = tv_grp_trans ^ 0.4) %>% 
        mutate(influencers_views_trans = influencers_views_trans ^ 0.4) %>% 
        setNames(names(.) %>% str_remove_all("_trans"))
    
    return(trans_tbl)
}

# * Data Transformation ----
transformed_tbl <- get_transformed_data(mmm_tbl)

transformed_tbl %>% glimpse()


# *****************************************************************************
# **** ----
# REGRESSION 2: TRANSFORMED DATA ----
# *****************************************************************************

lm_fit_trans <- lm(accounts_subscriptions ~ ., data = transformed_tbl)

lm_fit_trans_coef_tbl <- broom::tidy(lm_fit_trans) %>% 
    mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no"))

lm_fit_trans_metrics_tbl <- broom::glance(lm_fit_trans)



# *****************************************************************************
# **** ----
# CONTRIBUTION ----
# *****************************************************************************

# * Data Prep ----

# subscription value
subscription_value <- 35

# model coefficients
roi_tbl <- lm_fit_final_coef_tbl %>% 
    select(term, estimate) %>% 
    filter(!grepl("\\(Intercept\\)|promotion|competitors_promotion|dates_school_holidays", term)) %>% 
    
    # join sum of predictors
    left_join(
        mmm_final_tbl %>% 
            select(-date) %>% 
            gather() %>% 
            group_by(key) %>% 
            summarise(contribution = sum(value)) %>% 
            ungroup(),
        by = c("term" = "key")
    ) %>% 
    
    # calculate revenue
    mutate(revenue = contribution * estimate) %>% 
    
    # spend
    mutate(spend = contribution * subscription_value) %>% 
    
    # ROI
    mutate(roi = revenue / spend)

roi_tbl


# *****************************************************************************
# REPREX ----
# *****************************************************************************

library(tidyverse)

lamda_values <- c(0.2, 0.3, 0.4)
alpha_values <- c(0.3, 0.5, 0.9)

set.seed(123)
data <- tibble(
    sales = runif(5, min = 1000, max = 1500),
    var_1 = runif(5, min = 12, max = 25),
    var_2 = runif(5, min = 75, max = 90),
)

tibble(
    var_1
)

# *****************************************************************************
# SECTION NAME 
# *****************************************************************************
# *****************************************************************************
# SECTION NAME 
# *****************************************************************************





