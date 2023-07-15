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
    mutate(date = as.Date(date)) %>% 
    setNames(names(.) %>% str_replace_all("impressions", "imp"))

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


tibble(feature = names(corr_subscriptions), correlation = corr_subscriptions) %>% 
    filter(feature != "accounts_subscriptions") %>% 
    arrange(desc(correlation))


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

# * Regression ----
lm_fit <- lm(
    formula = accounts_subscriptions ~ .,
    data    = data_tbl %>% select(-date)
)

# * Regression Output ----
lm_summary <- broom::tidy(lm_fit) %>% 
    mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no"))
    
lm_params <- broom::glance(lm_fit)


# *****************************************************************************
# ADSTOCK DATA TRANSFORMATION ----
# *****************************************************************************

# - Adstock (carry over effect of media advertising on the consumer)

# * Functions ----
get_adstock <- function(data, var, lambda) {
    
    adstock <- numeric(nrow(data))
    
    for (i in 1:nrow(data)) {
        if (i == 1) {
            adstock[i] = data[[var]][i]
        } else {
            adstock[i] = data[[var]][i] + (1 - lambda) * adstock[i - 1]
        }
    }
    
    ret <- adstock %>% 
        as_tibble() %>% 
        `colnames<-`(c(paste0({{var}}, "_ad_", lambda)))
    
    return(ret)
}


get_adstock_plot <- function(data, var, legend_position = "bottom",
                             scale_y = FALSE, scale = 1000, alpha = 0.8) {
    
    df <- data %>% 
        select(date, starts_with({{var}})) %>% 
        pivot_longer(!date) %>% 
        mutate(name = fct_reorder(name, value) %>% fct_rev)
    
    if (scale_y) {
        p <- df %>% 
            ggplot(aes(x = date, y = value/scale, fill = name))
    } else {
        p <- df %>% 
            ggplot(aes(x = date, y = value, fill = name))
    }
    
    p <- p+
        geom_area(position = "identity", alpha = alpha, color = "grey30", linewidth = 0.3)+
        scale_fill_brewer(palette = "BuPu", name = "", direction = -1)+
        scale_x_date(date_breaks = "4 months")+
        theme_bw()+
        theme(
            # legend.position = c(0.95, 0.9),
            # legend.justification = c(1, 1),
            legend.position = legend_position,
            legend.key.size = unit(0.5, 'cm')
        )+
        guides(fill = guide_legend(reverse = TRUE))
    
    return(p)
    
}

# * Data Transformation ----

# ** Media Tibble ----
media_tbl <- data_tbl %>% 
    select(date, tv_grp, you_tube_imp, meta_imp, influencers_views)

# ** Lambda Values ----
adstock_lambda_values <- c(0.5, 0.7, 0.9)

# ** Media Transformed Tibble ----
media_adstock_tbl <- bind_cols(
    media_tbl,
    bind_cols(map(adstock_lambda_values, ~ get_adstock(media_tbl, "tv_grp", .x))),
    bind_cols(map(adstock_lambda_values, ~ get_adstock(media_tbl, "you_tube_imp", .x))),
    bind_cols(map(adstock_lambda_values, ~ get_adstock(media_tbl, "meta_imp", .x))),
    bind_cols(map(adstock_lambda_values, ~ get_adstock(media_tbl, "influencers_views", .x)))
) 

# media_adstock_tbl %>% View()
media_adstock_tbl %>% glimpse()
    

# * Visualization ----
get_adstock_plot(media_adstock_tbl, "tv_grp")

get_adstock_plot(media_adstock_tbl, "you_tube_imp", scale_y = TRUE, scale = 1000)

get_adstock_plot(media_adstock_tbl, "meta_imp", scale_y = TRUE, scale = 1000000)

get_adstock_plot(media_adstock_tbl, "influencers_views", scale_y = FALSE)


# *****************************************************************************
# DIMINISHING RETURNS TRANSFORMATION ----
# *****************************************************************************

# - Applied on the adstock data

# * Functions ----
get_saturation <- function(data, var, alpha) {
    
    df <- data[[var]] ^ alpha
    
    ret <- df %>% 
        as_tibble() %>% 
        `colnames<-`(c(paste0({{var}}, "_dr_", alpha)))
    
    return(ret)
}


# * Alpha Values ----
alpha_values <- c(0.3, 0.4, 0.5)

# ** Youtube Decay Data ----
youtube_saturation_tbl <- bind_cols(
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "you_tube_imp_ad_0.5", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "you_tube_imp_ad_0.7", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "you_tube_imp_ad_0.9", .x)))
) 

# youtube_saturation_tbl %>% View()
youtube_saturation_tbl %>% glimpse()

# ** TV GRP Decay Data ----
tv_grp_saturation_tbl <- bind_cols(
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "tv_grp_ad_0.5", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "tv_grp_ad_0.7", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "tv_grp_ad_0.9", .x)))
)

#tv_grp_saturation_tbl %>% View()

# ** Meta Decay Data ----
meta_saturation_tbl <- bind_cols(
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "meta_imp_ad_0.5", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "meta_imp_ad_0.7", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "meta_imp_ad_0.9", .x)))
)

# meta_decay_tbl %>% View()

# ** Influencers Decay Data ----
influencers_saturation_tbl <- bind_cols(
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "influencers_views_ad_0.5", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "influencers_views_ad_0.7", .x))),
    bind_cols(map(alpha_values, ~ get_saturation(media_adstock_tbl, "influencers_views_ad_0.9", .x)))
)

# influencers_decay_tbl %>% View()

# ** Final Decay Data ----
transformed_tbl <- bind_cols(
    data_tbl %>% select(-c(meta_imp, you_tube_imp, influencers_views, tv_grp)),
    youtube_saturation_tbl,
    tv_grp_saturation_tbl,
    meta_saturation_tbl,
    influencers_saturation_tbl
)

transformed_tbl %>% glimpse()

# transformed_tbl %>% View()


# * Visualization (Response Curves) ----
# youtube_saturation_tbl %>% 
#     select(you_tube_imp_ad_0.9_dr_0.3, you_tube_imp_ad_0.9_dr_0.4, you_tube_imp_ad_0.9_dr_0.5) %>% 
#     bind_cols(media_adstock_tbl %>% select(you_tube_imp_ad_0.9)) %>% 
#     
#     ggplot(aes(x = you_tube_imp_ad_0.9))+
#     geom_point(aes(y = you_tube_imp_ad_0.9_dr_0.3, color = "Adstock/DR (90%/30%)"), alpha = 0.5)+
#     geom_point(aes(y = you_tube_imp_ad_0.9_dr_0.4, color = "Adstock/DR (90%/40%)"), alpha = 0.5)+
#     geom_point(aes(y = you_tube_imp_ad_0.9_dr_0.5, color = "Adstock/DR (90%/50%)"), alpha = 0.5)+
#     scale_color_manual(
#         values = c("blue", "red", "orange"),
#         labels = c("Adstock/DR (90%/30%)", "Adstock/DR (90%/40%)", "Adstock/DR (90%/50%)"),
#         name = ""
#     )+
#     theme(legend.position = "bottom")


# *****************************************************************************
# CORRELATION ANALYSIS (TRANSFORMED FEATURES) ----
# *****************************************************************************

# * Correlation Analysis ----
get_correlation <- function(data, target) {
    
    df <- data %>% 
        select(-date) %>% 
        cor() %>% 
        round(digits = 2) 
    
    corr_target <- df[, target]
    
    ret <- tibble(
        feature = names(corr_target), correlation = corr_target
    ) %>% 
        filter(feature != target) %>% 
        arrange(desc(correlation)) 
    
    return(ret)
}

corr_transformed_tbl <- get_correlation(
    data   = transformed_tbl, 
    target = "accounts_subscriptions"
)

corr_transformed_tbl %>% 
    filter(str_detect(feature, "you_tube"))





# * Regression ----
lm_fit <- lm(
    formula = accounts_subscriptions ~ .,
    data    = transformed_tbl %>% select(-date)
)

# * Regression Output ----
lm_summary <- broom::tidy(lm_fit) %>% 
    mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no")) %>% 
    View()

lm_params <- broom::glance(lm_fit)


# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# SECTION NAME ----
# *****************************************************************************





