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
        scale_fill_brewer(palette = "Blues", name = "", direction = -1)+
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
bind_cols(
    mmm_tbl %>% 
        get_adstock("you_tube_imp", 0.9)
    get_adstock("tv_grp", 0.5) %>% 
        get_adstock()
)


media_tbl <- mmm_tbl %>% 
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

# * Final Transformed Data ----

# - Take features from the raw dataset (excluding media features)
# - Combine with transformed features (adstock and diminishing returns)

transformed_tbl <- bind_cols(
    mmm_tbl %>% select(-c(meta_imp, you_tube_imp, influencers_views, tv_grp)),
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

# * Function ----
get_correlation <- function(data, target) {
    
    df <- data %>% 
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

# * Correlation Analysis ----

get_correlation(
    data   = transformed_tbl %>% select(accounts_subscriptions, starts_with("influencers")), 
    target = "accounts_subscriptions"
)


# *****************************************************************************
# REGRESSION 2: TRANSFORMED DATA ----
# *****************************************************************************

# * Function ----
get_linear_reg <- function(data, output = "summary") {
    
    # lm fit
    lm_fit <- lm(
        formula = accounts_subscriptions ~ .,
        data    = data 
    )
    
    # lm tidy
    lm_tidy <- broom::tidy(lm_fit) %>% 
        mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no"))
    
    # metrics
    lm_params <- broom::glance(lm_fit)
    
    # predictions
    pred_tbl <- mmm_tbl %>% 
        select(accounts_subscriptions) %>% 
        bind_cols(
            predict(lin_reg, new_data = mmm_tbl %>% select(accounts_subscriptions)) %>% 
                as_tibble() %>% 
                `colnames<-`(".pred")
        )
    
    # final metrics
    lm_params <- pred_tbl %>% 
        metrics(truth = accounts_subscriptions, estimate = .pred) %>% 
        select(-.estimator) %>% 
        pivot_wider(names_from = .metric, values_from = .estimate) %>% 
        bind_cols(lm_params)
    
    # output selection
    if (output == "summary") {
        ret <-  lm_tidy
    } else if (output == "params") {
        ret <- lm_params
    } 
    
    # return
    return(ret)
}

# * Regression ----

get_linear_reg(data = transformed_tbl %>% select(-date))

get_linear_reg(data = transformed_tbl %>% select(-date), output = "params")


# * Stat Sig Features Only ----
stat_sig_feature_list <- get_linear_reg(data = transformed_tbl %>% select(-date)) %>% 
    filter(stat_sig == "yes") %>% 
    pull(term)

get_linear_reg(
    data = transformed_tbl %>% 
        select(
            accounts_subscriptions,
            matches(paste(stat_sig_feature_list, collapse = "|"))
        )
) %>% View()

get_linear_reg(
    data = transformed_tbl %>% 
        select(
            accounts_subscriptions,
            matches(paste(stat_sig_feature_list, collapse = "|"))
        ),
    output = "params"
) 


# *****************************************************************************
# **** ----
# OPTIMIZATION ----
# *****************************************************************************

# - This section uses a for loop to find the best lambda and alpha values
# - Best = best values that maximize the r-squared

# * Lambda Values List ----
lambda_values <- seq(0.1, 0.3, by = 0.1)

# * Alpha Values List ----
alpha_values  <- seq(0.1, 0.3, by = 0.1)

# * Create Empty Results Tibble ----
results_tbl <- tibble(
    column   = character(), 
    lambda   = numeric(), 
    alpha    = numeric(), 
    rsquared = numeric()
)

columns <- c("tv_grp", "you_tube_imp", "meta_imp")

# * Regression ----
# - Iterate Over Lambda & Alpha Values

for (col in columns) {
    for (i in lambda_values) {
        for (j in alpha_values) {
            
            # # Data transformation
            # trans_tbl <- mmm_tbl %>% 
            #     bind_cols(
            #         get_adstock(data = mmm_tbl, var = "tv_grp", lambda = i, TRUE),
            #         get_adstock(data = mmm_tbl, var = "you_tube_imp", lambda = i, TRUE),    
            #         get_adstock(data = mmm_tbl, var = "meta_imp", lambda = i, TRUE)  
            #         # get_adstock(data = mmm_tbl, var = "influencers_views", lambda = i, TRUE),  
            #         # get_adstock(data = mmm_tbl, var = "google_display_imp", lambda = i, TRUE),      
            #         # get_adstock(data = mmm_tbl, var = "google_generic_paid_search_imp", lambda = i, TRUE)    
            #     ) %>% 
            #     select(-c(tv_grp, you_tube_imp, meta_imp)) %>%
            #     mutate(tv_grp_trans = tv_grp_trans ^ j) %>% 
            #     mutate(you_tube_imp_trans = you_tube_imp_trans ^ j) %>% 
            #     mutate(meta_imp_trans = meta_imp_trans ^ j) 
            # mutate(influencers_views_trans = influencers_views_trans ^ j) %>% 
            # mutate(google_display_imp_trans = google_display_imp_trans ^ j) %>% 
            # mutate(google_generic_paid_search_imp_trans = google_generic_paid_search_imp_trans ^ j) 
            
            trans_tbl <- mmm_tbl %>% 
                bind_cols(
                    get_adstock(data = mmm_tbl, var = {{col}}, lambda = i, TRUE)
                ) %>% 
                select(-{{col}}) %>% 
                mutate(across(ends_with("_trans"), ~ . ^ j))
            
            lm_fit   <- lm(accounts_subscriptions ~ ., data = trans_tbl %>% select(-date))
            rsquared <- summary(lm_fit)$r.squared
            
            # Append the lambda and R-squared values to the results tibble
            results_tbl <- results_tbl %>%
                add_row(column = col, lambda = i, alpha = j, rsquared = rsquared) %>% 
                arrange(desc(rsquared))
            
        }
    }
    
    
}


lapply(lambda_values, function(x) get_adstock(data = mmm_tbl, var = "tv_grp", lambda = x) ^ j) %>% 
    bind_rows() %>% 
    as_tibble()
  



# Top 10 R - Squared Values ----
results_tbl %>% head(5)


# *****************************************************************************
# **** ----
# REGRESSION: OPTIMIZED ALPHA & LAMBDA VALUES ----
# *****************************************************************************

# * Data Prep ----
best_lambda <- pull(results_tbl[1,][1])
best_alpha  <- pull(results_tbl[1,][2])

mmm_final_tbl <- mmm_tbl %>% 
    bind_cols(
        get_adstock(data = mmm_tbl, var = "tv_grp", lambda = best_lambda, TRUE),
        get_adstock(data = mmm_tbl, var = "you_tube_imp", lambda = best_lambda, TRUE),    
        get_adstock(data = mmm_tbl, var = "meta_imp", lambda = best_lambda, TRUE),    
        get_adstock(data = mmm_tbl, var = "influencers_views", lambda = best_lambda, TRUE),  
        get_adstock(data = mmm_tbl, var = "google_display_imp", lambda = best_lambda, TRUE),      
        get_adstock(data = mmm_tbl, var = "google_generic_paid_search_imp", lambda = best_lambda, TRUE)    
    ) %>% 
    select(-c(tv_grp, you_tube_imp, meta_imp, influencers_views,
              google_display_imp, google_generic_paid_search_imp)) %>%
    mutate(tv_grp_trans = tv_grp_trans ^ best_alpha) %>% 
    mutate(you_tube_imp_trans = you_tube_imp_trans ^ best_alpha) %>% 
    mutate(meta_imp_trans = meta_imp_trans ^ best_alpha) %>% 
    mutate(influencers_views_trans = influencers_views_trans ^ best_alpha) %>% 
    mutate(google_display_imp_trans = google_display_imp_trans ^ best_alpha) %>% 
    mutate(google_generic_paid_search_imp_trans = google_generic_paid_search_imp_trans ^ best_alpha)


# * Regression ----
lm_fit_final <- lm(accounts_subscriptions ~., data = mmm_final_tbl %>% select(-date))

lm_fit_final_coef_tbl <- broom::tidy(lm_fit_final) %>% 
    mutate(stat_sig = ifelse(p.value < 0.05, "yes", "no"))

lm_fit_final_params_tbl <- broom::glance(lm_fit_final)


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





