# CLV ANALYSIS SCRIPT ----
# **** ----

# *****************************************************************************
# 1.0 SETUP ----
# *****************************************************************************

# 1.1 Set Working Dir ----
setwd(here::here("04_global_electronics_retailer_analysis", "R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(ggthemes)
library(gt)
library(gtExtras)
library(webshot2)
library(timetk)
library(tidymodels)
library(xgboost)

# 1.2 Source ----
source("../functions/utils_gt_table.R")
source("../functions/utils_data_wrangling.R")
source("../functions/utils_plotting.R")


# *****************************************************************************
# **** ----
# 2.0 DATA IMPORT ----
# *****************************************************************************

# 2.1 Load USA Online Data ----
usa_online_sales_tbl <- read_csv("../data/clean_data/usa_online_sales_tbl.csv")

# 2.2 Filter Analysis Cohort Data ----
clv_analysis_cohort_tbl <- usa_online_sales_tbl %>% 
    filter(year(first_purchase_date) == 2016) 

clv_analysis_cohort_tbl %>% distinct(customer_key) %>% nrow()


# *****************************************************************************
# **** ----
# 3.0 DATA PREP FOR CLV ----
# *****************************************************************************

# 3.1 Analysis Cohort Data Aggregated ----
monthly_aggregated_tbl <- clv_analysis_cohort_tbl %>% 
    summarise(
        quantity  = sum(quantity, na.rm = TRUE),
        sales_usd = sum(sales_usd, na.rm = TRUE),
        .by       = c(order_date, customer_key)
    )

# 3.2 Analysis Cohort Customer List ----
customer_keys <- unique(monthly_aggregated_tbl$customer_key)

# 3.3 Make Date Sequence ----
date_sequence_tbl <- tk_make_timeseries(
    start = min(monthly_aggregated_tbl$order_date),
    end   = max(monthly_aggregated_tbl$order_date),
    by    = "month"
) %>% 
    as_tibble() %>% 
    rename(order_date = value)

# 3.4 Analysis Timeframe Monthly Tibble ----
temp_daily_tbl <- customer_keys %>% 
    map(function(customer_key) {
        tibble(
            date_sequence_tbl,
            customer_key = customer_key
        )
    }) %>% 
    bind_rows()

# 3.5 Join Temp Data Table To Monthly Sales Data ----
clv_analysis_full_tbl <- temp_daily_tbl %>% 
    left_join(
        monthly_aggregated_tbl, 
        by = c("order_date", "customer_key")
    ) %>% 
    mutate(quantity = ifelse(is.na(quantity), 0, quantity)) %>%
    mutate(sales_usd = ifelse(is.na(sales_usd), 0, sales_usd))



# *****************************************************************************
# **** ----
# 4.0 DATA SPLITTING ----
# *****************************************************************************
clv_analysis_full_tbl <- clv_analysis_cohort_tbl

# 4.1 Data Splitting 1 ----
set.seed(100)
ids_train <- clv_analysis_full_tbl %>% 
    pull(customer_key) %>%
    unique() %>% 
    sample(size = round(0.7 * length(.))) %>% 
    sort()

split_1_train_tbl <- clv_analysis_full_tbl %>% 
    filter(customer_key %in% ids_train)

unique(split_1_train_tbl$customer_key) %>% length()

split_1_test_tbl <- clv_analysis_full_tbl %>%
    filter(!customer_key %in% ids_train)

unique(split_1_test_tbl$customer_key) %>% length()


# 4.2 Data Splitting 2 ----
splits_2_train <- timetk::time_series_split(
    data       = split_1_train_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

testing(splits_2_train) %>% distinct(order_date)

splits_2_test <- timetk::time_series_split(
    data       = split_1_test_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

splits_2_test %>%
    timetk::tk_time_series_cv_plan() %>%
    timetk::plot_time_series_cv_plan(order_date, sales_usd, .interactive = FALSE)


# *****************************************************************************
# **** ----
# 4.0 FEATURE ENGINEERING ----
# *****************************************************************************

# 4.1 Make In-Sample Targets from Train Data ----
targets_train_tbl <- testing(splits_2_train) %>% 
    summarise(
        spend_90_total = sum(sales_usd, na.rm = TRUE),
        spend_90_flag  = 1,
        .by            = customer_key
    )

targets_train_tbl %>% View()

# * Make Out-Sample Targets from Test Data ----
targets_test_tbl <- testing(splits_2_test) %>% 
    summarise(
        spend_90_total = sum(sales_usd, na.rm = TRUE),
        spend_90_flag  = 1,
        .by            = customer_key
    )

testing(splits_2_test) %>% pull(order_date) %>% range()

testing(splits_2_test) %>% 
    filter(customer_key %in% unique(targets_test_tbl$customer_key)) %>%
    View()

clv_analysis_cohort_tbl %>% 
    filter(order_date >= as.Date("2020-01-01")) %>%
    View()

# * Additional Customer Features ----
top_10_states <- clv_analysis_full_tbl %>% 
    get_top_n_sales_by_category(state, head = 10) %>% 
    pull(state)

clv_customer_features_tbl <- clv_analysis_full_tbl %>% 
    select(customer_key, gender, state) %>% 
    distinct() %>% 
    drop_na() %>%
    mutate(state = ifelse(state %in% top_10_states, state, "Other"))

# * Make Training Data ----
max_date_train <- max(training(splits_2_train)$order_date)

train_tbl <- training(splits_2_train) %>% 
    summarise(
        recency = (max(order_date) - max_date_train) / ddays(1),
        frequency = n_distinct(order_date),
        sales_sum = sum(sales_usd, na.rm = TRUE),
        sales_avg = mean(sales_usd, na.rm = TRUE),
        .by       = customer_key
    ) %>% 
    left_join(targets_train_tbl, by = "customer_key") %>%
    replace_na(list(spend_90_total = 0, spend_90_flag = 0)) %>% 
    mutate(spend_90_flag = as.factor(spend_90_flag))

train_tbl %>% View()

# * Make Testing Data ----
test_tbl <- testing(splits_2_test) %>% 
    summarise(
        recency = (max(order_date) - max_date_train) / ddays(1),
        frequency = n_distinct(order_date),
        sales_sum = sum(sales_usd, na.rm = TRUE),
        sales_avg = mean(sales_usd, na.rm = TRUE),
        .by       = customer_key
    ) %>% 
    left_join(targets_test_tbl, by = "customer_key") %>%
    replace_na(list(spend_90_total = 0, spend_90_flag = 0)) %>% 
    mutate(spend_90_flag = as.factor(spend_90_flag))

train_tbl %>% View()

# Recipes ----

# * Recipe for Spend Total ----
recipe_spend_total <- recipe(spend_90_total ~ ., data = train_tbl) %>% 
    step_rm(spend_90_flag, customer_key)

# * Recipe for Spend Flag ----
recipe_spend_flag <- recipe(spend_90_flag ~ ., data = train_tbl) %>% 
    step_rm(spend_90_total, customer_key)


# MODELS ----

# * Spend Total Model ----
wflw_spend_total_xgb <- workflow() %>% 
    add_model(
        boost_tree(
            mode = "regression"
        ) %>% 
            set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spend_total) %>%
    fit(data = train_tbl)

# * Spend Flag Model ----
wflw_spend_flag_xgb <- workflow() %>% 
    add_model(
        boost_tree(
            mode = "classification"
        ) %>% 
            set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spend_flag) %>%
    fit(data = train_tbl)

# PREDICTIONS ----

# * Predictions Table ----
predictions_test_tbl <- bind_cols(
    
    predict(wflw_spend_total_xgb, test_tbl) %>% 
        rename(.pred_total = .pred),
    
    predict(wflw_spend_flag_xgb, test_tbl, type = "prob") %>%
        select(.pred_1) %>% 
        rename(.pred_prob = .pred_1)
) %>% 
    bind_cols(test_tbl) %>% 
    select(starts_with(".pred"), starts_with("spend_"), everything())

# * Test Accuracy Spend Total ----
predictions_test_tbl %>% 
    mae(truth = spend_90_total, estimate = .pred_total) 

# * Test Accuracy Spend Flag ----
predictions_test_tbl %>% 
    yardstick::roc_auc(spend_90_flag, .pred_prob, event_level = "second")


splits <- m750 %>%
    time_series_split(assess = "3 months", cumulative = TRUE)

testing(splits)

training(splits)

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
