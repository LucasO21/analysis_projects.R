#  STARBUCKS BEVERAGE ANALYSIS PART 1: MODELING WITH WORKFLOWSETS

# 1.0 INTRODUCTION ----
# The goal of this project is to predict calorie count of Starbucks beverages using Tidymodels & Workflowsets


# 2.0 SETUP ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(readr)
library(tidymodels)
library(workflowsets)
library(rules)
library(Cubist)
library(xgboost)
library(finetune)
library(tictoc)


# * 2.1 Load Data ----
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv"

starbucks_raw_tbl <- read_csv(url) %>% 
    as_tibble() %>% 
    clean_names()

starbucks_raw_tbl

# * 2.2 Clean Data ----
starbucks_clean_tbl <- starbucks_raw_tbl %>% 
    mutate(whip = ifelse(whip == 0, "no", "no")) %>% 
    mutate(milk = case_when(
        milk == 0 ~ "none",
        milk == 1 ~ "non-fat",
        milk == 2 ~ "2% milk",
        milk == 3 ~ "soy milk",
        milk == 4 ~ "coconut milk",
        milk == 5 ~ "whole milk"
    )) %>% 
    mutate(trans_fat_g = as.numeric(trans_fat_g),
           fiber_g = as.numeric(fiber_g)) %>% 
    mutate_if(is.character, as.factor)

starbucks_clean_tbl %>% glimpse()

# * 2.3 Data Filtering ----
starbucks_clean_filtered_tbl <- starbucks_clean_tbl %>% 
    filter(size %in% c("short", "tall", "grande", "venti", "trenta"))

starbucks_clean_filtered_tbl %>% distinct(size)

# 3.0 EXPLORATORY DATA ANALYSIS ----


# 4.0 FEATURE ENGINEERING ----

# * 4.1 Generate New Features ----
starbucks_feat_eng_tbl <- starbucks_clean_filtered_tbl %>% 
    mutate(product_name = product_name %>% str_to_lower() %>% as.factor) %>% 
    mutate(
        is_white_hot_choc = str_detect(product_name, "white hot chocolate") %>% as.numeric(),
        is_frappuccino = str_detect(product_name, "frappuccino") %>% as.numeric(),
        is_tea = str_detect(product_name, "tea") %>% as.numeric(),
        is_coffee = str_detect(product_name, "coffee") %>% as.numeric(),
        is_iced = str_detect(product_name, "iced") %>% as.numeric()
    )

starbucks_feat_eng_tbl %>% glimpse()

starbucks_feat_eng_tbl %>% count(is_coffee)

# 5.0 MODELING ----

# * 5.1 Data Splits ----
set.seed(123)
split_obj <- initial_split(starbucks_feat_eng_tbl, prop = 0.80)

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# * 5.2 Cross Validation/Resamples Specs ----
set.seed(123)
resamples_obj <- vfold_cv(starbucks_feat_eng_tbl, v = 5)

# * 5.3 Recipe/Preprocessing Specs ----

# ** Normalized Recipe ----
normalized_recipe <- recipe(formula = calories ~ ., data = train_tbl) %>% 
    step_rm(product_name, serv_size_m_l) %>% 
    step_novel(all_nominal(), -all_outcomes()) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>% 
    step_zv(all_predictors()) %>% 
    step_corr(ends_with("g"), threshold = 0.9) %>% 
    step_normalize(ends_with("g")) 

# ** Tree Recipe ----
tree_recipe <- recipe(formula = calories ~ ., data = train_tbl) %>% 
    step_rm(product_name, serv_size_m_l) %>% 
    step_zv(all_predictors())

# ** Xgboost Recipe ----
xgboost_recipe <- recipe(formula = calories ~ ., data = train_tbl) %>% 
    step_rm(product_name, serv_size_m_l) %>% 
    step_novel(all_nominal(), -all_outcomes()) %>% 
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
    step_zv(all_predictors()) 

normalized_recipe %>% prep() %>% juice() %>% glimpse()

# * 5.4 Model Specs ----

# ** Glmnet ----
glmnet_spec <- linear_reg(
    penalty = tune(), 
    mixture = tune()
) %>% 
    set_mode("regression") %>% 
    set_engine("glmnet") 

# ** KNN ----
kknn_spec <- nearest_neighbor(
    neighbors   = tune(), 
    weight_func = tune()
) %>% 
    set_mode("regression") %>% 
    set_engine("kknn") 

# ** SVM ----
svm_r_spec <- svm_rbf(
    cost = tune(),
    rbf_sigma = tune(),
) %>% 
    set_mode("regression") %>% 
    set_engine("kernlab")

# ** Cubist ----
cubist_spec <- cubist_rules(
    committees = tune(), 
    neighbors  = tune()
) %>% 
    set_engine("Cubist") 

# ** Random Forest ----
ranger_spec <- rand_forest(
    mtry  = tune(), 
    min_n = tune(), 
    trees = tune()
) %>% 
    set_mode("regression") %>% 
    set_engine("ranger") 

# ** Xgboost ----
xgboost_spec <- boost_tree(
    trees          = tune(), 
    min_n          = tune(), 
    tree_depth     = tune(), 
    learn_rate     = tune(), 
    loss_reduction = tune(), 
    sample_size    = tune()
) %>% 
    set_mode("regression") %>% 
    set_engine("xgboost") 

# * 5.5 Model Parameter Updates ----
workflow() %>% 
    add_model(ranger_spec) %>% 
    add_recipe(tree_recipe) %>% 
    parameters() %>% 
    finalize(train_tbl) %>% 
    pull_dials_object("mtry")

ranger_param <- ranger_spec %>% 
    parameters() %>% 
    update(mtry = mtry(c(1, 20)))

ranger_param %>% pull_dials_object("mtry")

# * 5.6 Workflowsets ----

# ** Normalized Workflowset ----
normalized_wokflowset <- workflow_set(
    preproc = list(normalized = normalized_recipe),
    models  = list(
        glmnet  = glmnet_spec, 
        kknn    = kknn_spec, 
        svm_rbm = svm_r_spec
    )
)

# ** Tree Workflowset ----
tree_workflowset <- workflow_set(
    preproc = list(tree = tree_recipe),
    models  = list(
        cubist        = cubist_spec,
        random_forest = ranger_spec
    ) 
) %>% 
    option_add(
        param_info = ranger_param,
        id         = "tree_random_forest"
    )

tree_workflowset %>% select(option) %>% unlist()

# ** Xgboost Workflowset ----
xgboost_workflowset <- workflow_set(
    preproc = list(xgboost = xgboost_recipe),
    models  = list(xgboost = xgboost_spec)
)

# ** Combined Workflowsets ----
combined_workflowsets <- bind_rows(
    normalized_wokflowset,
    tree_workflowset,
    xgboost_workflowset
) %>% 
    mutate(wflow_id = gsub("(normalized_)|(tree_)|(xgboost_)", "", wflow_id))

# * 5.7 Tuning with Racing Method ----
race_control <- control_race(
    save_pred     = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
)

tic()
race_results <- workflow_map(
    object    = combined_workflowsets,
    fn        = "tune_race_anova",
    seed      = 123,
    resamples = resamples_obj,
    grid      = 10,
    control   = race_control
)
toc()

race_results

# 5.8 Evaluate Tuning Results ----
autoplot(
    object      = race_results,
    rank_metric = "rmse",
    metrc       = "rmse",
    select_best = TRUE
)+
    theme_bw()

# 6.0 FINALIZE MODEL ----

# * 6.1 Select Top 2 Models and Update Best Params

# ** Cubist
best_cubist_params <- race_results %>% 
    pull_workflow_set_result("cubist") %>% 
    select_best("rmse")

# ** Xgboost
best_xgboost_params <- race_results %>% 
    pull_workflow_set_result("xgboost") %>% 
    select_best("rmse")
    

# * 6.2 Fit the Final Model ----

# ** Cubist ----
cubist_final_fit <- race_results %>% 
    pull_workflow("cubist") %>% 
    finalize_workflow(best_cubist_params) %>% 
    last_fit(split_obj)

# ** Cubist ----
xgboost_final_fit <- race_results %>% 
    pull_workflow("xgboost") %>% 
    finalize_workflow(best_xgboost_params) %>% 
    last_fit(split_obj)

# * 6.3 Test Metrics

# ** Cubist ----
cubist_final_fit %>% collect_metrics()

cubist_final_fit %>% 
    collect_predictions() %>% 
    ggplot(aes(calories, .pred))+
    geom_abline(col = "green", lty = 2)+
    geom_point(alpha = 0.5)+
    coord_obs_pred()+
    theme_bw()+
    labs(title = "Cubist Model Test Set Fit", x = "observed", y = "predicted")

# ** Xgboost ----
xgboost_final_fit %>% collect_metrics()

xgboost_final_fit %>% 
    collect_predictions() %>% 
    ggplot(aes(calories, .pred))+
    geom_abline(col = "green", lty = 2)+
    geom_point(alpha = 0.5)+
    coord_obs_pred()+
    theme_bw()+
    labs(title = "Xgboost Model Test Set Fit", x = "observed", y = "predicted")

race_results %>% 
    pull_workflow("cubist") %>% 
    extract_preprocessor()



# 7.0 SAVING ARTIFACTS

artifacts_list <- list(
    # data
    data = list(
        feature_engineered_tbl = starbucks_feat_eng_tbl,
        train_tbl = train_tbl,
        test_tbl = test_tbl
    ),
    
    # race results
    race_results = list(race_results = race_results)
    

)

artifacts_list %>% saveRDS("../Artifacts/artifacts_list.rds")


    


