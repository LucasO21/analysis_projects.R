# CUSTOMER SEGMENTATION (BANKING):
# ANALYSIS SCRIPT:
# *** ----


# *************************************************************************
# 1.0 SETUP ----
# *************************************************************************

# 1.1 Set Working Dir ----
setwd(here::here("customer_segmentation_banking", "R"))

# 1.2 Libraries ----
library(tidyverse)
library(janitor)
library(tidymodels)
library(tidyquant)


# *************************************************************************
# 2.0 DATA IMPORT ----
# *************************************************************************
customer_tbl <- read_csv("../data/Marketing_data.csv") %>% 
    clean_names() %>% 
    as_tibble()

customer_tbl %>% glimpse()


# *************************************************************************
# 3.0 EDA ----
# *************************************************************************

# 3.1 Data Inspection ----
customer_tbl %>% sapply(function(x) sum(is.na(x)))

# 3.2 Skimr ----
skimr::skim(customer_tbl)


# 3.3 Distributions ----

# 3.3.1 Histograms of All Features ----
customer_tbl %>% 
    drop_na() %>% 
    select(-cust_id) %>% 
    gather(key = "key", value = "value") %>% 
    ggplot(aes(value))+
    geom_histogram(color = "white", fill = "#2c3e50", bins = 35)+
    facet_wrap(~ key, scales = "free", ncol = 4)+
    theme_bw()+
    theme(
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 9, face = "bold")
    )+
    labs(title = "Distribution of Feature Values", x = NULL, y = NULL)


# 3.3.2 Histograms of Credit Utilization ----

# - Data
cc_utilization_tbl <- customer_tbl %>% 
    drop_na() %>% 
    mutate(credit_utilization = balance/credit_limit) %>% 
    mutate(cohort = ifelse(credit_utilization >= 1, "Above 100%", "Below 100%")) %>% 
    mutate(threshold = ifelse(credit_utilization >= 0.30, "Above", "Below")) 


# - Visual
cc_utilization_tbl %>% 
    ggplot(aes(credit_utilization))+
    geom_histogram(col = "white", fill = "#2c3e50", bins = 50)+
    geom_vline(aes(xintercept = 0.30), color = "orange")+
    facet_wrap(~ cohort, scales = "free")+
    theme_bw()+
    labs(
        title = "Credit Utilization",
        subtitle = str_glue("2.6% of customers have credit utilization above 100%
                            51% of customers have credit utilization above 30%"),
        y = "frequency",
        x = "credit_utilization (%)"
    )+
    theme(strip.text = element_text(face = "bold"))

cc_utilization_tbl %>% count(threshold) %>% mutate(pct = n/sum(n))

cc_utilization_tbl %>% count(cohort) %>% mutate(pct = n/sum(n))


# 3.4 Example Cohort 1: Cash Advance Customers ----
#   - These customers have high cash advance, and appear to use cc for loans.
customer_tbl %>% 
    drop_na() %>% 
    select(cust_id, cash_advance, cash_advance_trx, purchases) %>% 
    filter(purchases == 0 & cash_advance >= 0) %>%
    mutate(cash_advance_avg = cash_advance/cash_advance_trx) %>% 
    mutate(cohort = case_when(
        cash_advance_avg <= 1000                 ~ "Low",
        cash_advance_avg %>% between(1001, 5000) ~ "Medium",
        TRUE                                     ~ "High"
    )) %>% 
    mutate(cohort = fct_reorder(cohort, cash_advance_avg)) %>% 
    ggplot(aes(cash_advance_avg, cohort, color = cohort))+
    geom_boxplot()+
    theme_bw()+
    scale_fill_tq()
    labs(
        title = "Avg Cash Advance for Customers with 0 Purchases",
        subtitle = "These customers appear to be using their credit cards to take loans",
        y = NULL
    )+
    theme(legend.position = "none")+
    scale_color_tq()



# 3.5 Example Cohort 2: Purchase/Balance Customers ----

# - Histograms
customer_tbl %>% 
    drop_na() %>% 
    filter(cash_advance == 0) %>% 
    select(purchases, balance) %>% 
    filter(purchases > 100) %>%
    gather() %>% 
    ggplot(aes(value))+
    geom_histogram(col = "white", fill = "#2c3350", bins = 100)+
    facet_wrap(~ key, ncol = 1, scales = "free")+
    theme_bw()+
    scale_fill_tq()+
    labs(
        title = "Histogram of Purchases & Balance",
        x = NULL, y = "frequency"
    )

# - Category Data
purchase_balance_tbl <- customer_tbl %>% 
    drop_na() %>% 
    filter(cash_advance == 0) %>% 
    filter(purchases > 100) %>%
    mutate(purchase_cohort = ntile(purchases, 2)) %>% 
    mutate(balance_cohort = ntile(balance, 2)) %>% 
    select(balance, purchases, purchase_cohort, balance_cohort) %>% 
    mutate(purchase_cohort = case_when(
        purchase_cohort == 1 ~ "Low Purchase",
        purchase_cohort == 2 ~ "High Purchase"
        # TRUE                 ~ "High Purchase"
    )) %>% 
    mutate(balance_cohort = case_when(
        balance_cohort == 1 ~ "Low Balance",
        balance_cohort == 2 ~ "High Balance"
        # TRUE                ~ "High Balance"
    )) %>% 
    mutate(cohort = paste0(purchase_cohort, " / ", balance_cohort))

# - Category Visuals
purchase_balance_tbl %>% 
    select(-purchase_cohort, -balance_cohort) %>% 
    group_by(cohort) %>% 
    summarise(
        median_balance = median(balance),
        median_purchases = median(purchases)
    ) %>% 
    ungroup() %>% 
    gather(key = "key", value = "avg_value", !cohort) %>% 
    mutate(cohort = fct_reorder(cohort, avg_value)) %>% 
    ggplot(aes(avg_value, cohort, fill = key))+
    geom_col(position = "dodge")+
    theme_bw()+
    scale_fill_tq(name = "")+
    labs(
        title = "Purchase / Balance Customer Trends",
        subtitle = "These customers appear to be using their credit cards for strickly purchasing purposes",
        y = NULL
    )+
    theme(legend.position = "bottom")


# *************************************************************************
# 4.0 ANALYSIS COHORT ----
# *************************************************************************

# 4.1 Tenure = 10 Cohort ----
# - Filtering down the data to focus on a the cohort of customers with tenure = 10
analysis_cohort_tbl <- customer_tbl %>% 
    filter(tenure == 10) %>% 
    drop_na() %>% 
    mutate(credit_utilization = balance / credit_limit) %>% 
    filter(!cust_id == "C11004")

customer_tbl %>% count(tenure, sort = TRUE) %>% mutate(pct = n/sum(n))


# *************************************************************************
# 5.0 DATA PREPROCESSING ----
# *************************************************************************

# 5.1 Preprocessing Spec ----
recipe_spec <- recipe(~ ., data = analysis_cohort_tbl) %>% 
    step_rm(cust_id, installments_purchases) %>% 
    # step_naomit(all_numeric_predictors()) %>% 
    # step_mutate(credit_utillization = balance / credit_limit) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_pca(all_numeric_predictors(), threshold = 0.8)

# 5.2 Apply Preprocessing Spec ----
normalized_tbl <- recipe_spec %>% prep() %>% juice()

# 5.3 Save Sample of Preprocessed Dataset ----
normalized_tbl %>% 
    head(10) %>% 
    kableExtra::kbl() %>% 
    kableExtra::kable_minimal(full_width = FALSE)
    #write_rds("../artifacts/normalized_tbl_sample.rds")

# *************************************************************************
# 6.0 K-MEANS ----
# *************************************************************************

# 6.1 Optimal Centers Function ----
kmeans_mapper <- function(centers = 3) {
    normalized_tbl %>% 
        kmeans(centers = centers, nstart = 100)
}

# 6.2 Map Optimal Centers Function ----
set.seed(111)
kmeans_mapped_tbl <- tibble(centers = 1:15) %>% 
    mutate(k_means = centers %>% map(kmeans_mapper)) %>%  
    mutate(glance = k_means %>% map(glance)) %>% 
    unnest(glance)


# 6.3 Skree Plot ----
kmeans_mapped_tbl %>% 
    select(centers, tot.withinss) %>% 
    ggplot(aes(centers, tot.withinss))+
    geom_point(size = 3, color = "#2c3e50")+
    geom_line(size = 1, color = "#2c3e50")+
    ggrepel::geom_label_repel(aes(label = centers))+
    theme_bw()+
    labs(
        title    = "Skree Plot",
        subtitle = "Measures the distance that each customer is from the closest K-Means center"
    )
    
    # geom_point(aes(x = 6, y = 900), shape = 1, size = 20, color = "orange")
    # geom_curve(
    #     aes(x = 5.5, y = 104000, xend = 5.2, yend = 100000), 
    #     curvature = 0.2, lineend = "round", size = 1, color = "orange", 
    #     arrow = arrow(length = unit(0.2, "inches"))
    # )+
    # annotate(
    #     geom = "text", x = 6.5, y = 104000, fontface = "bold", size = 4,
    #     label = "Optimal Number of Clusters"
    # )



# *************************************************************************
# 7.0 UMAP ----
# *************************************************************************

# 7.1 Get UMAP Data ----
set.seed(111)
umap_obj <- normalized_tbl %>% umap::umap()

umap_tbl <- umap_obj$layout %>% 
    as_tibble() %>% 
    setNames(c("x", "y")) %>% 
    bind_cols(analysis_cohort_tbl %>% select(cust_id))


# *************************************************************************
# 8.0 COMBINE KMEANS & UMAP ----
# *************************************************************************
kmeans_4_obj <- kmeans_mapped_tbl %>% 
    pull(k_means) %>% 
    pluck(4)

kmeans_4_clusters_tbl <- kmeans_4_obj %>% 
    augment(analysis_cohort_tbl) %>% 
    select(cust_id, .cluster)


umap_kmeans_4_tbl <- umap_tbl %>% 
    left_join(kmeans_4_clusters_tbl) %>% 
    left_join(analysis_cohort_tbl)


# *************************************************************************
# 9.0 SAVE ARTIFACTS ----
# *************************************************************************

cohort_10_artifacts_list <- list(
    # data
    data = list(
        analysis_cohort_tbl, 
        kmeans_mapped_tbl, 
        umap_tbl, 
        kmeans_4_clusters_tbl,
        umap_kmeans_4_tbl
    ),
    
    # objects
    objects = list(umap_obj)
)

cohort_10_artifacts_list %>% write_rds("../artifacts/cohort_10_artifacts_list.rds")


# *************************************************************************
# 10.0 VISUALIZE CLUSTERS ----
# *************************************************************************

# * Cluster Assignments ----
umap_kmeans_4_tbl %>% 
    ggplot(aes(x = x, y = y, color = .cluster))+
    geom_point(size = 3)+
    theme_bw()+
    tidyquant::scale_color_tq()+
    labs(
        title = "Customer Segmentation: 2D Projection",
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignments",
        caption = "Conclusion: 4 customer segments identified using 2 algorithms"
    )


# *************************************************************************
# 11.0 CLUSTER ANALYSIS ----
# *************************************************************************

# 11.1 Feature Distribution by Cluster (Viz 1) ----
umap_kmeans_4_tbl %>% 
    mutate(credit_utilization = balance / credit_limit) %>% 
    select(.cluster, balance, credit_utilization, purchases, cash_advance) %>% 
    gather(key = "key", value = "value", !.cluster) %>% 
    drop_na() %>% 
    ggplot(aes(value, .cluster, col = .cluster))+
    geom_boxplot()+
    facet_wrap(~ key, scales = "free")+
    theme_bw()+
    scale_color_tq()+
    labs(
        title = "Feature Distribution by Clusters",
        x = NULL, y = "cluster"
    )+
    theme(
        strip.text = element_text(face = "bold"),
        legend.position = "none"
    )


# 11.2 Feature Distribution by Cluster (Viz 2) ----
umap_kmeans_4_tbl %>% 
    drop_na() %>% 
    select(.cluster, credit_limit, purchases_trx, payments, minimum_payments) %>% 
    gather(key = "key", value = "value", !.cluster) %>% 
    ggplot(aes(value, .cluster, col = .cluster))+
    geom_boxplot()+
    facet_wrap(~ key, scales = "free")+
    theme_bw()+
    scale_color_tq()+
    labs(
        title = "Feature Distribution by Clusters",
        subtitle = "Credit Limit, Minimum Payments, Payments, Purchases Transactions",
        x = NULL, y = "cluster"
    )+
    theme(
        strip.text = element_text(face = "bold"),
        legend.position = "none"
    )


# 11.3 Cluster Stats ----
umap_kmeans_4_tbl %>% 
    drop_na() %>% 
    select(.cluster, credit_utilization, cash_advance, purchases, balance) %>% 
    group_by(.cluster) %>% 
    summarise(across(credit_utilization:balance, list(median = median, max = max))) %>% 
    ungroup()

# *************************************************************************
# SECTION NAME ----
# *************************************************************************
# *************************************************************************
# SECTION NAME ----
# *************************************************************************











