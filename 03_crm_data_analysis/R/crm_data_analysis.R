# CRM DATA ANALYSIS ----
# *** ----

# *************************************************************************
# NOTES / QUESTIONS -----
# *************************************************************************

# - Determine the right features to segment the crm data?
# - What are the key metrics to track for every segment?
# - How are segments performing for each metric?
# - How do we analyze the performance of sales agents?


# *************************************************************************
# 1.0 SETUP ----
# *************************************************************************

# Working Dir ----
setwd(here::here("03_crm_data_analysis", "R"))

# Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(rlang)
library(ggthemes)
library(gt)
library(gtExtras)
library(webshot2)
library(magrittr)

# Source Functions ----
source("../functions/utils_data_wrangling.R")
source("../functions/utils_gt_table.R")
source("../functions/utils_plotting.R")



# *************************************************************************
# 2.0 LOAD DATA ----
# *************************************************************************

# Load Data ----
accounts_tbl <- read_csv("../data/crm_data/accounts.csv") %>% 
    as_tibble() %>% 
    clean_names()

products_tbl <- read_csv("../data/crm_data/products.csv") %>% 
    as_tibble() %>% 
    clean_names()

pipeline_tbl <- read_csv("../data/crm_data/sales_pipeline.csv") %>% 
    as_tibble() %>% 
    clean_names()

teams_tbl <- read_csv("../data/crm_data/sales_teams.csv") %>% 
    as_tibble() %>% 
    clean_names()

data_description_tbl <- read_csv("../data/crm_data/data_dictionary.csv") %>% 
    as_tibble() %>% 
    clean_names()

# Check for Missing Values ----
accounts_tbl %>% sapply(function(x) sum(is.na(x)))

products_tbl %>% sapply(function(x) sum(is.na(x)))

pipeline_tbl %>% sapply(function(x) sum(is.na(x)))

teams_tbl %>% sapply(function(x) sum(is.na(x)))

# Check for Duplicates ----
accounts_tbl %>% duplicated() %>% sum()

products_tbl %>% duplicated() %>% sum()

pipeline_tbl %>% duplicated() %>% sum()

teams_tbl %>% duplicated() %>% sum()


# *************************************************************************
# 3.0 MERGE DATA ----
# *************************************************************************

# 3.1 Merge and Format Data ----
crm_tbl <- pipeline_tbl %>% 
    left_join(teams_tbl, by = c("sales_agent" = "sales_agent")) %>% 
    left_join(products_tbl, by = c("product" = "product")) %>% 
    left_join(accounts_tbl, by = c("account" = "account")) %>% 
    
    # deal close month
    mutate(engage_month = floor_date(engage_date, "month"), .after = close_date) %>% 
    mutate(close_month = floor_date(close_date, "month"), .after = engage_month) %>%
    
    # employee size
    mutate(employee_size = case_when(
        employees > 0 & employees <= 1000     ~ "0 - 1000",
        employees > 1000 & employees <= 2500  ~ "1001 - 2500",
        employees > 2500 & employees <= 5000  ~ "2051 - 5000",
        employees > 5000 & employees <= 7500  ~ "5001 - 7500",
        employees > 7500 & employees <= 10000 ~ "7501 - 10000",
        TRUE                                  ~ "10000 +"
    )) %>% 
    mutate(employee_size = as.factor(employee_size)) %>%
    mutate(
        employee_size = fct_relevel(
            employee_size, "0 - 1000", "1001 - 2500", "2051 - 5000", 
            "5001 - 7500", "7501 - 10000", "10000 +"
        )
    ) %>%
    
    # time to close
    mutate(time_to_close = case_when(
        is.na(engage_date) ~ NA,
        is.na(close_date)  ~ NA,
        TRUE ~ as.numeric(difftime(close_date, engage_date, units = "days"))

    )) %>% 
    
    # time to close bins
    mutate(time_to_close_bin = case_when(
        time_to_close <= 30 ~ "0 - 30 days",
        time_to_close > 30 & time_to_close <= 60 ~ "31 - 60 days",
        time_to_close > 60 & time_to_close <= 90 ~ "61 - 90 days",
        time_to_close > 90 ~ "90+ days"
    )) %>% 
    
    # fix column values
    mutate(sector = ifelse(sector == "technolgy", "technology", sector)) %>%
    mutate(revenue = revenue * 1000000) %>%
    
    # drop unwanted columns
    select(-c(series, subsidiary_of, sales_price))

crm_tbl %>% glimpse()


crm_tbl %>% count(employee_size, sort = TRUE) %>% mutate(prop = n / sum(n))
crm_tbl %>% count(opportunity_id, sort = TRUE)
crm_tbl %>% count(deal_stage, sort = TRUE)
    
    

# 3.2 Save Merged Data ----
write_csv(crm_tbl, "../data/crm_data/crm_merged_tbl.csv")


# *************************************************************************
# 4.0 DATA FILTERING ----
# *************************************************************************

# * Filter for United States ----
crm_usa_tbl <- crm_tbl %>% filter(office_location == "United States")


# *************************************************************************
# 5.0 EXPLORATORY DATA ANALYSIS ----
# *************************************************************************

# 4.1 Date Ranges ----
crm_usa_tbl %>% pull(engage_date) %>% range(na.rm = TRUE)
crm_usa_tbl %>% pull(close_date) %>% range(na.rm = TRUE)

# 4.2 Unique Features List ----
cols = c("sales_agent", "manager", "regional_office", "deal_stage", 
         "product", "sector", "employee_size")

# 4.3 Count of Unique Feature Values ----
crm_usa_tbl %>% 
    select(all_of(cols)) %>% 
    map(function(x) n_distinct(x, na.rm = TRUE))

# 4.4 Unique Feature Values ----
crm_usa_tbl %>% 
    select(all_of(cols)) %>% 
    map(unique)

# 4.5 Skim Data (to get data summary) ----
skimr::skim(crm_usa_tbl)


# *************************************************************************
# 6.0 SEGMENTATION ----
# *************************************************************************

#' Segments will be Sector, Employee Size and Regional Office


# *************************************************************************
# 7.0 REVENUE ANALYSIS ----
# *************************************************************************

# 7.1 Segments List ----
segments <- c("sector", "employee_size", "regional_office")

# 7.2 Accounts, Deals & Revenue by Segments List ----
segments_revenue_tbl <- segments %>% 
  map(function(segment) {
    crm_usa_tbl %>% 
      group_by(!!rlang::ensym(segment), account) %>%
      summarise(
        accounts         = n_distinct(account),
        deals            = n_distinct(opportunity_id),
        avg_rev_per_acct = mean(revenue, na.rm = TRUE), 
        .groups          = 'drop'
      ) %>% 
      group_by(!!rlang::ensym(segment)) %>%
      summarise(
          accounts    = sum(accounts),
          deals       = sum(deals),
          avg_revenue = mean(avg_rev_per_acct)
      ) %>% 
      ungroup() %>% 
      arrange(desc(avg_revenue)) %>% 
      mutate(accounts_pct = accounts / sum(accounts)) %>%
      mutate(deals_pct    = deals / sum(deals)) %>%
      drop_na()
})

# 7.3 Average Time To Close by Segments List ----
segments_time_to_close <- segments %>% 
  map(function(segment) {
    crm_usa_tbl %>% 
        summarise(
            avg_time_to_close = mean(time_to_close, na.rm = TRUE),
            .by = c(segment)
        ) %>% 
        arrange((avg_time_to_close)) %>% 
        drop_na()
}) 

# 7.4 Revenue by Sector (GT Table) ----
segments_revenue_tbl[[1]] %>% 
    arrange(desc(deals_pct)) %>% 
    get_gt_table(
        title                = "Segmentation Analysis (by Sector)",
        #subtitle             = "Accounts/Revenue/Deals Analysis by Sector",
        green_format_column  = "avg_revenue",
        blue_format_column   = "accounts_pct",
        orange_format_column = "deals_pct"
    ) %>%
    gtsave_extra(filename = "../png/segmentation_analysis_sector.png", zoom = 2)

# 7.5 Revenue by Employee Size (GT Table) ----
segments_revenue_tbl[[2]] %>% 
    arrange(desc(deals_pct)) %>%
    get_gt_table(
        title                = "Segmentation Analysis (by Employee Size)",
        #subtitle             = "Accounts/Revenue/Deals Analysis by Employee Size",
        green_format_column  = "avg_revenue",
        blue_format_column   = "accounts_pct",
        orange_format_column = "deals_pct"
    ) %>% 
    gtsave_extra(filename = "../png/segmentation_analysis_emp_size.png", zoom = 2)

# 7.6 Revenue by Regional Office (GT Table) ----
segments_revenue_tbl[[3]] %>% 
    arrange(desc(deals_pct)) %>%
    rename(office = regional_office) %>%
    get_gt_table(
        title                = "Segmentation Analysis (by Regional Office)",
        #subtitle             = "Accounts/Revenue/Deals Analysis by Regional Office",
        green_format_column  = "avg_revenue",
        blue_format_column   = "accounts_pct",
        orange_format_column = "deals_pct"
    ) %>% 
    gtsave_extra(filename = "../png/segmentation_analysis_reg_office.png", zoom = 2)

# 7.7 Time to Close by Sector (GT Table) ----
segments_time_to_close[[1]] %>% 
    get_gt_table(title = "Avg Time to Close (Sector)") %>% 
    cols_width(everything() ~ px(250)) %>% 
    fmt_number(columns = everything(), decimals = 1) %>% 
    gtsave_extra(filename = "../png/time_to_close_sector.png", zoom = 2)

# 7.8 Time to Close by Employee Size (GT Table) ----
segments_time_to_close[[2]] %>%
    get_gt_table(title = "Avg Time to Close (Emp Size)") %>% 
    cols_width(everything() ~ px(250)) %>% 
    fmt_number(columns = everything(), decimals = 1) %>% 
    gtsave_extra(filename = "../png/time_to_close_emp_size.png", zoom = 2)

# 7.9 Time to Close by Regional Office (GT Table) ----
segments_time_to_close[[3]] %>%
    get_gt_table(title = "Avg Time to Close (Regional Office)") %>% 
    cols_width(everything() ~ px(250)) %>% 
    fmt_number(columns = everything(), decimals = 1) %>% 
    gtsave_extra(filename = "../png/time_to_close_reg_office.png", zoom = 2)



# *************************************************************************
# 8.0 METRICS ANALYSIS ----
# *************************************************************************

#' Key Metrics to Analyze - 
#' - Deal Close Rate
#' - Average Deal Value
#' - Average Time to Close
#' - Average Deal ASP
#' - Average Deal Event Value

# 8.1 Time to Close Buckets ----
segment_name <- "sector" # change to either "employee_size" or "regional_office" and re-run code below
segment_name_caps <- segment_name %>% str_replace_all("_", " ") %>% str_to_title()

crm_usa_tbl %>% 
    select(!!ensym(segment_name), time_to_close_bin) %>% 
    drop_na() %>%
    summarise(
        count = n(),
        .by = c(!!ensym(segment_name), time_to_close_bin)
    ) %>% 
    mutate(pct = count / sum(count), .by = !!ensym(segment_name)) %>%
    ggplot(aes(count, !!ensym(segment_name), fill = time_to_close_bin)) +
    geom_col(position = "fill") +
    geom_text(
        aes(label = scales::percent(pct, accuracy = 0.02)), 
        position = position_fill(vjust = 0.5)
    ) +
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 11, color = "black"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold")
    )+
    labs(
        title = str_glue("Time To Close Analysis by {segment_name_caps}"), 
        x = NULL
    )
    
# 8.2 Metrics by Segment Tibble List ----

# - Deal Close Rate, Average Deal Value, Deal Event Value
segments <- c("sector", "employee_size", "regional_office")

segments_metrics_tbl <- segments %>% 
    map(function(segment) {
        crm_usa_tbl %>% 
            summarise(
                deals_total = n_distinct(opportunity_id),
                deals_closed = n_distinct(opportunity_id[deal_stage == "Won"]),
                total_close_value = sum(close_value[deal_stage == "Won"]),
                .by = c(segment)
            ) %>% 
            mutate(deals_close_rate = deals_closed / deals_total, .after = deals_closed) %>% 
            mutate(avg_deal_value = total_close_value / deals_closed) %>%
            mutate(deal_event_value = deals_close_rate * avg_deal_value) %>%
            arrange(desc(deal_event_value)) %>% 
            drop_na()
})

# 8.3 Metrics vs Company (Sector) ----
segments_metrics_tbl[[1]] %>% 
    get_metrics_vs_company_data_prepped(
        crm_data    = crm_usa_tbl, 
        sort_column = "deal_event_value"
    ) %>% 
    get_gt_table(
        title                = "Metrics Analysis (by Sector)",
        green_format_column  = "total_close_value",
        blue_format_column   = "avg_deal_value",
        orange_format_column = "deal_event_value",
        red_format_column    = "deals_close_rate"
    ) %>% 
    get_gt_table_with_spanner() %>% 
    gtsave_extra(filename = "../png/metrics_analysis_sector_vs_company.png", zoom = 2) # save as png

# 8.4 Metrics vs Company (Employee Size) ----
segments_metrics_tbl[[2]] %>% 
    get_metrics_vs_company_data_prepped(
        crm_data = crm_usa_tbl, 
        sort_column = "deal_event_value"
    ) %>% 
    get_gt_table(
        title                = "Metrics Analysis (by Employee Size)",
        green_format_column  = "total_close_value",
        blue_format_column   = "avg_deal_value",
        orange_format_column = "deal_event_value",
        red_format_column    = "deals_close_rate"
    ) %>% 
    get_gt_table_with_spanner() %>% 
    gtsave_extra(filename = "../png/metrics_analysis_emp_size_vs_company.png", zoom = 2) # save as png

# 8.5 Metrics vs Company (Employee Size) ----
segments_metrics_tbl[[3]] %>% 
    get_metrics_vs_company_data_prepped(
        crm_data = crm_usa_tbl, 
        sort_column = "deal_event_value"
    ) %>% 
    get_gt_table(
        title                = "Metrics Analysis (by Regional Office)",
        green_format_column  = "total_close_value",
        blue_format_column   = "avg_deal_value",
        orange_format_column = "deal_event_value",
        red_format_column    = "deals_close_rate"
    ) %>% 
    get_gt_table_with_spanner() %>% 
    gtsave_extra(filename = "../png/metrics_analysis_reg_office_vs_company.png", zoom = 2) # save as png


# *************************************************************************
# 9.0 METRICS TREND ANALYSIS ----
# *************************************************************************

# 9.1 Total Deals & Close Rate ----

# 9.1.1 Overall ----
crm_usa_tbl %>% 
    get_metrics_trend_data_prepped(group_column = "engage_month") %>% 
    get_metrics_trend_combo_plot(
        combo            = TRUE,
        facet            = FALSE,
        x_axis_text_size = 10,
        y_axis_text_size = 10
    ) %>% 
    ggsave(filename = "../png/total_deals_and_close_rate_trend.png", width = 12, height = 6)
    
    
# 9.1.2 Sector ----
get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "sector"
) %>% 
  get_metrics_trend_combo_plot(
      combo            = TRUE,
      facet            = TRUE,
      x_axis_text_size = 9,
      facet_ncol       = 5
    ) %>% 
    ggsave(filename = "../png/total_deals_and_close_rate_trend_sector.png", width = 14, height = 6)
    

# 9.1.3 Employee Size ----
get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "employee_size"
) %>% 
  get_metrics_trend_combo_plot(
      combo = TRUE,
      facet = TRUE,
      x_axis_text_size = 9,
      facet_ncol = 3
    ) %>% 
    ggsave(filename = "../png/total_deals_and_close_rate_trend_empsize.png", width = 12, height = 6)
    

# 9.1.4 Regional Office ----
get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "regional_office"
) %>% 
    get_metrics_trend_combo_plot(
        combo = TRUE,
        facet = TRUE,
        x_axis_text_size = 9,
        facet_ncol = 3
    ) %>% 
    ggsave(filename = "../png/total_deals_and_close_rate_trend_office.png", width = 12, height = 3)


# 9.2 Average Deal Value Trend ----

# 9.2.1 Overall ----
{
  get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "sector"
  ) %>% 
    get_metrics_trend_combo_plot(
      yp_vars = "avg_deal_value",
      combo = FALSE,
      facet = TRUE,
      x_axis_text_size = 9,
      facet_ncol = 5,
    ) + 
    labs(
      title = "Average Deal Value Trend (by Sector)",
      y     = "Average Deal Value ($)\n"
    )
} %>% 
    ggsave(filename = "../png/avg_deal_value_trend_sector.png", width = 14, height = 6)

# 9.2.2 Employee Size ----
{
  get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "employee_size"
  ) %>% 
    get_metrics_trend_combo_plot(
      yp_vars          = "avg_deal_value",
      combo            = FALSE,
      facet            = TRUE,
      x_axis_text_size = 9,
      facet_ncol       = 3
    )
} %>% 
    ggsave(filename = "../png/avg_deal_value_trend_emp_size.png", width = 12, height = 6)

# 9.2.3 Regional Office ----
{
  get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "regional_office"
  ) %>% 
    get_metrics_trend_combo_plot(
      yp_vars          = "avg_deal_value",
      combo            = FALSE,
      facet            = TRUE,
      x_axis_text_size = 9,
      facet_ncol       = 3
    )
} %>% 
    ggsave(filename = "../png/avg_deal_value_trend_reg_office.png", width = 12, height = 3)
    

# 9.3 Deal Event Value ----

# 9.3.1 Sector ----
{
  get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "sector"
  ) %>% 
    get_metrics_trend_combo_plot(
      yp_vars          = "deal_event_value",
      combo            = FALSE,
      facet            = TRUE,
      x_axis_text_size = 9,
      facet_ncol       = 5
    ) + 
    labs(
      title = "Deal Event Value Trend (by Sector)",
      y = "Deal Event Value ($)\n"
    )
} %>% 
    ggsave(filename = "../png/deal_event_value_trend_sector.png", width = 14, height = 6)

# 9.3.2 Employee Size ----
{
  get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "employee_size"
  ) %>% 
    get_metrics_trend_combo_plot(
      yp_vars          = "deal_event_value",
      combo            = FALSE,
      facet            = TRUE,
      x_axis_text_size = 9,
      facet_ncol       = 3
    ) + 
    labs(
      title = "Deal Event Value Trend (by Employee Size)",
      y = "Deal Event Value ($)\n"
    )
} %>% 
    ggsave(filename = "../png/deal_event_value_trend_emp_size.png", width = 12, height = 6)
    

# 9.3.3 Regional Office ----
{
  get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    segment_name = "regional_office"
  ) %>% 
    get_metrics_trend_combo_plot(
      yp_vars          = "deal_event_value",
      combo            = FALSE,
      facet            = TRUE,
      x_axis_text_size = 9,
      facet_ncol       = 3
    ) + 
    labs(
      title = "Deal Event Value Trend (by Regional Office)",
      y = "Deal Event Value ($)\n"
    )
} %>% 
    ggsave(filename = "../png/deal_event_value_trend_reg_office.png", width = 12, height = 3)
    

# *************************************************************************
# 10.0 METRICS ANALYSIS (SALES AGENTS) ----
# *************************************************************************

# 10.1 Categorize Sales Agents Performance ----

# 10.1.1 Sales Agents Metrics Data ----
sales_agent_metrics_tbl <- crm_usa_tbl %>% 
    summarise(
        deals_total = n_distinct(opportunity_id),
        deals_closed = n_distinct(
            opportunity_id[
                time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won"
            ]
        ),
        total_close_value = sum(
            close_value[
                time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won"
            ]
        ),
        avg_time_to_close = mean(
            time_to_close[
                time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won"
            ], na.rm = TRUE
        ),
        .by = c(sales_agent)
    ) %>% 
    mutate(deals_close_rate = deals_closed / deals_total, .after = deals_closed) %>% 
    mutate(avg_deal_value = total_close_value / deals_closed) %>%
    mutate(deal_event_value = deals_close_rate * avg_deal_value) %>%
    drop_na() %>% 
    left_join(
        crm_usa_tbl %>% 
            select(sales_agent, manager, regional_office) %>% 
            distinct(),
        by = "sales_agent"
    ) %>% 
    select(sales_agent, manager, regional_office, everything())

# 10.1.2 Get Quantiles ----
sales_agent_metrics_score_tbl <- sales_agent_metrics_tbl %>% 
    get_quantiles(c("deals_close_rate", "avg_deal_value", "avg_time_to_close")) %>% 
    mutate(combined_score = deals_close_rate_qtile + avg_deal_value_qtile + avg_time_to_close_qtile) %>% 
    select(-c(total_close_value, deals_total, deals_closed)) 

sales_agent_metrics_score_tbl %>% glimpse()

# 10.1.3 Sales Agent Performance Categories ----
threshold <- quantile(sales_agent_metrics_score_tbl$combined_score, probs = c(1/3, 2/3))

sales_agent_metrics_category_tbl <- sales_agent_metrics_score_tbl %>% 
    mutate(score_category = case_when(
        combined_score <= threshold[1] ~ "Low Performers",
        combined_score > threshold[1] & combined_score <= threshold[2] ~ "Mid Performers",
        combined_score > threshold[2] ~ "Top Performers"
    ))

sales_agent_metrics_category_tbl %>% count(score_category)



# 10.2 Performance Categories GT Tables ----

# 10.2.1 Top Performers GT Table ----
sales_agent_metrics_category_tbl %>% 
    filter(score_category == "Top Performers") %>% 
    select(-c(score_category, ends_with("qtile"))) %>%
    arrange(desc(combined_score)) %>% 
    get_gt_table(
        title                = "Top Performers",
        green_format_column  = "combined_score"
    ) %>% 
    cols_width(columns = everything() ~ px(170)) %>% 
    gtsave_extra(filename = "../png/top_performers.png", zoom = 2)

# 10.2.2 Mid Performers GT Table ----
sales_agent_metrics_category_tbl %>% 
    filter(score_category == "Mid Performers") %>% 
  select(-c(score_category, ends_with("qtile"))) %>%
    arrange(desc(combined_score)) %>% 
    get_gt_table(
        title                = "Mid Performers",
        orange_format_column  = "combined_score"
    ) %>% 
    cols_width(columns = everything() ~ px(170)) %>% 
    gtsave_extra(filename = "../png/mid_performers.png", zoom = 2)

# 10.2.3 Low Performers GT Table ----
sales_agent_metrics_category_tbl %>% 
    filter(score_category == "Low Performers") %>% 
  select(-c(score_category, ends_with("qtile"))) %>%
    arrange(desc(combined_score)) %>% 
    get_gt_table(
        title                = "Low Performers",
        red_format_column  = "combined_score"
    ) %>% 
    cols_width(columns = everything() ~ px(170)) %>% 
    gtsave_extra(filename = "../png/low_performers.png", zoom = 2)



# 10.3 Hypothesize Performance Drivers ----

# 10.3.1 Sales Agent by Tenure ----
sales_agent_tenure_tbl <- crm_usa_tbl %>% 
    select(sales_agent, engage_date) %>% 
    summarise(
        min_egage_date = min(engage_date, na.rm = TRUE),
        tenure_length = as.numeric(
            difftime(max(
                crm_usa_tbl$engage_date, na.rm = TRUE), 
                min_egage_date, units = "days"
            )
        ),
        .by = c(sales_agent)
    ) %>% 
    left_join(
        sales_agent_metrics_category_tbl %>% 
            select(sales_agent, combined_score, score_category),
        by = "sales_agent"
    )

# 10.3.2 Sales Agent Tenure GT ----
sales_agent_tenure_tbl %>% 
    arrange(desc(combined_score))  %>% 
    get_gt_table(
        title                = "Sales Agent Tenure (Days)",
        green_format_column  = "tenure_length"
    ) %>% 
    cols_width(columns = everything() ~ px(170)) %>% 
    gtsave_extra(filename = "../png/sales_agent_tenure.png", zoom = 2)

# 10.3.3 Sales Agent Average Tenure GT ----
sales_agent_tenure_tbl %>% 
    summarize(
        avg_tenure_length = mean(tenure_length, na.rm = TRUE),
        .by = c(score_category)
    ) %>% 
    arrange(desc(score_category)) %>% 
    get_gt_table(
        title = "Sales Agent Average Tenure (Days)"
    ) %>%
    cols_width(columns = everything() ~ px(170)) %>%
    gtsave_extra(filename = "../png/sales_agent_avg_tenure.png", zoom = 2)
  


# 10.4 Sales Agent by Manager ----

# 10.4.1 Count of Sales Agent by Manager GT ----
sales_agent_metrics_category_tbl %>% 
    summarise(
        sales_agents = n_distinct(sales_agent),
        .by = c(manager, score_category)
    ) %>% 
    pivot_wider(names_from = score_category, values_from = sales_agents, values_fill = 0) %>% 
    mutate(total_agents = `Low Performers` + `Mid Performers` + `Top Performers`) %>% 
    select(manager, `Top Performers`, `Mid Performers`, `Low Performers`, total_agents) %>% 
    arrange(desc(`Top Performers`)) %>% 
    get_gt_table(
        title = "Sales Agent by Manager",
        green_format_column = "Top Performers",
        orange_format_column = "Mid Performers",
        red_format_column = "Low Performers"
    ) %>% 
    cols_width(columns = everything() ~ px(170)) %>% 
    gtsave_extra(filename = "../png/sales_agent_by_manager.png", zoom = 2)

# 10.4.2 Count of Sales Agent by Regional Office GT ----
sales_agent_metrics_category_tbl %>% 
    summarise(
        sales_agents = n_distinct(sales_agent),
        .by = c(regional_office, score_category)
    ) %>% 
    pivot_wider(names_from = score_category, values_from = sales_agents, values_fill = 0) %>% 
    mutate(Total = `Low Performers` + `Mid Performers` + `Top Performers`) %>% 
    select(regional_office, `Top Performers`, `Mid Performers`, `Low Performers`, Total) %>% 
    arrange(desc(`Top Performers`)) %>% 
    get_gt_table(
        title = "Sales Agent by Regional Office",
        green_format_column = "Top Performers",
        orange_format_column = "Mid Performers",
        red_format_column = "Low Performers"
    ) %>% 
    cols_width(columns = everything() ~ px(170)) %>% 
    gtsave_extra(filename = "../png/sales_agent_by_reg_office.png", zoom = 2)


# *************************************************************************
# 11.0 METRICS BY PRODUCTS ----
# *************************************************************************

# 11.1 Metrics by Products Data ----
metrics_products_tbl <- get_metrics_trend_data_prepped(
    data         = crm_usa_tbl,
    group_column = "product"
) %>% 
    arrange(desc(deal_event_value))

# 11.2 Products by Sales Agent ----
metrics_product_and_sales_agent_tbl <- crm_usa_tbl %>% 
    filter( time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won") %>% 
    summarise(
        deals_total = n_distinct(opportunity_id),
        .by = c(sales_agent, product)
    ) %>% 
    left_join(
        sales_agent_metrics_category_tbl %>% select(sales_agent, combined_score, score_category),
        by = "sales_agent"
    ) %>% 
    arrange(desc(combined_score)) %>% 
    pivot_wider(names_from = product, values_from = deals_total, values_fill = 0)

# 11.3 Top 3 & Bottom 3 Sales Agents Comparison ----
metrics_product_and_sales_agent_tbl %>%
    head(3) %>% 
    bind_rows(
        metrics_product_and_sales_agent_tbl %>% 
            tail(3)
    ) %>% 
    get_gt_table(
        title = "Top 3 & Bottom 3 Sales Agents Comparison",
        subtitle = "Number of Closed by Product"
    ) %>%
    cols_width(
        columns = c("Sales Agent", "Combined Score",  "Score Category", "Gtx Plus Basic") 
        ~ px(175)
    )




# *************************************************************************
# REPREX ----
# *************************************************************************

# segments_time_to_close %>% 
#     gt(rowname_col = "name") %>% 
#     tab_row_group(
#         label = md("**Sector**"),
#         rows = category == "sector",
#         id = "sector"
#     ) %>% 
#     tab_row_group(
#         label = md("**Employee Size**"),
#         rows = category == "employee_size",
#         id = "employee_size"
#     ) %>%
#     tab_row_group(
#         label = md("**Regional Office**"),
#         rows = category == "regional_office",
#         id = "regional_office"
#     ) %>% 
#     row_group_order(groups = c("sector", "employee_size", "regional_office")) %>% 
#     gt_theme_guardian() %>% 
#     fmt_number(columns = everything(), decimals = 1) 


# Load necessary library
library(gt)

# Sample data
data <- tribble(
    ~group, ~item, ~value,
    "Fruit", "Apple", 10,
    "Fruit", "Orange", 12,
    "Vegetable", "Carrot", 5,
    "Vegetable", "Onion", 7
)

# Create a gt table
gt_table <- data %>%
    gt() %>%
    cols_label(
        group = "Food Group",
        item = "Food Item",
        value = "Quantity"
    ) %>%
    tab_spanner(
        label = "Produce",
        columns = vars(group, item)
    )

# Add borders to the spanner header
gt_table <- gt_table %>%
    tab_style(
        style = list(
            cell_borders(
                sides = "bottom", 
                color = "blue",  
                weight = px(2)
            )
        ),
        locations = cells_column_spanners(spanners = "Produce")
    )

# Add borders to the columns under the spanner
gt_table <- gt_table %>%
    tab_style(
        style = list(
            cell_borders(
                sides = "bottom", # Bottom border for clarity in example, adjust as needed
                color = "blue",
                weight = px(2)
            )
        ),
        locations = cells_column_spanners(spanners = c("Produce"))
    )

# Print the table
gt_table

