

# Get Quantiles ----
get_quantiles <- function(df, columns) {
    # Check if columns are provided and valid
    if (missing(columns) || length(columns) == 0) {
        stop("No columns specified")
    }
    
    # Check if specified columns exist in the dataframe
    if (!all(columns %in% names(df))) {
        stop("One or more specified columns do not exist in the dataframe")
    }
    
    # Function to determine quantile ranks
    find_quantile_rank <- function(x) {
        quantiles <- quantile(x, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE, type = 7)
        findInterval(x, quantiles, all.inside = TRUE)
    }
    
    # Calculate quantiles for each specified column
    for (col in columns) {
        quantile_col_name <- paste0(col, "_qtile")
        df[[quantile_col_name]] <- find_quantile_rank(df[[col]])
    }
    
    # Reverse quantile ranks for avg time to close for consistency with common usage
    df <- df %>% mutate_at(vars(ends_with("close_qtile")), ~ 5 - .)
    
    return(df)
}


# Get Metrics vs Company Data Prepped ----
get_metrics_vs_company_data_prepped <- function(crm_data, segment_data, sort_column = "deal_event_value") {
    
    segment <- segment_data[, 1] %>% colnames()
    
    # overall company metrics
    overall_metrics_tbl <- crm_data %>% 
        summarise(
            deals_total = n_distinct(opportunity_id),
            deals_closed = n_distinct(opportunity_id[deal_stage == "Won"]),
            total_close_value = sum(close_value[deal_stage == "Won"]),
            avg_time_to_close = mean(time_to_close[deal_stage == "Won"], na.rm = TRUE)
        ) %>% 
        mutate(deals_close_rate = deals_closed / deals_total) %>% 
        mutate(avg_deal_value = total_close_value / deals_closed) %>%
        mutate(deal_event_value = deals_close_rate * avg_deal_value) %>%
        drop_na()
    
    # vs company metrics
    vs_company_tbl <- segment_data %>% 
        arrange(desc(!!rlang::ensym(sort_column))) %>% 
        mutate(deal_close_rate_vs_company = (deals_close_rate / overall_metrics_tbl$deals_close_rate) - 1) %>% 
        mutate(avg_deal_value_vs_company = (avg_deal_value / overall_metrics_tbl$avg_deal_value) - 1) %>%
        mutate(deal_event_value_vs_company = (deal_event_value / overall_metrics_tbl$deal_event_value) - 1) %>%
        select(c(
            !!rlang::ensym(segment), deals_total, deals_closed, total_close_value, deals_close_rate, deal_close_rate_vs_company,
            avg_deal_value, avg_deal_value_vs_company, deal_event_value, deal_event_value_vs_company, 
        ))
    
    # return
    return(vs_company_tbl)
    
}


# Get Metrics Trend Data Prepped ----
get_metrics_trend_data_prepped <- function(data, group_column = "engage_month", segment_name) {
  
    # rlang setup
    group_column_sym <- rlang::ensym(group_column)
    segment_name_sym <- rlang::ensym(segment_name)

    # data aggregation
    if (!missing(segment_name)) {
        grouped_tbl <- data %>% 
        group_by(!!group_column_sym, !!segment_name_sym)
       } else {
         grouped_tbl <- data %>% 
            group_by(!!group_column_sym)
    }
    
    # metrics trend table
    metrics_trend_tbl <- grouped_tbl %>%
    summarise(
        deals_total = n_distinct(opportunity_id),
        
        deals_closed = n_distinct(
            opportunity_id[
                time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won"
            ], na.rm = TRUE
        ),
        total_close_value = sum(
            close_value[
                time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won"
            ], na.rm = TRUE
        ),
        avg_time_to_close = mean(
            time_to_close[
                time_to_close_bin %in% c("0 - 30 days", "31 - 60 days") & deal_stage == "Won"
            ], na.rm = TRUE
        )
    ) %>%
    ungroup() %>% 
    mutate(deals_close_rate = deals_closed / deals_total, .after = deals_closed) %>%
    mutate(avg_deal_value = total_close_value / deals_closed) %>%
    mutate(deal_event_value = deals_close_rate * avg_deal_value) %>%
    drop_na()
    
    # return
    return(metrics_trend_tbl)
}


# Top N Sales by Segment ----
get_top_n_sales_by_category <- function(data, category_cols, head = 10) {
  
  data_prep <- data %>% 
    summarise(sales_usd = sum(sales_usd), .by = !!rlang::ensym(category_cols)) %>% 
    arrange(desc(sales_usd)) %>% 
    mutate(sales_prop = sales_usd / sum(sales_usd)) %>% 
    mutate(sales_prop_cummulative = cumsum(sales_prop)) %>%
    head(head)
  
  return(data_prep)
  
}