get_customer_metrics <-
function(data) {
    
    ret <- data %>% 
        filter(total_1yr_purch_net > 0) %>% 
        select(lead_source, days_to_purchase, total_1yr_purch_net, touch_points) %>% 
        mutate(total = "Total") %>%
        pivot_longer(
            cols      = -c(days_to_purchase, total_1yr_purch_net, touch_points),
            names_to  = "name",
            values_to = "campaign"
        ) %>% 
        summarise(
            total_customers = n(),
            avg_days_to_purch  = mean(days_to_purchase, na.rm = TRUE),
            avg_1yr_revenue    = mean(total_1yr_purch_net, na.rm = TRUE),
            touch_points       = mean(touch_points, na.rm = TRUE),
            .by = campaign
        ) %>% 
        arrange(campaign) %>% 
        
        # average customer age
        left_join(
            survey_tbl %>% 
                summarise(avg_age = mean(age, na.rm = TRUE), .by = lead_source) %>% 
                add_row(lead_source = "Total", avg_age = mean(survey_tbl$age)),
            by = c("campaign" = "lead_source")
        ) %>% 
        
        # customer acquisition cost
        left_join(
            get_ppc_metrics(
                data    = campaign_tbl,
                metric  = "cost",
                level   = "campaign"
            )
        ) %>% 
        mutate(customer_acq_cost = total_cost / total_customers) %>% 
        select(-total_cost) %>% 
        
        # lead to customer cvr
        left_join(
            get_ppc_metrics(
                data   = campaign_tbl,
                metric = "conversions",
                level  = "campaign"
            )
        ) %>% 
        mutate(lead_cvr = total_customers / total_conversions) %>% 
        select(-total_conversions)
    
    return(ret)
    
}
get_customer_lifetime_value <-
function(data, discount_rate, gross_margin) {
    
    # data: customer metrics table
    ret <- data %>% 
        select(campaign, avg_1yr_revenue, customer_acq_cost) %>% 
        
        # gross profit
        mutate(gross_profit = avg_1yr_revenue * gross_margin) %>% 
        
        # customer retention rate
        left_join(get_customer_retention()) %>% 
        
        # clv gross
        mutate(
            clv_gross = gross_profit * (
                retention_rate / (1 + discount_rate - retention_rate)
            )
        ) %>% 
        
        # clv net
        mutate(clv_net = clv_gross - customer_acq_cost)
    
    return(ret)
    
    
}
get_pivot_table <-
function(data, select_cols = NULL, unpivot_cols = NULL,
                            group_cols = NULL, return = "volume", format = FALSE) {
    
    
    # rlang setup
    select_cols   <- rlang::enquos(select_cols)
    unpivot_cols  <- rlang::enquo(unpivot_cols)
    group_cols    <- rlang::enquos(group_cols)
    
    # group calc
    group_tbl <- data %>% 
        select(!!!select_cols) %>% 
        mutate(total = "Total") %>%   
        pivot_longer(
            cols      = -!!unpivot_cols,
            names_to  = "name",
            values_to = "campaign"
        ) %>% 
        count(campaign, !!unpivot_cols) %>% 
        mutate(pct = n/sum(n), .by = campaign) 
    
    # total calc
    # total_tbl <- group_tbl %>% 
    #     summarise(n = sum(n), .by = campaign) %>% 
    #     mutate(pct = n/sum(n))
    
    
    # final calc
    if (return == "volume") {
        result_tbl <- group_tbl %>% 
            select(-pct) %>% 
            pivot_wider(names_from = campaign, values_from = n, values_fill = 0) %>% 
            adorn_totals("row", name = "Grand Total")
        
    } else if (return == "percent") {
        result_tbl <- group_tbl %>% 
            select(-n) %>% 
            pivot_wider(names_from = campaign, values_from = pct, values_fill = 0) %>% 
            adorn_totals("row", name = "Grand Total")
        
        if (format) {
            result_tbl <- result_tbl %>% 
                mutate_at(2:4, ~format(scales::percent(., accuracy = 0.01), nsmall = 4))
        } else {
                result_tbl <- result_tbl
            }
    }
    
    return(result_tbl)
}
get_customer_analysis_final_output <-
function(data_leads, data_survey,
                                               discount_rate = 0.10,
                                               gross_margin = 0.75,
                                               output = "customer_metrics") {
    
    cust_metrics_tbl <- get_customer_metrics(data = leads_tbl)
    
    cust_ret_tbl <- get_customer_retention()
    
    clv_tbl <- get_customer_lifetime_value(
        customer_metrics_tbl, discount_rate, gross_margin
    )
    
    if (output == "customer_metrics") {
        ret <- cust_metrics_tbl
    } else if (output == "customer_retention") {
        ret <- cust_ret_tbl
    } else if (output == "clv") {
        ret <- clv_tbl
    }
    
    return(ret)
    
}
