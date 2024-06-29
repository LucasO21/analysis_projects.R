

# GT Table ----
get_gt_table <- function(data, title = NULL, subtitle = NULL, red_format_column,
                         green_format_column, blue_format_column, orange_format_column,
                         format_currency_columns = TRUE, format_percent_columns = TRUE,
                         green_low = "#d9ead3", green_high = "#6aa84f",
                         blue_low = "#c9daf8", blue_high = "#619cff",
                         orange_low = "#fff2cc", orange_high = "#f1c232",
                         red_low = "#f4cccc", red_high = "#cc0000") {
    
    # colors setup
    colors <-  scales::hue_pal(direction = -1)(3)
    
    # column names setup
    data <- data %>% 
        setNames(names(.) %>% str_replace_all("_", " ") %>% str_to_title()) 
    
    # table setup
    t <- data %>% 
        gt() %>% 
        gt_theme_guardian() %>% 
        tab_options(
            heading.title.font.size = px(22),
            column_labels.border.bottom.color = "#9ADDFA"
        ) %>% 
        tab_header(
            title = title,
            subtitle = subtitle
        ) 
    
    
    # function to apply color formatting 
    apply_color_formatting <- function(table, column, low, high) {
        if (!missing(column)) {
            column <- str_replace_all(column, "_", " ") %>% str_to_title()
            table <- table %>% gt_color_rows(!!ensym(column), palette = c(low, high))
        }
        return(table)
    }
    
    # color formatting
    t <- apply_color_formatting(t, green_format_column, low = green_low, high = green_high)
    t <- apply_color_formatting(t, blue_format_column, low = blue_low, high =  blue_high)
    t <- apply_color_formatting(t, orange_format_column, low = orange_low, high = orange_high)
    t <- apply_color_formatting(t, red_format_column, low = red_low, high = red_high)
    
    # currency format
    if (format_currency_columns) {
        
        t <- t %>% fmt_currency(
            columns = c(contains("sales"), ends_with("value")), 
            currency = "USD",
            decimals = 0
        )
    }
    
    # format percent
    if (format_percent_columns) {
        t <- t %>% fmt_percent(
            columns = c(contains("prop")), 
            decimals = 2
        )
    }
    
    # return
    return(t)
    
}

# GT Table with Spanner ---
get_gt_table_with_spanner <- function(gt_table) {
    
    # gt table prep
    gt <- gt_table %>% 
        fmt_percent(columns = c(ends_with("Vs Company")), decimals = 2) %>% 
        tab_spanner(
            label = "Deal Close Rates",
            columns = c("Deals Close Rate", "Deal Close Rate Vs Company")
        ) %>% 
        tab_spanner(
            label = "Average Deal Values",
            columns = c("Avg Deal Value", "Avg Deal Value Vs Company")
        ) %>%
        tab_spanner(
            label = "Deal Event Values",
            columns = c("Deal Event Value", "Deal Event Value Vs Company")
        ) %>% 
        gt_add_divider(columns = "Deal Close Rate Vs Company", sides = "right") %>% 
        gt_add_divider(columns = "Deals Close Rate", sides = "left") %>% 
        gt_add_divider(columns = "Avg Deal Value Vs Company", sides = "right") %>% 
        cols_label(
            "Deal Close Rate Vs Company" = "VS Company",
            "Avg Deal Value Vs Company" = "VS Company",
            "Deal Event Value Vs Company" = "VS Company"
        ) %>% 
        cols_width(everything() ~ px(150)) %>% 
        cols_width(
            columns = c(ends_with("Vs Company"), ends_with("Total"), ends_with("Closed")) 
            ~ px(130)
        )
    
    # return
    return(gt)
}


# GT Table Settings for "VS Company" Tables ----
get_gt_table_vs_company <- function(gt_table, table_font_size = px(14),
                                    segment_name = "employee_size") {
    
    gt <- gt_table %>% 
        tab_options(table.font.size = table_font_size) %>% 
            cols_width(everything() ~ px(100)) %>%  
            cols_width(
                columns = c("Deals Total", "Deals Close Rate", "Avg Deal Value") ~ px(90)
            ) %>%
            cols_label(
                `Total Close Value` = "Total Close $",
                `Deals Close Rate` = "Close Rate",
                `Avg Deal Value` = "Avg Deal $",
                `Deal Event Value` = "Deal Event $"
            )
    if (segment_name == "employee_size") {
        gt <- gt %>% cols_label(`Employee Size` = "Emp Size")
    } else if (segment_name == "regional_office") {
        gt <- gt %>% cols_label(`Regional Office` = "Office")
    }
    
    gt <- gt %>%
            tab_source_note("Total Close $ = Total Value of Closed Deals") %>% 
            tab_source_note("Close Rate = Deal Close Rate [Deals Closed / Total Deals]") %>%
            tab_source_note("Avg Deal $ = Average Deal Value [Total Deal Value / Closed Deals]") %>%
            tab_source_note("Deal Event $ = Deal Event Value [Close Rate * Avg Deal Value]")
    
    return(gt)
}


get_gt_table_sales_agents_by_manager_custom <- function(gt_table) {
    
    gt <- gt_table %>% 
        fmt_number(columns = c("Avg Time To Close"), decimals = 2) %>%
        cols_align(align = "left", columns = c("Manager", "Regional Office")) %>% 
        tab_options(table.font.size = 15) %>% 
        cols_label(
            `Regional Office` = "Office",
        ) %>% 
        cols_width(columns = c("Sales Agent", "Manager") ~ px(160)) %>% 
        cols_width(columns = "Regional Office" ~ px(70))
    
    return(gt)
}





