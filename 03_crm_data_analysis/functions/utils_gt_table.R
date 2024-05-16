

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
        tab_options(heading.title.font.size = px(24)) %>% 
        tab_header(
            title = title,
            subtitle = subtitle
        ) %>% 
        gt_theme_guardian() 
    
    
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
            columns = c(ends_with("revenue"), ends_with("value")), 
            currency = "USD",
            decimals = 0
        )
    }
    
    # format percent
    if (format_percent_columns) {
        t <- t %>% fmt_percent(
            columns = c(ends_with("pct"), ends_with("rate")), 
            decimals = 2
        )
    }
    
    # fix width and alignment
    t <- t %>%
        cols_width(everything() ~ px(120)) %>% 
        cols_align(align = "center", columns = everything()) %>% 
        cols_width(names(data)[1] ~ px(150)) %>% 
        cols_align(align = "left", columns = c(names(data)[1]))
    
    # return table
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







