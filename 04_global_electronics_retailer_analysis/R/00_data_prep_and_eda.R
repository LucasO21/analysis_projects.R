# SALES ANALYSIS SCRIPT ----
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

# 1.2 Source ----
source("../functions/utils_gt_table.R")
source("../functions/utils_data_wrangling.R")
source("../functions/utils_plotting.R")


# *****************************************************************************
# **** ----
# 2.0 DATA IMPORT ----
# *****************************************************************************

# 2.1 Sales Data ----
sales_tbl <- read_csv("../data/raw_data/Sales.csv") %>% 
    clean_names() %>%
    mutate(order_date = as.Date(order_date, format = "%m/%d/%Y")) %>% 
    mutate(delivery_date = as.Date(delivery_date, format = "%m/%d/%Y"))

sales_tbl %>% sapply(function(x) sum(is.na(x)))

# 2.2 Product Data ----
product_tbl <- read_csv("../data/raw_data/Products.csv") %>% 
    clean_names() %>% 
    mutate(unit_price_usd = parse_number(unit_price_usd)) %>% 
    mutate(unit_cost_usd = parse_number(unit_cost_usd))

product_tbl %>% sapply(function(x) sum(is.na(x)))

# 2.3 Customer Data ----
customer_tbl <- read_csv("../data/raw_data/Customers.csv") %>% 
    clean_names() %>% 
    mutate(birthday = as.Date(birthday, format = "%m/%d/%Y"))

customer_tbl %>% sapply(function(x) sum(is.na(x)))

# 2.4 Stores Data ----
stores_tbl <- read_csv("../data/raw_data/Stores.csv") %>% 
    clean_names() %>% 
    mutate(open_date = as.Date(open_date, format = "%m/%d/%Y")) %>% 
    rename(
        store_country = country,
        store_state = state
    )

stores_tbl %>% sapply(function(x) sum(is.na(x)))

# 2.5 Exchange Rates Data ----
exchange_rates_tbl <- read_csv("../data/raw_data/Exchange_Rates.csv") %>% 
    clean_names() %>% 
    mutate(date = as.Date(date, format = "%m/%d/%Y"))

exchange_rates_tbl %>% sapply(function(x) sum(is.na(x)))

# 2.6 Data Audit ----
sales_tbl %>% count(customer_key, sort = TRUE)


# *****************************************************************************
# **** ----
# 3.0 COMBINE DATA ----
# *****************************************************************************

# 3.1 Combine Data ----
combined_sales_tbl <- sales_tbl %>% 
    
    # join product data
    left_join(customer_tbl, by = "customer_key") %>% 
    
    # join product data
    left_join(product_tbl, by = "product_key") %>%
    
    # join store data
    left_join(stores_tbl, by = "store_key") %>% 
    
    # join exchange rates
    # left_join(exchange_rates_tbl, by = c("order_date" = "date")) %>% 
    
    # add sales revenue
    mutate(sales_usd = unit_price_usd * quantity, .after = unit_price_usd) %>%
    
    # add order month
    mutate(order_month = floor_date(order_date, "month"), .before = order_date)

    
combined_sales_tbl %>% glimpse()

combined_sales_tbl %>% count(store_country, sort = TRUE)


# *****************************************************************************
# **** ----
# 4.0 DATA FILTERING (USA) ----
# *****************************************************************************

# 4.1 Filter USA & Online ----
usa_online_sales_temp_tbl <- combined_sales_tbl %>% 
    filter(country == "United States") %>% 
    filter(store_state == "Online") %>% 
    left_join(
        combined_sales_tbl %>% 
            summarise(first_purchase_date = min(order_date), .by = customer_key),
        by = "customer_key"
    ) %>% 
    mutate(
        age_in_days = as.numeric(
            difftime(
                max(combined_sales_tbl$order_date), first_purchase_date, units = "days"
            )
        )
    ) %>% 
    select(-c(country, continent, store_country, store_state, square_meters, open_date, currency_code))

usa_online_sales_temp_tbl %>% glimpse()

# 4.2 Create Repeat Purchase Flag Table ----
repeat_customer_tbl <- usa_online_sales_temp_tbl %>% 
    count(customer_key, sort = TRUE) %>% 
    mutate(repeat_customer = ifelse(n > 1, 1, 0)) %>% 
    rename(unique_purchase_count = n)

# 4.3 Merge Repeat Purchase Flag to Main Table ----
usa_online_sales_tbl <- usa_online_sales_temp_tbl %>% 
    left_join(repeat_customer_tbl, by = "customer_key")

usa_online_sales_tbl %>% glimpse()

# 4.3 Save Data ----
usa_online_sales_tbl %>% write_csv("../data/clean_data/usa_online_sales_tbl.csv")



# *****************************************************************************
# **** ----
# 5.0 EXPLORATORY DATA ANALYSIS ----
# *****************************************************************************

# 5.1 Order Data Date Range ----
usa_online_sales_tbl %>% pull(order_date) %>% range()


# 5.2 Top 10 Sales by Category ----
usa_online_sales_tbl %>% 
    get_top_n_sales_by_category("category") %>% 
    select(-sales_prop_cummulative) %>% 
    get_gt_table(
        title = "Top 10 Sales by Product Categories",
        green_format_column = "sales_usd",
    ) %>% 
    cols_width(columns = "Category" ~ px(300)) %>% 
    cols_label("Sales Usd" = "Sales (USD)") %>% 
    gtsave_extra(filename = "../png/eda/top_10_sales_by_category.png", zoom = 2)


# 5.3 Top 20 Sales by Products & Category ----
usa_online_sales_tbl %>% 
    summarise(sales_usd = sum(sales_usd), .by = c(category, product_name)) %>% 
    arrange(desc(sales_usd)) %>%
    mutate(sales_prop = sales_usd / sum(sales_usd)) %>%
    select(product_name, category, sales_usd, sales_prop) %>%
    head(10) %>%
    get_gt_table(
        title = "Top 20 Sales by Products & Category",
        green_format_column = "sales_usd",
    ) %>% 
    cols_width(columns = "Category" ~ px(200)) %>% 
    cols_label("Sales Usd" = "Sales (USD)") %>% 
    cols_width(columns = "Product Name" ~ px(425)) %>% 
    cols_align(columns = c("Product Name", "Category"), align = "left") %>% 
    gtsave_extra(filename = "../png/eda/top_20_sales_by_products_category.png", zoom = 2)


# 5.4 Top 10 Sales by States ----
usa_online_sales_tbl %>% 
    get_top_n_sales_by_category(state) %>% 
    get_gt_table(
        title = "Top 10 Sales by States",
        green_format_column = "sales_usd"
    ) %>% 
    cols_width(columns = "State" ~ px(300)) %>% 
    cols_label("Sales Usd" = "Sales (USD)") %>% 
    gtsave_extra(filename = "../png/eda/top_10_sales_by_states.png", zoom = 2)

# 5.5 Age in Days Distribution
# usa_online_sales_tbl %>% 
#     ggplot(aes(age_in_days)) +
#     geom_histogram(bins = 30, fill = "grey", color = "black") +
#     scale_x_continuous(labels = scales::comma) +
#     labs(title = "Age in Days Distribution", x = "Age in Days", y = "Count") + 
#     get_custom_ggplot_theme()



# *****************************************************************************
# **** ----
# 6.0 SALES DECLINE DEEP DIVE ----
# *****************************************************************************

# 6.1 Sales Trend Overall (Online) ----
{
    usa_online_sales_tbl %>% 
        summarise(sales_usd = sum(sales_usd), .by = order_month) %>% 
        #print(n = 75) %>% 
        ggplot(aes(order_month, sales_usd)) +
        geom_line(size = 1) +
        geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.7, alpha = 0.8) +
        scale_y_continuous(labels = scales::dollar) +
        scale_x_date(date_breaks = "5 month", date_labels = "%b %Y") +
        theme_bw() +
        labs(
            title = "Sales Trend Overall (Online)",
            x = "Order Month",
            y = "Sales (USD)\n"
        ) +
        get_custom_ggplot_theme()
} %>% 
    ggsave(filename = "../png/eda/sales_trend_overall_online.png", width = 10, height = 6, dpi = 300)


# 6.2 Sales Trend Overall (In-Store) ----
{
    combined_sales_tbl %>% 
        filter(store_state != "Online") %>% 
        summarise(sales_usd = sum(sales_usd), .by = order_month) %>% 
        ggplot(aes(order_month, sales_usd)) +
        geom_line(size = 1) +
        geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.7, alpha = 0.8) +
        scale_y_continuous(labels = scales::dollar) +
        scale_x_date(date_breaks = "5 month", date_labels = "%b %Y") +
        theme_bw() +
        labs(
            title = "Sales Trend Overall (In-Store)",
            x = "Order Month",
            y = "Sales (USD)\n"
        )+
        get_custom_ggplot_theme()
} %>% 
    ggsave(filename = "../png/eda/sales_trend_overall_instore.png", width = 10, height = 6, dpi = 300)


# 6.3 Sales Trend by Product Category ----
{
    usa_online_sales_tbl %>% 
        summarise(sales_usd = sum(sales_usd), .by = c(order_month, category)) %>% 
        ggplot(aes(order_month, sales_usd, color = category)) +
        geom_line(size = 1) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +
        facet_wrap(vars(category), scales = "free_y", ncol = 3) +
        scale_x_date(date_breaks = "18 month", date_labels = "%b %Y") +
        #scale_color_manual(values = scales::hue_pal()(n_distinct(usa_online_sales_tbl$category))) +
        theme_bw() +
        labs(
            title = "Sales Trend by Product Category",
            x = "Order Month",
            y = "Sales (USD)"
        )+
        theme(
            legend.position = "none", 
            strip.text = element_text(size = 12, face = "bold"), 
        ) +
        get_custom_ggplot_theme()
} %>% 
    ggsave(filename = "../png/eda/sales_trend_by_category.png", width = 12, height = 8, dpi = 300)


    
# 6.4 Sales Trend by State ----
{
    usa_online_sales_tbl %>% 
        filter(state %in% c(
            "California", "Texas", "New York", "Florida", "Ohio", 
            "Pennsylvania", "Illinois", "Michigan", "Georgia"
        )) %>%
        summarise(sales_usd = sum(sales_usd), .by = c(order_month, state)) %>%
        ggplot(aes(order_month, sales_usd, color = state)) +
        geom_line(size = 1) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +
        facet_wrap(vars(state), scales = "free_y") +
        theme_bw() +
        labs(
            title = "Sales Trend by Top 6 State",
            x = "Order Month",
            y = "Sales (USD)"
        )+
        theme(
            legend.position = "none", 
            strip.text = element_text(size = 12, face = "bold"), 
        ) +
        get_custom_ggplot_theme()
} %>% 
    ggsave(filename = "../png/eda/sales_trend_by_state.png", width = 12, height = 8, dpi = 300)


# 6.5 Sales Trend by Customer Age ----
{
    usa_online_sales_tbl %>% 
        mutate(customer_age = year(order_date)  - year(birthday)) %>% 
        mutate(age_buckets =  case_when(
            between(customer_age, 0, 20)  ~ "0 - 20 Year",
            between(customer_age, 21, 35) ~ "21 - 35 Years",
            between(customer_age, 36, 50) ~ "36 - 50 Years",
            TRUE ~ "50+ Years"
        )) %>% 
        summarise(sales_usd = sum(sales_usd), .by = c(order_month, age_buckets)) %>%
        ggplot(aes(order_month, sales_usd, color = age_buckets)) +
        geom_line(size = 1) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +
        facet_wrap(vars(age_buckets), scales = "free_y") +
        theme_bw() +
        labs(
            title = "Sales Trend by Customer Age",
            x = "Order Month",
            y = "Sales (USD)"
        )+
        theme(
            legend.position = "none", 
            strip.text = element_text(size = 12, face = "bold") 
        ) +
        get_custom_ggplot_theme()
} %>% 
    ggsave(filename = "../png/eda/sales_trend_by_customer_age.png", width = 12, height = 8, dpi = 300)

    

# 6.6 Sample of Customers Purchase Trend
# customer_ids <- c(1520875, 1360450, 2092033, 1924461, 1345382, 1439502, 1782682, 2084343, 366001)
# 
# usa_online_sales_tbl %>% 
#     filter(customer_key %in% customer_ids) %>% 
#     summarise(sales_usd = sum(sales_usd), .by = c(order_month, customer_key)) %>% 
#     ggplot(aes(order_month, sales_usd, color = as.factor(customer_key))) +
#     geom_line(size = 1) +
#     geom_point(size = 2) +
#     scale_y_continuous(labels = scales::dollar) +
#     facet_wrap(vars(customer_key), scales = "free_y") +
#     theme_bw() +
#     get_custom_ggplot_theme()+
#     labs(
#         title = "Sales Trend by Customer",
#         x = "Order Month",
#         y = "Sales (USD)"
#     ) +
#     theme(
#         legend.position = "none", 
#         strip.text = element_text(size = 12, face = "bold"), 
#     )

# 6.6 Prop of Customers with Repeat Purchases (Overall) ----
repeat_purchasers_tbl <- usa_online_sales_tbl %>% 
    summarise(
        total_customers = n_distinct(customer_key),
        total_sales = sum(sales_usd),
        repeat_customers = n_distinct(customer_key[repeat_customer == 1]),
        total_sales_repeat_customers = sum(sales_usd[repeat_customer == 1])
    ) %>% 
    mutate(repeat_customers_prop = repeat_customers / total_customers) %>%
    mutate(repeat_sales_prop = total_sales_repeat_customers / total_sales)

repeat_purchasers_tbl %>%
    get_gt_table(
        title = "Proportion of Customers with Repeat Purchases"
    ) %>% 
    cols_label(
        "Total Sales Repeat Customers" = "Repeat Customers Total Sales",
        "Repeat Sales Prop" = "Repeat Customers Sales Prop",
    ) %>% 
    cols_width(columns = everything() ~ px(135)) %>% 
    cols_align(columns = everything(), align = "center") %>% 
    gtsave_extra(filename = "../png/eda/repeat_purchasers_table.png")


# 6.7 Count of Repeat Purchase Customers ----
{
    usa_online_sales_tbl %>% 
        count(customer_key, sort = TRUE) %>% 
        filter(n > 1) %>%
        summarise(
            count = n(),
            .by = n
        ) %>% 
        mutate(n = as.factor(n)) %>% 
        ggplot(aes(n, count)) +
        geom_col(width = 0.8)+
        geom_text(aes(label = count), vjust = -0.5) +
        labs(
            title = "Count of Repeat Purchase Customers",
            x = "Number of Repeat Purchases",
            y = "Count"
        ) +
        theme_bw() +
        get_custom_ggplot_theme()
} %>% 
    ggsave(filename = "../png/eda/repeat_purchase_customers_count.png", width = 10, height = 6, dpi = 300)


# 6.8 Sales Revenue Trend by Repeat Customers ----
sales_trend_with_repeat_customers_tbl <- usa_online_sales_tbl %>% 
    #filter(year(order_date) >= 2020) %>%
    summarise(
        total_customers  = n_distinct(customer_key),
        total_revenue    = sum(sales_usd),
        repeat_customers = n_distinct(customer_key[repeat_customer == 1]),
        repeat_revenue   = sum(sales_usd[repeat_customer == 1]),
        .by = order_month
    ) %>% 
    mutate(repeat_revenue_prop = repeat_revenue / total_revenue) %>% 
    mutate(repeat_customers_prop = repeat_customers / total_customers) %>% 
    mutate(period = case_when(
        order_month < as.Date("2018-06-01") ~ "Pre Peak Sales",
        order_month >= as.Date("2018-06-01") & order_month < as.Date("2020-02-01") ~ "Peak Sales",
        TRUE ~ "Post Peak Sales"
    ))

max_revenue <- max(sales_trend_with_repeat_customers_tbl$total_revenue, na.rm = TRUE)

{
    sales_trend_with_repeat_customers_tbl %>%
        ggplot(aes(x = order_month)) +
        geom_line(aes(y = total_revenue, color = "Total Sales"), size = 1) +
        geom_line(aes(y = repeat_revenue, color = "Total Sales (Repeat Customers)"), size = 1) +
        geom_bar(
            aes(y = repeat_customers_prop * max_revenue, fill = "Repeat Customers Prop"), 
            stat = "identity", 
            alpha = 0.5
        ) +
        scale_y_continuous(
            name = "Total Sales (USD)",
            sec.axis = sec_axis(
                ~ . / max_revenue, 
                name = "Repeat Customers (%)",
                labels = scales::percent
            ),
            labels = scales::dollar
        ) +
        scale_color_manual(
            name = "", 
            values = c("Total Sales" = "black", "Total Sales (Repeat Customers)" = "blue")
        ) +
        scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
        scale_fill_manual(name = "", values = c("Repeat Customers Prop" = "grey50")) +
        theme_bw() +
        theme(legend.position = "bottom") +
        labs(
            title = "Sales Revenue Trend by Repeat Customers",
            x = NULL,
            y = "Total Revenue"
        ) +
        get_custom_ggplot_theme()
} %>% 
    ggsave(
        filename = "../png/eda/sales_revenue_trend_repeat_customers.png", 
        width = 10, 
        height = 6, 
        dpi = 300
    )


sales_trend_with_repeat_customers_tbl %>% tail(12)


# 6.9 Total Customers by Month Trend ----
{
    usa_online_sales_tbl %>% 
        summarise(total_customers = n_distinct(customer_key), .by = order_month) %>% 
        mutate(period = case_when(
            order_month < as.Date("2018-06-01") ~ "Pre Peak Sales",
            order_month >= as.Date("2018-06-01") & order_month < as.Date("2020-02-01") ~ "Peak Sales",
            TRUE ~ "Post Peak Sales"
        )) %>% 
        mutate(period = factor(period, levels = c("Pre Peak Sales", "Peak Sales", "Post Peak Sales"))) %>% 
        ggplot(aes(order_month, total_customers, fill = period)) +
        geom_col() +
        geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.7, alpha = 0.7) +
        scale_x_date(date_breaks = "5 month", date_labels = "%b %Y") +
        labs(
            title = "Total Customers by Month Trend",
            x = NULL,
            y = "Total Customers"
        ) +
        theme_bw() +
        get_custom_ggplot_theme()+
        scale_fill_grey() +
        theme(legend.position = "bottom", legend.title = element_blank())
} %>% 
    ggsave(
        filename = "../png/eda/total_customers_by_month_trend.png", 
        width = 10, 
        height = 6, 
        dpi = 300
    )

    
# 6.10 Average Spend per Customer Trend ----
{
    usa_online_sales_tbl %>% 
        summarise(
            total_orders = n_distinct(customer_key[quantity > 0]),
            total_sales = sum(sales_usd),
            .by = order_month
        ) %>% 
        mutate(avg_sales_per_order = total_sales / total_orders) %>% 
        mutate(period = case_when(
            order_month < as.Date("2018-06-01") ~ "Pre Peak Sales",
            order_month >= as.Date("2018-06-01") & order_month < as.Date("2020-02-01") ~ "Peak Sales",
            TRUE ~ "Post Peak Sales"
        )) %>% 
        #tail(12) %>% 
        mutate(period = factor(period, levels = c("Pre Peak Sales", "Peak Sales", "Post Peak Sales"))) %>% 
        ggplot(aes(order_month, avg_sales_per_order, fill = period)) +
        geom_col() +
        geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.7, alpha = 0.8) +
        scale_x_date(date_breaks = "5 month", date_labels = "%b %Y") +
        labs(
            title = "Average Spend per Customer Trend",
            x = NULL,
            y = "Average Spend per Customer\n"
        ) +
        theme_bw() +
        get_custom_ggplot_theme() +
        scale_fill_grey() +
        theme(legend.position = "bottom", legend.title = element_blank())
} %>% 
    ggsave(
        filename = "../png/eda/avg_spend_per_customer_trend.png", 
        width = 10, 
        height = 6, 
        dpi = 300
    )


# 6.11 Summary ----
sales_trend_with_repeat_customers_tbl %>% 
    filter(order_month != as.Date("2020-04-01")) %>% 
    summarise(
        total_customers            = sum(repeat_customers, na.rm = TRUE),
        avg_repeat_customers_prop  = mean(repeat_customers_prop, na.rm = TRUE), 
        total_sales                = sum(total_revenue, na.rm = TRUE),
        .by                        = period
    ) %>% 
    mutate(avg_sales_per_customer = total_sales / total_customers) %>% 
    select(-total_sales) %>%
    get_gt_table(
        title = "Summary Total Sales & Customers by Period",
        green_format_column = "total_customers",
        red_format_column = "avg_repeat_customers_prop",
        blue_format_column = "avg_sales_per_customer"
    ) %>% 
    cols_width(columns = everything() ~ px(150)) %>% 
    tab_source_note("Note: Data excludes April 2020 which appears to be an outlier month.") %>% 
    gtsave_extra(filename = "../png/eda/summary_total_sales_customers_by_period.png")


# # 6.11 Average Delivery Time Trend
# usa_online_sales_tbl %>% 
#     mutate(delivery_time_in_days = as.numeric(difftime(delivery_date, order_date, units = "days"))) %>%
#     summarise(
#         avg_delivery_time = mean(delivery_time_in_days, na.rm = TRUE),
#         .by = order_month
#     ) %>%
#     ggplot(aes(order_month, avg_delivery_time)) +
#     geom_line(size = 1) +
#     scale_x_date(date_breaks = "5 month", date_labels = "%b %Y") +
#     labs(
#         title = "Average Delivery Time Trend",
#         x = NULL,
#         y = "Average Delivery Time (Days)\n"
#     ) +
#     theme_bw() +
#     get_custom_ggplot_theme()


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


# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
