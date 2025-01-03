# SCRIPT TOPIC:
# SCRIPT NOTES:
# *** ----

# *****************************************************************************
# NOTES ----
# *****************************************************************************
#' Key Questions to Answer:
#' 1. What are the most efficient factory to customer shipping routes?
#' 2. What are the least efficient factory to customer shipping routes?
#' 3. What product lines have the best profit margins?
#' 4. Which product lines should be moved to a different factory to optimize shipping routes?
#' 
#' Dimensions: 
#' 1. Geo Based:Country, state, city, zip code, region, division.
#' 2. Product Based: Division, Product ID/Name.
#' 3. Factory Based: Factory ID/Name.
#' 4. Time Based: Order Date. 
#' 
#' Measure:
#' 1. Given: Total Sales, Total Units, Gross Profit, Total Cost.
#' 2. Derived: Net Profit, Profit Margin, 
#' 3. Efficiency Metrics: Cost Per Mile, Net Profit Per Mile, Sales Cost Ratio Per Mile, Composite Efficiency.
#' 
#' EDA:
#' 1. Metrics by Geo Based Dimensions.
#' 2. Establish baseline metrics:
#'      - Overall Baseline: Mean/median of entire data.
#'      - Geo Based Baseline: Mean/median by geo based dimensions.
#'      - Product Based Baseline: Mean/median by product based dimensions.
#'      - Factory Based Baseline: Mean/median by factory based dimensions.
#'      - Product Line Based Baseline: Mean/median by product line based dimensions.
#'      - Hybrid Baseline: Mean/median by hybrid dimensions.
#' 2. Compare metrics of specific dimensions to baselines. 
#' 
#' Shipping Route Efficiency:
#' 1. Calculate shipping distance between factory and customer.
#' 2. Calculate the sales by mile, cost by mile and net profit by mile.
#' 3. See what factories are most efficient and least efficient.
#' 4. Scenario Analysis: What if we move product lines to different factories?

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("05_candy_distributor_analysis", "R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

# Sales Data ----
sales_raw_tbl <- read_csv("../data/data_raw/Candy_Sales.csv") |> 
    as_tibble() |> 
    clean_names()

sales_raw_tbl |> glimpse()
sales_raw_tbl |> head() |> View()
sales_raw_tbl |> sapply(function(x) sum(is.na(x)))


# Products Data ----
products_raw_tbl <- read_csv("../data/data_raw/Candy_Products.csv") |> 
    as_tibble() |> 
    clean_names()

products_raw_tbl |> glimpse()
products_raw_tbl |> head()
products_raw_tbl |> sapply(function(x) sum(is.na(x)))


# Factories Data ----
factories_raw_tbl <- read_csv("../data/data_raw/Candy_Factories.csv") |> 
    as_tibble() |> 
    clean_names()

factories_raw_tbl |> glimpse()
factories_raw_tbl |> head()
factories_raw_tbl |> sapply(function(x) sum(is.na(x)))


# Targets Data ----
targets_raw_tbl <- read_csv("../data/data_raw/Candy_Targets.csv") |> 
    as_tibble() |> 
    clean_names()

targets_raw_tbl |> glimpse()
targets_raw_tbl |> head()


# Zip Codes Data ----
zip_codes_raw_tbl <- read_csv("../data/data_raw/uszips.csv") |> 
    as_tibble() |> 
    clean_names()

zip_codes_raw_tbl |> glimpse()
zip_codes_raw_tbl |> head()
zip_codes_raw_tbl |> sapply(function(x) sum(is.na(x)))


# *****************************************************************************
# **** ----
# DATA AUDITS SECTION ----
# *****************************************************************************

zip_codes_raw_tbl |> 
    filter(zip == 94122)

zip_codes_raw_tbl |>
    filter(lng == -81.1 & lat == 32.1)

point1 <- c(-122.0, 32.9)
point2 <- c(-122.0, 37.8)
distance <- geosphere::distHaversine(point1, point2)
distance_km <- distance / 1000
distance_miles <- distance_km * 0.621371


# *****************************************************************************
# **** ----
# COMBINE DATA ----
# *****************************************************************************

# * Combine Data ----
sales_combined_tbl <- sales_raw_tbl |> 
    filter(nchar(postal_code) == 5) |>
    rename(
        total_sales = sales,
        total_cost = cost,
        total_units = units
    ) |> 
    mutate(net_profit = gross_profit - total_cost, .after = total_cost) |> 
    mutate(profit_margin = net_profit / total_sales, .after = net_profit) |>
    left_join(
        products_raw_tbl |> select(factory, product_id),
        by = "product_id"
    ) |> 
    left_join(
        factories_raw_tbl,
        by = "factory"
    ) |> 
    rename(
        factory_lng = longitude,
        factory_lat = latitude
    ) |> 
    left_join(
        zip_codes_raw_tbl |> select(zip, lng, lat),
        by = c("postal_code" = "zip")
    ) |>
    rename(
        customer_lng = lng,
        customer_lat = lat
    ) |> 
    mutate(
        shipping_km = geosphere::distHaversine(
            cbind(factory_lng, factory_lat),
            cbind(customer_lng, customer_lat)
        ) / 1000
    ) |> 
    mutate(
        shipping_miles = shipping_km * 0.621371
    ) |> 
    
    # efficiency metrics
    mutate(
        sales_per_mile = total_sales / shipping_miles,
        cost_per_mile = total_cost / shipping_miles,
        net_profit_per_mile = net_profit / shipping_miles,
        sales_cost_ratio_per_mile = total_sales / (total_cost * shipping_miles),
        composite_efficiency = net_profit_per_mile / cost_per_mile
    )

    

sales_combined_tbl |> head() |> View()
sales_combined_tbl |> sapply(function(x) sum(is.na(x)))

sales_combined_tbl |> filter(is.na(sales_cost_ratio_per_mile)) |> View()



# *****************************************************************************
# **** ----
# EXPLANATORY DATA ANALYSIS ----
# *****************************************************************************





# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
