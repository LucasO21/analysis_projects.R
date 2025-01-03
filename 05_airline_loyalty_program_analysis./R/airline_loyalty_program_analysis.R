# AIRLINE LOYALTY PROGRAM ANALYSIS ----
# *** ----

# *****************************************************************************
# ANALYSIS QUESTIONS ----
# *****************************************************************************

# *****************************************************************************
# 1.0 SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("05_airline_loyalty_program_analysis", "R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)


# *****************************************************************************
# **** ----
# 2.0 DATA IMPORT ----
# *****************************************************************************

# * 2.1 Load Data ----
customer_flight_tbl <- read_csv("../data/Customer Flight Activity.csv") |> 
    as_tibble() |> 
    clean_names() |> 
    mutate(date = make_date(year, month), .after = loyalty_number)


# * 2.2 Customer Loyalty History ----
customer_loyalty_tbl <- read_csv("../data/Customer Loyalty History.csv") |> 
    as_tibble() |> 
    clean_names() |> 
    mutate(
        enrollment_date = make_date(enrollment_year, enrollment_month)
    ) |> 
    mutate(
        cancellation_date = make_date(cancellation_year, cancellation_month)
    )
    
customer_loyalty_tbl |> glimpse()


# * Calender ----
calender_tbl <- read_csv("../data/Calendar.csv") |> 
    as_tibble() |> 
    clean_names()


customer_flight_tbl |> 
    summarise(
        total_flights = sum(total_flights),
        .by = c(date)
    ) |> 
    plot_time_series(
        .date = date,
        .value = total_flights,
    )


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
