# ATTRIBUTION MODELING ----
# DATA GENERATION SCRIPT ----
# *** ----


# *************************************************************************
# SETUP ----
# *************************************************************************
# 1.1 Set Working Dir ----
setwd(here::here("03_attribution_modeling", "R"))

# Libraries ----
library(tidyverse)
library(janitor)
library(magrittr)


# *************************************************************************
# DATA GENERATOR ----
# *************************************************************************
# Set seed for reproducibility
set.seed(123)

# Create a synthetic dataset for attribution modeling
dataset <- tibble(
    donor_id = 1:10000,
    age = sample(18:70, 10000, replace = TRUE),
    sex = sample(c("Male", "Female"), 10000, replace = TRUE),
    education_level = sample(c("High School", "College", "Graduate"), 10000, replace = TRUE),
    employment_status = sample(c("Employed", "Unemployed", "Retired"), 10000, replace = TRUE),
    marital_status = sample(c("Single", "Married", "Divorced", "Widowed"), 10000, replace = TRUE),
    previous_volunteer = sample(c(TRUE, FALSE), 10000, replace = TRUE),
    conversion_path = NA_character_,
    state = sample(state.abb, 10000, replace = TRUE),
    giving_history = sample(c("Recurring", "One-time", "Major Gift"), 10000, replace = TRUE)
)

# Define conversion channels
conversion_channels <- c(
    "website", "email campaigns", "social media", "events",
    "direct mail", "peer referrals", "corporate giving programs",
    "partnerships/collaborations", "online advertising"
)

# Generate conversion paths for each donor
dataset <- dataset %>%
    mutate(
        conversion_path = case_when(
            donor_id %% 3 == 0 ~ sample(conversion_channels, 1),
            donor_id %% 5 == 0 ~ paste(sample(conversion_channels, 2), collapse = " > "),
            TRUE ~ sample(conversion_channels, 3) %>% str_c(collapse = " > ")
        )
    )

# Add additional non-profit database characteristics
dataset$donation_amount <- round(runif(10000, min = 10, max = 500), 2)
dataset$donation_date <- sample(
    seq(
        as.Date("2022-01-01"), 
        as.Date("2022-12-31"), 
        by = "day"
    ), 
    10000, 
    replace = TRUE
)
dataset$donation_category <- sample(
    c("Education", 
      "Healthcare", 
      "Environment", 
      "Poverty", 
      "Arts"
    ), 10000, 
    replace = TRUE
)

# Display the first few rows of the dataset
head(dataset)



# *************************************************************************
# EXPLORATORY DATA ANALYSIS ----
# *************************************************************************



# *************************************************************************

# *************************************************************************
# *************************************************************************

# *************************************************************************
# *************************************************************************

# *************************************************************************
# *************************************************************************

# *************************************************************************