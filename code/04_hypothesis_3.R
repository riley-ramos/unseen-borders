# =============================================================================
# Script:      04_hypothesis_3.R
# Author:      Riley Ramos
# Date:        2025
# Description: Examines the relationship between urban sprawl and private
#              vehicle usage in the Las Vegas Valley. Computes the percentage
#              of residents commuting by car vs. other means per census tract,
#              and runs OLS regression models with race and income controls.
#              Corresponds to Hypothesis 3 in the thesis.
#
#              Note: The t-test comparing sprawl averages between car-dominant
#              and non-car-dominant census tracts (Table 7) is run in
#              07_combined_analysis.R.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#                - sheet: "commute_duration"
#
# Output:      Console — regression summaries
#              Console — stargazer LaTeX table (Table 8 in thesis)
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)        # portable file paths
library(readxl)      # read Excel workbook sheets
library(tidyverse)   # dplyr, ggplot2, etc.
library(stargazer)   # regression table output (LaTeX)


# --- Global options ----------------------------------------------------------

options(scipen = 999)  # disable scientific notation


# =============================================================================
# Section 1: Load Data
# =============================================================================

sprawl <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "sprawl_index")

demographics <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                           sheet = "demographics")

commute <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                      sheet = "work_commute_time")


# =============================================================================
# Section 2: Rebuild Race + Income Dataset
# =============================================================================
# Re-derives the combined race and income dataset from the cleaned sheets,
# consistent with 02_hypothesis_1.R and 03_hypothesis_2.R.

race_income <- demographics %>%
  mutate(
    perc_poc      = (poc_population / total_population) * 100,
    median_income = median_income / 1000  # scale to thousands for interpretability
  ) %>%
  select(census_tract, perc_poc, median_income) %>%
  inner_join(sprawl, by = "census_tract")


# =============================================================================
# Section 3: Transportation Mode Data Preparation
# =============================================================================
# Aggregates commute counts into two groups — car and other — per census
# tract, then calculates the percentage of residents using each mode.
# "Car" includes all car, truck, or van entries; "Other" includes all
# remaining transportation types (public transit, walked, taxicab, etc.).

# Total commuters per tract (denominator for percentage calculation)
total_commuters <- commute %>%
  filter(commute_length == "Total",
         transportation_type == "All transportation methods") %>%
  select(census_tract, count) %>%
  rename(total_pop = count)

# Aggregate into car vs. other, summing across all time intervals
transpo_grouped <- commute %>%
  filter(commute_length == "Total",
         transportation_type != "All transportation methods") %>%
  mutate(transportation_type = ifelse(
    grepl("Car", transportation_type, ignore.case = TRUE), "car", "other"
  )) %>%
  rename(total_count = count) %>%
  group_by(census_tract, transportation_type) %>%
  summarise(total_count = sum(total_count, na.rm = TRUE), .groups = "drop")

# Calculate percentage for each mode
transpo_perc <- transpo_grouped %>%
  inner_join(total_commuters, by = "census_tract") %>%
  mutate(perc = (total_count / total_pop) * 100) %>%
  select(census_tract, transportation_type, perc)

# Pivot to wide format: one column per transportation mode
transpo_wide <- transpo_perc %>%
  pivot_wider(
    names_from  = transportation_type,
    values_from = perc,
    names_glue  = "{transportation_type}_perc"
  )

# Join with sprawl index and race/income
transpo_race_income <- sprawl %>%
  inner_join(transpo_wide,   by = "census_tract") %>%
  inner_join(race_income %>% select(census_tract, perc_poc, median_income),
             by = "census_tract")


# =============================================================================
# Section 4: OLS Regressions — Thesis Models (Table 8)
# =============================================================================
# Four models with percentage of car users as the dependent variable.
# Results correspond to Table 8 in the thesis.

# Model 1: Effect of sprawl on % car users
m11 <- lm(car_perc ~ sprawl_index, data = transpo_race_income)
summary(m11)

# Model 2: Sprawl + % POC
m12 <- lm(car_perc ~ sprawl_index + perc_poc, data = transpo_race_income)
summary(m12)

# Model 3: Sprawl + median income
m13 <- lm(car_perc ~ sprawl_index + median_income, data = transpo_race_income)
summary(m13)

# Model 4: Sprawl + % POC + median income
m14 <- lm(car_perc ~ sprawl_index + perc_poc + median_income,
           data = transpo_race_income)
summary(m14)


# =============================================================================
# Section 5: Regression Table Output (Table 8 in thesis)
# =============================================================================

stargazer(m11, m12, m13, m14,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of Transportation Use, Race, Income, and Interaction Effect on Sprawl",
          covariate.labels = c("Sprawl Index", "% POC", "Median Income"),
          dep.var.labels   = "% Car Users")


# =============================================================================
# Section 6: Exploratory Models (not in thesis)
# =============================================================================
# Models below were used during analysis but did not appear in the final paper.
# Retained here for transparency and reproducibility.

# --- OLS: sprawl as dependent, car_perc as predictor (reversed direction) ----

m1 <- lm(sprawl_index ~ car_perc, data = transpo_race_income)
summary(m1)

m2 <- lm(sprawl_index ~ car_perc + perc_poc, data = transpo_race_income)
summary(m2)

m3 <- lm(sprawl_index ~ car_perc + median_income, data = transpo_race_income)
summary(m3)

m4 <- lm(sprawl_index ~ car_perc + perc_poc + median_income,
          data = transpo_race_income)
summary(m4)

m5 <- lm(sprawl_index ~ car_perc + (perc_poc * median_income),
          data = transpo_race_income)
summary(m5)

stargazer(m1, m2, m3, m4, m5,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          covariate.labels = c("% Car Users", "% POC", "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = "Sprawl Index")

# --- OLS: sprawl as dependent, other_perc as predictor ----------------------

m6  <- lm(sprawl_index ~ other_perc, data = transpo_race_income)
summary(m6)

m7  <- lm(sprawl_index ~ other_perc + perc_poc, data = transpo_race_income)
summary(m7)

m8  <- lm(sprawl_index ~ other_perc + median_income, data = transpo_race_income)
summary(m8)

m9  <- lm(sprawl_index ~ other_perc + perc_poc + median_income,
           data = transpo_race_income)
summary(m9)

m10 <- lm(sprawl_index ~ other_perc + (perc_poc * median_income),
           data = transpo_race_income)
summary(m10)

# --- Interaction model -------------------------------------------------------

m15 <- lm(car_perc ~ sprawl_index + (perc_poc * median_income),
           data = transpo_race_income)
summary(m15)

# --- car_perc ~ race/income only (no sprawl) ---------------------------------

m16 <- lm(car_perc ~ perc_poc, data = transpo_race_income)
summary(m16)

m17 <- lm(car_perc ~ median_income, data = transpo_race_income)
summary(m17)

m18 <- lm(car_perc ~ perc_poc + median_income, data = transpo_race_income)
summary(m18)

m19 <- lm(car_perc ~ perc_poc * median_income, data = transpo_race_income)
summary(m19)

stargazer(m16, m17, m18, m19,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          covariate.labels = c("% POC", "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = "% Car Users")
