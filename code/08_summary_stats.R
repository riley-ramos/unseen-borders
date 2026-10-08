# =============================================================================
# Script:      08_summary_stats.R
# Author:      Riley Ramos
# Date:        2025
# Description: Generates descriptive summary statistics for all key thesis
#              variables. Corresponds to Table 2 (Summary Statistics) in the
#              thesis.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#                - sheet: "mean_travel_time"
#                - sheet: "commute_duration"
#                - sheet: "occupation"
#                - sheet: "occupation_salary"
#                - sheet: "PM25_concentrations"
#
# Output:      Console — summary() and sd() for each variable group
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)        # portable file paths
library(readxl)      # read Excel workbook sheets
library(tidyverse)   # dplyr, etc.


# --- Global options ----------------------------------------------------------

options(scipen = 999)  # disable scientific notation


# =============================================================================
# Section 1: Load Data
# =============================================================================

sprawl <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "sprawl_index")

demographics <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                           sheet = "demographics")

mean_travel <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                          sheet = "mean_travel_time")

commute <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                      sheet = "work_commute_time")

occupation <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                         sheet = "occupation")

salary <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "occupation_salary")

pm <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                 sheet = "PM25_concentrations")


# =============================================================================
# Section 2: Sprawl Index
# =============================================================================

cat("\n--- Sprawl Index ---\n")
print(summary(sprawl$sprawl_index))
cat("SD:", sd(sprawl$sprawl_index, na.rm = TRUE), "\n")


# =============================================================================
# Section 3: Race and Income (from demographics)
# =============================================================================

demographics <- demographics %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .))))

# --- % POC -------------------------------------------------------------------
race <- demographics %>%
  select(census_tract, total_population, white_population, poc_population) %>%
  mutate(
    perc_poc   = (poc_population / total_population) * 100,
    perc_white = (white_population / total_population) * 100
  ) %>%
  inner_join(sprawl, by = "census_tract")

cat("\n--- Race (% POC, % White) ---\n")
print(summary(race[, c("perc_poc", "perc_white")]))
cat("SD perc_poc:", sd(race$perc_poc, na.rm = TRUE), "\n")

# --- Median Income -----------------------------------------------------------
income <- demographics %>%
  select(census_tract, median_income) %>%
  mutate(median_income = median_income / 1000) %>%
  inner_join(sprawl, by = "census_tract")

cat("\n--- Median Income ($1,000s) ---\n")
print(summary(income$median_income))
cat("SD:", sd(income$median_income, na.rm = TRUE), "\n")


# =============================================================================
# Section 4: Mean Travel Time
# =============================================================================

mean_travel <- mean_travel %>%
  rename(avg_travel_time = "avg_work_commute_mins") %>%
  mutate(avg_travel_time = as.numeric(avg_travel_time))

cat("\n--- Mean Travel Time (minutes) ---\n")
print(summary(mean_travel$avg_travel_time))
cat("SD:", sd(mean_travel$avg_travel_time, na.rm = TRUE), "\n")


# =============================================================================
# Section 5: Transportation Mode Breakdown (from commute_duration)
# =============================================================================
# Derives per-tract percentage for each transportation type, then summarises
# across all tracts. Includes the granular breakdown by individual mode.

commute <- commute %>%
  mutate(count = as.numeric(gsub(",", "", count)))

# Total commuters per tract (all methods, Total time interval)
total_commuters <- commute %>%
  filter(transportation_type == "All transportation methods",
         commute_length == "Total") %>%
  select(census_tract, count) %>%
  rename(total = count)

# Per-mode totals (exclude "All transportation methods" aggregate row)
mode_counts <- commute %>%
  filter(transportation_type != "All transportation methods",
         commute_length == "Total") %>%
  select(census_tract, transportation_type, count)

# Join with totals and compute percentage per tract per mode
mode_perc <- mode_counts %>%
  inner_join(total_commuters, by = "census_tract") %>%
  mutate(perc = (count / total) * 100)

# --- Granular summary by transportation type ---------------------------------
cat("\n--- Transportation Mode Breakdown (% of commuters per tract) ---\n")
all_transpo <- mode_perc %>%
  group_by(transportation_type) %>%
  summarise(
    mean = mean(perc, na.rm = TRUE),
    min  = min(perc,  na.rm = TRUE),
    max  = max(perc,  na.rm = TRUE),
    sd   = sd(perc,   na.rm = TRUE),
    .groups = "drop"
  )
print(all_transpo)

# =============================================================================
# Section 6: Occupation (% Management, % Service)
# =============================================================================

occupation <- occupation %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .))))

mgmt_service <- occupation %>%
  mutate(
    mgmt_percent    = (mgmt_pop    / employed_pop) * 100,
    service_percent = (service_pop / employed_pop) * 100
  ) %>%
  select(census_tract, mgmt_percent, service_percent) %>%
  inner_join(sprawl, by = "census_tract")

cat("\n--- Occupation Percentages ---\n")
print(summary(mgmt_service[, c("mgmt_percent", "service_percent")]))
cat("SD mgmt_percent:",    sd(mgmt_service$mgmt_percent,    na.rm = TRUE), "\n")
cat("SD service_percent:", sd(mgmt_service$service_percent, na.rm = TRUE), "\n")


# =============================================================================
# Section 7: Occupation Salary
# =============================================================================

salary <- salary %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .)) / 1000)) %>%
  inner_join(sprawl, by = "census_tract")

cat("\n--- Occupation Salary ($1,000s) ---\n")
print(summary(salary[, c("total_med_salary", "mgmt_med_salary", "service_med_salary")]))
cat("SD avg:",     sd(salary$total_med_salary,     na.rm = TRUE), "\n")
cat("SD mgmt:",    sd(salary$mgmt_med_salary,    na.rm = TRUE), "\n")
cat("SD service:", sd(salary$service_med_salary, na.rm = TRUE), "\n")


# =============================================================================
# Section 8: PM2.5 Air Pollution
# =============================================================================

sprawl_pm <- sprawl %>%
  inner_join(pm, by = "census_tract")

cat("\n--- PM2.5 Concentrations ---\n")
print(summary(sprawl_pm[, c("avg_PM_pred")]))
cat("SD avg_PM_pred:", sd(sprawl_pm$avg_PM_pred, na.rm = TRUE), "\n")
