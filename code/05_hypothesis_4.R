# =============================================================================
# Script:      05_hypothesis_4.R
# Author:      Riley Ramos
# Date:        2025
# Description: Examines the relationship between urban sprawl and employment
#              composition in the Las Vegas Valley. Analyzes the percentage
#              of residents working in management vs. service occupations and
#              their respective salaries as predictors and outcomes of sprawl,
#              controlling for race and income.
#              Corresponds to Hypothesis 4 in the thesis.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#                - sheet: "occupation"
#                - sheet: "occupation_salary"
#
# Output:      Console — regression summaries
#              Console — stargazer LaTeX tables (Tables 9, 10, 11 in thesis)
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)        # portable file paths
library(readxl)      # read Excel workbook sheets
library(tidyverse)   # dplyr, ggplot2, etc.
library(car)         # vif()
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

occupation <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                         sheet = "occupation")

salary <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "occupation_salary") 


# =============================================================================
# Section 2: Rebuild Race + Income Dataset
# =============================================================================
# Re-derives the combined race and income dataset from the cleaned sheets,
# consistent with previous hypothesis scripts.

race_income <- demographics %>%
  mutate(
    perc_poc      = (poc_population / total_population) * 100,
    median_income = median_income / 1000  # scale to thousands for interpretability
  ) %>%
  select(census_tract, perc_poc, median_income) %>%
  inner_join(sprawl, by = "census_tract")


# =============================================================================
# Section 3: Occupation Data Preparation
# =============================================================================
# Converts occupation counts to percentages of total employed population.

occupation_perc <- occupation %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(
    mgmt_percent    = (mgmt_pop    / employed_pop) * 100,
    service_percent = (service_pop / employed_pop) * 100
  ) %>%
  select(census_tract, employed_pop, mgmt_percent, service_percent)


# =============================================================================
# Section 4: Salary Data Preparation
# =============================================================================
# Scales salary columns to thousands of dollars for interpretability of
# regression coefficients. Joins with sprawl index.

salary_sprawl <- salary %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(across(-census_tract, ~ . / 1000)) %>%  # scale all to $1,000s
  inner_join(sprawl, by = "census_tract")

# Extract management and service salary columns
mgmt_salary <- salary_sprawl %>%
  select(census_tract, sprawl_index, mgmt_med_salary)

service_salary <- salary_sprawl %>%
  select(census_tract, sprawl_index, service_med_salary)


# =============================================================================
# Section 5: Build Combined Analysis Dataset
# =============================================================================
# Joins occupation percentages, salary, sprawl, and race/income into one
# dataset for use in all thesis regression models.

mgmt_service_demogs <- occupation_perc %>%
  inner_join(mgmt_salary,   by = "census_tract") %>%
  inner_join(service_salary %>% select(-sprawl_index), by = "census_tract") %>%
  inner_join(race_income,   by = c("census_tract", "sprawl_index"))


# =============================================================================
# Section 6: OLS Regressions — Thesis Models (Tables 9, 10, 11)
# =============================================================================

# --- Table 9: Workforce size as driver of sprawl ----------------------------
# Sprawl as dependent variable; occupation % and salary as predictors.

# Model 1: % management jobs ~ sprawl
m1 <- lm(sprawl_index ~ mgmt_percent, data = mgmt_service_demogs)
summary(m1)

# Model 2: management salary ~ sprawl
m2 <- lm(sprawl_index ~ mgmt_med_salary, data = mgmt_service_demogs)
summary(m2)

# Model 3: % service jobs ~ sprawl
m3 <- lm(sprawl_index ~ service_percent, data = mgmt_service_demogs)
summary(m3)

# Model 4: service salary ~ sprawl
m4 <- lm(sprawl_index ~ service_med_salary, data = mgmt_service_demogs)
summary(m4)

# --- Table 10: Sprawl as driver of management workforce size -----------------
# Management % as dependent variable.

# Model 1: mgmt_percent ~ sprawl
m0a <- lm(mgmt_percent ~ sprawl_index, data = mgmt_service_demogs)
summary(m0a)

# Model 2: + % POC
m1a <- lm(mgmt_percent ~ sprawl_index + perc_poc, data = mgmt_service_demogs)
summary(m1a)

# Model 3: + median income
m2a <- lm(mgmt_percent ~ sprawl_index + median_income, data = mgmt_service_demogs)
summary(m2a)

# Model 4: + % POC + median income
m4a <- lm(mgmt_percent ~ sprawl_index + perc_poc + median_income,
           data = mgmt_service_demogs)
summary(m4a)

# --- Table 11: Sprawl as driver of service workforce size --------------------
# Service % as dependent variable.

# Model 1: service_percent ~ sprawl
m0c <- lm(service_percent ~ sprawl_index, data = mgmt_service_demogs)
summary(m0c)

# Model 2: + % POC
m1c <- lm(service_percent ~ sprawl_index + perc_poc, data = mgmt_service_demogs)
summary(m1c)

# Model 3: + median income
m2c <- lm(service_percent ~ sprawl_index + median_income, data = mgmt_service_demogs)
summary(m2c)

# Model 4: + % POC + median income
m4c <- lm(service_percent ~ sprawl_index + perc_poc + median_income,
           data = mgmt_service_demogs)
summary(m4c)


# =============================================================================
# Section 7: Regression Table Output (Tables 9, 10, 11 in thesis)
# =============================================================================

# Table 9: Workforce size as driver of sprawl
stargazer(m1, m2, m3, m4,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of Workforce Size of Management and Service Occupations on Sprawl",
          covariate.labels = c("% in Management Jobs", "Average Management Salary",
                               "% in Service Jobs",    "Average Service Salary"),
          dep.var.labels   = "Sprawl Index")

# Table 10: Sprawl as driver of management workforce
stargazer(m0a, m1a, m2a, m4a,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of Sprawl on Management Workforce",
          covariate.labels = c("Sprawl Index", "% POC", "Median Income"),
          dep.var.labels   = "% in Management Jobs")

# Table 11: Sprawl as driver of service workforce
stargazer(m0c, m1c, m2c, m4c,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of Sprawl on Service Workforce",
          covariate.labels = c("Sprawl Index", "% POC", "Median Income"),
          dep.var.labels   = "% in Service Jobs")


# =============================================================================
# Section 8: Exploratory Models (not in thesis)
# =============================================================================
# Models and analyses below were used during analysis but did not appear in
# the final paper. Retained here for transparency and reproducibility.

# --- Hours worked analysis ---------------------------------------------------
hours_worked <- read_excel(here("data", "processed", "exploratory_data.xlsx"),
                           sheet = "other_work") 

hours_joined <- left_join(hours_worked, race_income, by = "census_tract")

model_hours <- lm(sprawl_index ~ mean_hours + median_income, data = hours_joined)
summary(model_hours)

anova_result <- aov(sprawl_index ~ avg_status, data = hours_joined)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

ggplot(hours_joined, aes(x = mean_hours, y = sprawl_index)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ splines::ns(x, df = 2),
              color = "green", se = FALSE) +
  labs(title = "Sprawl Index vs. Mean Hours Worked",
       x     = "Mean Hours Worked per Census Tract",
       y     = "Sprawl Index")

model_spline <- lm(sprawl_index ~ splines::ns(mean_hours, df = 2), data = hours_joined)
summary(model_spline)

# --- Occupation diversity index (Shannon entropy) ----------------------------
# Requires the vegan package: install.packages("vegan")
# occupation_diversity <- occupation %>%
#   rowwise() %>%
#   mutate(occupation_diversity =
#            vegan::diversity(c_across(c("mgmt_pop", "service_pop",
#                                        "sales_office_pop", "construction_pop",
#                                        "production_pop")), index = "shannon")) %>%
#   ungroup() %>%
#   select(census_tract, occupation_diversity)

# --- Average salary regressions ----------------------------------------------
salary_demogs <- inner_join(salary_sprawl, race_income, by = c("census_tract", "sprawl_index")) %>%
  mutate(perc_poc = perc_poc * 100)

m9  <- lm(total_med_salary ~ sprawl_index, data = salary_demogs)
m10 <- lm(total_med_salary ~ sprawl_index + perc_poc, data = salary_demogs)
m11 <- lm(total_med_salary ~ sprawl_index + median_income, data = salary_demogs)
m12 <- lm(total_med_salary ~ sprawl_index + perc_poc + median_income, data = salary_demogs)
m13 <- lm(total_med_salary ~ sprawl_index + (perc_poc * median_income), data = salary_demogs)

stargazer(m9, m10, m11, m12, m13,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          covariate.labels = c("Sprawl Index", "% POC", "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = "Average Salary")

m14 <- lm(sprawl_index ~ total_med_salary, data = salary_demogs)
m15 <- lm(sprawl_index ~ total_med_salary + perc_poc, data = salary_demogs)
m16 <- lm(sprawl_index ~ total_med_salary + median_income, data = salary_demogs)
m17 <- lm(sprawl_index ~ total_med_salary + perc_poc + median_income, data = salary_demogs)
m18 <- lm(sprawl_index ~ total_med_salary + (perc_poc * median_income), data = salary_demogs)

stargazer(m14, m15, m16, m17, m18,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          covariate.labels = c("Average Salary", "% POC", "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = "Sprawl Index")

# --- Reversed direction models (sprawl as predictor) -------------------------
m5 <- lm(mgmt_percent    ~ sprawl_index, data = mgmt_service_demogs)
m6 <- lm(mgmt_med_salary     ~ sprawl_index, data = mgmt_service_demogs)
m7 <- lm(service_percent ~ sprawl_index, data = mgmt_service_demogs)
m8 <- lm(service_med_salary  ~ sprawl_index, data = mgmt_service_demogs)

stargazer(m5, m6, m7, m8,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          covariate.labels = "Sprawl Index",
          dep.var.labels   = c("% in Management Jobs", "Average Management Salary",
                               "% in Service Jobs",    "Average Service Salary"))

# --- Salary + race/income interaction models ---------------------------------
m3a <- lm(mgmt_percent    ~ sprawl_index + (perc_poc * median_income),
           data = mgmt_service_demogs)
m3c <- lm(service_percent ~ sprawl_index + (perc_poc * median_income),
           data = mgmt_service_demogs)
m0b <- lm(mgmt_med_salary     ~ sprawl_index, data = mgmt_service_demogs)
m1b <- lm(mgmt_med_salary     ~ sprawl_index + perc_poc, data = mgmt_service_demogs)
m0d <- lm(service_med_salary  ~ sprawl_index, data = mgmt_service_demogs)
m1d <- lm(service_med_salary  ~ sprawl_index + perc_poc, data = mgmt_service_demogs)

stargazer(m3a, m3c, m1b, m1d,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          covariate.labels = c("Sprawl Index", "% POC", "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = c("% in Management Jobs", "% in Service Jobs",
                               "Avg Management Salary", "Avg Service Salary"))

# --- Correlation matrix and VIF on salary variables --------------------------
vars      <- salary_demogs[, c("total_med_salary", "perc_poc", "median_income")]
cor_matrix <- cor(vars, use = "complete.obs")
print(cor_matrix)

model_vif  <- lm(sprawl_index ~ total_med_salary + perc_poc + median_income, 
                 data = salary_demogs)
vif_values <- vif(model_vif)
print(vif_values)

# --- Scatter plots -----------------------------------------------------------

ggplot(mgmt_service_demogs, aes(x = mgmt_percent, y = sprawl_index)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ splines::ns(x, df = 2),
              color = "green", se = FALSE) +
  labs(title = "Sprawl Index vs. % Management Jobs",
       x = "Percent Working in Management Jobs", y = "Sprawl Index")

ggplot(mgmt_service_demogs, aes(x = mgmt_med_salary, y = sprawl_index)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ splines::ns(x, df = 2),
              color = "green", se = FALSE) +
  labs(title = "Sprawl Index vs. Average Management Salary",
       x = "Average Management Salary ($1,000s)", y = "Sprawl Index")

ggplot(mgmt_service_demogs, aes(x = service_percent, y = sprawl_index)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ splines::ns(x, df = 2),
              color = "green", se = FALSE) +
  labs(title = "Sprawl Index vs. % Service Jobs",
       x = "Percent Working in Service Jobs", y = "Sprawl Index")

ggplot(mgmt_service_demogs, aes(x = service_med_salary, y = sprawl_index)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ splines::ns(x, df = 2),
              color = "green", se = FALSE) +
  labs(title = "Sprawl Index vs. Average Service Salary",
       x = "Average Service Salary ($1,000s)", y = "Sprawl Index")

ggplot(salary_demogs, aes(x = perc_poc, y = total_med_salary)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ splines::ns(x, df = 2),
              color = "green", se = FALSE) +
  labs(title = "Percent POC vs. Average Salary",
       x = "Percent POC", y = "Average Salary ($1,000s)")
