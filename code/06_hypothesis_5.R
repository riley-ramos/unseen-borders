# =============================================================================
# Script:      06_hypothesis_5.R
# Author:      Riley Ramos
# Date:        2025
# Description: Examines the relationship between urban sprawl and PM2.5 air
#              pollution concentrations in the Las Vegas Valley. Tests both
#              directions — PM2.5 as a predictor of sprawl, and sprawl as a
#              predictor of PM2.5 — controlling for race and income.
#              Corresponds to Hypothesis 5 in the thesis.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#                - sheet: "PM25_concentrations"
#
# Output:      Console — regression summaries
#              Console — stargazer LaTeX tables (Tables 12, 13 in thesis)
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)        # portable file paths
library(readxl)      # read Excel workbook sheets
library(tidyverse)   # dplyr, ggplot2, etc.
library(stargazer)   # regression table output (LaTeX)
library(broom)       # tidy model extraction (used in exploratory section)


# --- Global options ----------------------------------------------------------

options(scipen = 999)  # disable scientific notation


# =============================================================================
# Section 1: Load Data
# =============================================================================

sprawl <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "sprawl_index")

demographics <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                           sheet = "demographics")

pm <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                 sheet = "PM25_concentrations")


# =============================================================================
# Section 2: Data Preparation
# =============================================================================
# Joins sprawl with PM2.5 concentrations for use in all regression models.

race_income <- demographics %>%
  mutate(
    perc_poc      = (poc_population / total_population) * 100,
    median_income = median_income / 1000  # scale to thousands for interpretability
  ) %>%
  select(census_tract, perc_poc, median_income) %>%
  inner_join(sprawl, by = "census_tract")

# Join sprawl with PM2.5
sprawl_pm <- sprawl %>%
  inner_join(pm, by = "census_tract")

# Join sprawl + PM2.5 with race and income controls
pm_demogs <- sprawl_pm %>%
  inner_join(race_income %>% select(census_tract, perc_poc, median_income),
             by = "census_tract")


# =============================================================================
# Section 3: OLS Regressions — Thesis Models (Table 12)
# =============================================================================
# Five models with sprawl index as the dependent variable and avg daily PM2.5
# concentration as the primary predictor. Results correspond to Table 12
# in the thesis.

# Model 1: sprawl ~ avg daily PM2.5
m1 <- lm(sprawl_index ~ avg_PM_pred, data = sprawl_pm)
summary(m1)

# Model 2: + % POC
m2 <- lm(sprawl_index ~ avg_PM_pred + perc_poc, data = pm_demogs)
summary(m2)

# Model 3: + median income
m3 <- lm(sprawl_index ~ avg_PM_pred + median_income, data = pm_demogs)
summary(m3)

# Model 4: + % POC + median income
m4 <- lm(sprawl_index ~ avg_PM_pred + perc_poc + median_income, data = pm_demogs)
summary(m4)

# Model 5: + % POC * median income (interaction)
m5 <- lm(sprawl_index ~ avg_PM_pred + (perc_poc * median_income), data = pm_demogs)
summary(m5)


# =============================================================================
# Section 4: OLS Regressions — Thesis Models (Table 13)
# =============================================================================
# Five models with avg daily PM2.5 as the dependent variable and sprawl index
# as the primary predictor (reversed direction). Results correspond to
# Table 13 in the thesis.

# Model 6: avg PM2.5 ~ sprawl
m6 <- lm(avg_PM_pred ~ sprawl_index, data = pm_demogs)
summary(m6)

# Model 7: + % POC
m7 <- lm(avg_PM_pred ~ sprawl_index + perc_poc, data = pm_demogs)
summary(m7)

# Model 8: + median income
m8 <- lm(avg_PM_pred ~ sprawl_index + median_income, data = pm_demogs)
summary(m8)

# Model 9: + % POC + median income
m9 <- lm(avg_PM_pred ~ sprawl_index + perc_poc + median_income, data = pm_demogs)
summary(m9)

# Model 10: + % POC * median income (interaction)
m10 <- lm(avg_PM_pred ~ sprawl_index + (perc_poc * median_income), data = pm_demogs)
summary(m10)


# =============================================================================
# Section 5: Regression Table Output (Tables 12, 13 in thesis)
# =============================================================================

# Table 12: PM2.5 as predictor of sprawl
stargazer(m1, m2, m3, m4, m5,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of PM2.5 Concentrations on Sprawl",
          covariate.labels = c("Average Daily PM2.5 Concentration",
                               "% POC",
                               "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = "Sprawl Index")

# Table 13: Sprawl as predictor of PM2.5
stargazer(m6, m7, m8, m9, m10,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of Sprawl on PM2.5 Concentrations",
          covariate.labels = c("Sprawl Index",
                               "% POC",
                               "Median Income",
                               "% POC x Median Income"),
          dep.var.labels   = "Average Daily PM2.5 Concentration")


# =============================================================================
# Section 6: Exploratory Models (not in thesis)
# =============================================================================
# Models and analyses below were used during analysis but did not appear in
# the final paper. Retained here for transparency and reproducibility.

# --- Yearly sum PM2.5 as predictor (alternative to daily avg) ----------------

result <- lm(sprawl_index ~ avg_PM_pred, data = sprawl_pm)
summary(result)

# Extract tidy model summary and model fit statistics
summary_df  <- tidy(result)
model_stats <- glance(result) %>%
  select(r.squared, adj.r.squared) %>%
  mutate(term = "Model Summary")

final_df <- bind_rows(summary_df, model_stats)

# --- Exploratory scatter plots -----------------------------------------------

# Sprawl vs. avg daily PM2.5
ggplot(sprawl_pm, aes(x = avg_PM_pred, y = sprawl_index)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Avg PM2.5 Concentrations vs Sprawl",
       x     = "Avg Daily PM2.5 Concentration",
       y     = "Sprawl Index")
