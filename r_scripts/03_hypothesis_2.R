# =============================================================================
# Script:      03_hypothesis_2.R
# Author:      Riley Ramos
# Date:        2025
# Description: Examines the relationship between urban sprawl and commute
#              length in the Las Vegas Valley using OLS regression models.
#              Also prepares commute duration data used in Table 6 of the
#              thesis (summary statistics by transportation method, which
#              are computed in 08_summary_stats.R).
#              Corresponds to Hypothesis 2 in the thesis.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#                - sheet: "work_commute_time"
#                - sheet: "mean_travel_time"
#
# Output:      Console — regression summaries
#              Console — stargazer LaTeX table (Table 5 in thesis)
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

mean_travel <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                          sheet = "mean_travel_time")


# =============================================================================
# Section 2: Rebuild Race + Income Dataset
# =============================================================================
# Re-derives the combined race and income dataset from the cleaned sheets,
# consistent with 02_hypothesis_1.R. 

race_income <- demographics %>%
  mutate(
    perc_poc      = (poc_population / total_population) * 100,
    median_income = median_income / 1000  # scale to thousands for interpretability
  ) %>%
  select(census_tract, perc_poc, median_income) %>%
  inner_join(sprawl, by = "census_tract")


# =============================================================================
# Section 3: Commute Data Preparation
# =============================================================================
# Aggregates commute duration counts by census tract across all transportation
# methods, then calculates the percentage of residents with long commutes
# (30 minutes or more) per tract. This aggregated dataset is used both in
# the OLS models below and in the summary statistics in 08_summary_stats.R.

# Aggregate across all transportation types
commute_grouped <- commute %>%
  filter(transportation_type == "All transportation methods") %>%
  select(-transportation_type) %>%
  group_by(census_tract, commute_length) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")

# Extract totals (denominator for percentage calculation)
all_intervals <- commute_grouped %>%
  filter(commute_length == "Total") %>%
  select(-commute_length) %>%
  rename(total_all = total_count)

# Calculate percentage of residents per time interval
commute_perc <- commute_grouped %>%
  filter(commute_length != "Total") %>%
  inner_join(all_intervals, by = "census_tract") %>%
  mutate(percent = total_count / total_all)

# Join with sprawl index
commute_sprawl <- inner_join(commute_perc, sprawl, by = "census_tract")

# Aggregate long commutes (>= 30 minutes) per census tract
commute_agg <- commute_sprawl %>%
  filter(commute_length %in% c("30 to 34 minutes",
                               "35 to 44 minutes",
                               "45 to 59 minutes",
                               "60 or more minutes")) %>%
  select(-commute_length) %>%
  group_by(census_tract, sprawl_index) %>%
  summarise(
    total_count      = sum(total_count, na.rm = TRUE),
    total_perc_long  = sum(percent, na.rm = TRUE),
    .groups          = "drop"
  )

# Join with race and income
commute_race <- inner_join(commute_agg, race_income,
                           by = c("census_tract", "sprawl_index"))


# =============================================================================
# Section 4: Mean Travel Time Preparation
# =============================================================================
# Joins mean travel time with race and income for use in thesis OLS models.

mean_travel_demogs <- mean_travel %>%
  inner_join(race_income, by = "census_tract") %>%
  rename(avg_travel_time = avg_work_commute_mins) %>%
  mutate(avg_travel_time = as.numeric(avg_travel_time))


# =============================================================================
# Section 5: OLS Regressions — Thesis Models (Table 5)
# =============================================================================
# Four models with average commute length (minutes) as the dependent variable.
# Results correspond to Table 5 in the thesis.

# Model 1: Effect of sprawl on average commute length
m13 <- lm(avg_travel_time ~ sprawl_index, data = mean_travel_demogs)
summary(m13)

# Model 2: Sprawl + % POC
m14 <- lm(avg_travel_time ~ sprawl_index + perc_poc, data = mean_travel_demogs)
summary(m14)

# Model 3: Sprawl + median income
m15 <- lm(avg_travel_time ~ sprawl_index + median_income, data = mean_travel_demogs)
summary(m15)

# Model 4: Sprawl + % POC + median income
m16 <- lm(avg_travel_time ~ sprawl_index + perc_poc + median_income,
           data = mean_travel_demogs)
summary(m16)


# =============================================================================
# Section 6: Regression Table Output (Table 5 in thesis)
# =============================================================================

stargazer(m13, m14, m15, m16,
          type             = "latex",
          digits           = 2,
          header           = FALSE,
          title            = "Impact of Sprawl on Commute Length, Controlling for Race and Income",
          covariate.labels = c("Sprawl Index", "% POC", "Median Income"),
          dep.var.labels   = "Average Commute Length (Minutes)")


# =============================================================================
# Section 7: Exploratory Models (not in thesis)
# =============================================================================
# Models below were used during analysis but did not appear in the final paper.
# Retained here for transparency and reproducibility.

# --- Pearson correlation test ------------------------------------------------

# % long commute vs sprawl
pear_test_result1 <- cor.test(commute_agg$total_perc_long,
                               commute_agg$sprawl_index,
                               method = "pearson")
print(pear_test_result1)


# --- Spearman correlation test -----------------------------------------------

# % long commute vs sprawl
sm_test_result1 <- cor.test(commute_agg$total_perc_long,
                             commute_agg$sprawl_index,
                             method = "spearman")
print(sm_test_result1)


# --- OLS: sprawl as dependent, % long commute as predictor ------------------

m1 <- lm(sprawl_index ~ total_perc_long, data = commute_agg)
summary(m1)

m2 <- lm(sprawl_index ~ total_perc_long + median_income, data = commute_race)
summary(m2)

m3 <- lm(sprawl_index ~ total_perc_long + perc_poc, data = commute_race)
summary(m3)

m4 <- lm(sprawl_index ~ total_perc_long + perc_poc + median_income,
          data = commute_race)
summary(m4)

m5 <- lm(sprawl_index ~ total_perc_long + (perc_poc * median_income),
          data = commute_race)
summary(m5)

m6 <- lm(sprawl_index ~ total_perc_long * median_income, data = commute_race)
summary(m6)

# --- OLS: sprawl as dependent, avg travel time as predictor (reversed) ------

m7  <- lm(sprawl_index ~ avg_travel_time, data = mean_travel_demogs)
summary(m7)

m8  <- lm(sprawl_index ~ avg_travel_time + perc_poc, data = mean_travel_demogs)
summary(m8)

m9  <- lm(sprawl_index ~ avg_travel_time + median_income, data = mean_travel_demogs)
summary(m9)

m10 <- lm(sprawl_index ~ avg_travel_time + perc_poc + median_income,
           data = mean_travel_demogs)
summary(m10)

m11 <- lm(sprawl_index ~ avg_travel_time + (perc_poc * median_income),
           data = mean_travel_demogs)
summary(m11)

m12 <- lm(avg_travel_time ~ sprawl_index, data = mean_travel_demogs)
summary(m12)

m17 <- lm(avg_travel_time ~ sprawl_index + (perc_poc * median_income),
           data = mean_travel_demogs)
summary(m17)

# Exploratory stargazer outputs
stargazer(m7, m8, m9, m10, m11,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Regression Results",
          covariate.labels = c("Average Commute Length to Work (Minutes)",
                               "% POC", "Median Income", "% POC x Median Income"),
          dep.var.labels   = "Sprawl Index")

stargazer(m13, m14, m15, m16, m17,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Regression Results",
          dep.var.labels   = "Average Commute Length (Minutes)",
          covariate.labels = c("Sprawl Index", "% POC",
                               "Median Income", "% POC x Median Income"))

# --- ANOVA + Tukey test ------------------------------------------------------
commute_classified <- commute_race %>%
  select(census_tract, sprawl_index, total_perc_long) %>%
  mutate(
    total_perc_short = 1 - total_perc_long,
    commute_class    = ifelse(total_perc_long >= 0.5, "long", "short")
  )

anova_result <- aov(sprawl_index ~ commute_class, data = commute_classified)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# --- Exploratory scatter plots -----------------------------------------------

ggplot(commute_classified, aes(x = total_perc_long, y = sprawl_index)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Sprawl Index vs. Percentage of Long Commute",
       x     = "Percent of census tract with a long commute",
       y     = "Sprawl Index")
