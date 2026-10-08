# =============================================================================
# Script:      10_burden_maps.R
# Author:      Riley Ramos
# Date:        2025
# Description: Generates a choropleth map of the equity burden score by census
#              tract for the Las Vegas Valley. The burden score is a PCA-derived
#              composite of race and income marginalization — computed in
#              07_combined_analysis.ipynb and exported as burden_score.csv.
#              Corresponds to the burden score map figure in the thesis.
#
# Input:       data/processed/burden_score.csv
#              Census tract shapefiles fetched via tigris (Clark County, NV,
#              2010 ACS)
#
# Output:      figures/burden_score_map.png
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)       # portable file paths
library(readr)      # read_csv
library(tidyverse)  # dplyr, ggplot2, etc.
library(tigris)     # Census tract shapefiles


# --- Global options ----------------------------------------------------------

options(scipen = 999)           # disable scientific notation
options(tigris_use_cache = TRUE) # cache shapefiles locally


# =============================================================================
# Section 1: Load Data
# =============================================================================

burden <- read_csv(here("data", "processed", "burden_score.csv"))


# =============================================================================
# Section 2: Load Census Tract Boundaries
# =============================================================================
# Fetches Clark County, NV tract shapefiles from the Census Bureau via tigris.
# Peripheral tracts (Laughlin / Moapa Valley) excluded for visual consistency
# with 09_maps.R.

tracts <- tracts(state = "NV", county = "Clark", year = 2010, class = "sf")

colnames(tracts) <- tolower(colnames(tracts))

tracts <- tracts %>%
  rename(census_tract = tractce10)

lower_tracts  <- c("57.04", "57.02")
higher_tracts <- c("59.03", "59.05", "56.14", "56.07", "56.12", "56.15")


# =============================================================================
# Section 3: Thesis Map — Equity Burden Score
# =============================================================================

burden_map <- tracts %>%
  inner_join(burden, by = "census_tract") %>%
  filter(!name10 %in% lower_tracts) %>%
  filter(!name10 %in% higher_tracts)

p_burden <- ggplot(burden_map) +
  geom_sf(aes(fill = burden_score), color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Sprawl Burden Score", direction = -1) +
  labs(
    title    = "Sprawl Burden in the Las Vegas Valley",
    subtitle = "Clark County, NV (2010)"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title          = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.title.position = "panel",
    plot.subtitle       = element_text(hjust = 0.5, size = 11),
    legend.title        = element_text(size = 10),
    legend.text         = element_text(size = 9)
  )

print(p_burden)
ggsave(here("figures", "burden_score_map.png"), plot = p_burden,
       width = 8, height = 6, dpi = 300)
