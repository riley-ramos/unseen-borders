# =============================================================================
# Script:      09_maps.R
# Author:      Riley Ramos
# Date:        2025
# Description: Generates choropleth maps of key thesis variables by census
#              tract for the Las Vegas Valley. Maps include urban sprawl index,
#              % people of color, median household income, and average commute
#              length. Corresponds to figures in the thesis.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#                - sheet: "mean_travel_time"
#              Census tract shapefiles fetched via tigris (Clark County, NV,
#              2010 ACS)
#
# Output:      figures/sprawl_index_map.png
#              figures/perc_poc_map.png
#              figures/median_income_map.png
#              figures/avg_commute_map.png     (exploratory — not in thesis)
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)          # portable file paths
library(readxl)        # read Excel workbook sheets
library(tidyverse)     # dplyr, ggplot2, etc.
library(tigris)        # Census tract shapefiles
library(scales)        # label_comma() for axis formatting
library(RColorBrewer)  # color palettes


# --- Global options ----------------------------------------------------------

options(scipen = 999)  # disable scientific notation
options(tigris_use_cache = TRUE)  # cache shapefiles locally


# =============================================================================
# Section 1: Load Data
# =============================================================================

sprawl <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "sprawl_index")

demographics <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                           sheet = "demographics") %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .))))

mean_travel <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                          sheet = "mean_travel_time") %>% 
  mutate(avg_work_commute_mins = as.numeric(gsub(",", "", avg_work_commute_mins)))


# =============================================================================
# Section 2: Derive Race and Income Variables
# =============================================================================

race_income <- demographics %>%
  mutate(
    perc_poc = (poc_population / total_population) * 100
  ) %>%
  select(census_tract, perc_poc, median_income)


# =============================================================================
# Section 3: Load Census Tract Boundaries
# =============================================================================
# Fetches Clark County, NV tract shapefiles from the Census Bureau via tigris.
# Tracts are filtered to the Las Vegas Valley core — peripheral tracts in
# Laughlin and Moapa are excluded for visual consistency across all maps.

tracts <- tracts(state = "NV", county = "Clark", year = 2010, class = "sf")

colnames(tracts) <- tolower(colnames(tracts))

tracts <- tracts %>%
  rename(census_tract = tractce10)

# Peripheral tracts excluded from all maps (Laughlin / Moapa Valley)
lower_tracts  <- c("005704", "005702")
higher_tracts <- c("005903", "005905", "005614", "005607", "005612", "005615")
exclude_tracts <- c(lower_tracts, higher_tracts)

# =============================================================================
# Section 4: Thesis Maps
# =============================================================================

# --- Shared theme ------------------------------------------------------------
map_theme <- function() {
  theme_minimal(base_family = "Times New Roman") +
    theme(
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "panel",
      plot.subtitle       = element_text(hjust = 0.5, size = 11),
      legend.title        = element_text(size = 10),
      legend.text         = element_text(size = 9)
    )
}


# --- Map 1: Urban Sprawl Index -----------------------------------------------

sprawl_map <- tracts %>%
  inner_join(sprawl, by = "census_tract") %>%
  filter(!name10 %in% c("57.04", "57.02", "59.03", "59.05", "56.14", "56.07", "56.12", "56.15"))

p_sprawl <- ggplot(sprawl_map) +
  geom_sf(aes(fill = sprawl_index), color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Sprawl Index") +
  labs(
    title    = "Urban Sprawl in the Las Vegas Valley",
    subtitle = "Clark County, NV (2010)"
  ) +
  map_theme()

print(p_sprawl)
ggsave(here("figures", "sprawl_index_map.png"), plot = p_sprawl,
       width = 8, height = 6, dpi = 300)


# --- Map 2: % People of Color ------------------------------------------------

poc_map <- tracts %>%
  inner_join(race_income, by = "census_tract") %>%
  filter(census_tract %in% sprawl$census_tract) 

p_poc <- ggplot(poc_map) +
  geom_sf(aes(fill = perc_poc), color = "white") +
  scale_fill_viridis_c(
    option    = "viridis",
    name      = "% POC",
    direction = -1
  ) +
  labs(
    title    = "Percentage of People of Color by Census Tract",
    subtitle = "Clark County, NV (2010)"
  ) +
  map_theme()

print(p_poc)
ggsave(here("figures", "perc_poc_map.png"), plot = p_poc,
       width = 8, height = 6, dpi = 300)


# --- Map 3: Median Household Income ------------------------------------------

income_map <- tracts %>%
  inner_join(race_income, by = "census_tract") %>%
  filter(census_tract %in% sprawl$census_tract) 

p_income <- ggplot(income_map) +
  geom_sf(aes(fill = median_income), color = "white") +
  scale_fill_viridis_c(
    option = "viridis",
    name   = "Median Income ($)",
    labels = label_comma()
  ) +
  labs(
    title    = "Median Household Income by Census Tract",
    subtitle = "Clark County, NV (2010)"
  ) +
  map_theme()

print(p_income)
ggsave(here("figures", "median_income_map.png"), plot = p_income,
       width = 8, height = 6, dpi = 300)


# =============================================================================
# Section 5: Exploratory Maps (not in thesis)
# =============================================================================
# Maps below were produced during analysis but did not appear in the final
# paper. Retained here for transparency and reproducibility.

# --- Average Commute Length --------------------------------------------------

commute_map <- tracts %>%
  inner_join(mean_travel, by = "census_tract") %>%
  filter(census_tract %in% sprawl$census_tract) 

p_commute <- ggplot(commute_map) +
  geom_sf(aes(fill = avg_work_commute_mins), color = "white") +
  scale_fill_viridis_c(
    option    = "viridis",
    name      = "Commute Length (Minutes)",
    labels    = label_comma(),
    direction = -1
  ) +
  labs(
    title    = "Average Commute Length by Census Tract",
    subtitle = "Clark County, NV (2010)"
  ) +
  map_theme()

print(p_commute)
ggsave(here("figures", "avg_commute_map.png"), plot = p_commute,
       width = 8, height = 6, dpi = 300)
