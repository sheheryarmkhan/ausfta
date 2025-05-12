# --- Load Libraries ---
library(dplyr)
library(tidyr)
library(fixest)
library(here)
library(ggplot2)
library(broom)

#combining datasets

# Load datasets
data <- readRDS(here("input","data","ourdata.rds"))

data <- data %>%
  filter(year <= 2016) %>%
  filter(!is.na(gdp_pwt_const_o), !is.na(gdp_pwt_const_d),
         !is.na(gdp_wdi_cap_const_o), !is.na(gdp_wdi_cap_const_d)) %>%
  mutate(
    Treated = ifelse((exporter_iso3 == "AUS" & importer_iso3 == "USA") |
                       (exporter_iso3 == "USA" & importer_iso3 == "AUS"), 1, 0),
    Post = ifelse(year >= 2005, 1, 0),
    AUSFTA = Treated * Post,
    aus_us_pair = Treated,
    log_trade = ifelse(trade > 0, log(trade), NA),
    pair_id = paste(pmin(exporter_iso3, importer_iso3),
                    pmax(exporter_iso3, importer_iso3), sep = "_")
  )

# --- Naive Gravity Model (No FEs, No Controls) ---
naive_model <- feols(
  log_trade ~ log(gdp_pwt_const_o) + log(gdp_pwt_const_d) +
    log(distance) + AUSFTA,
  data = data)
print(summary(naive_model))

# --- Base Gravity Model (No FEs, Add Controls) ---
base_model <- feols(
  log_trade ~ log(gdp_pwt_const_o) + log(gdp_pwt_const_d) +
    log(distance) + AUSFTA + agree_fta + agree_psa + common_language + colony_ever + contiguity,
  data = data, cluster = ~pair_id
)
print(summary(base_model))

# --- Structural Gravity with Full Fixed Effects ---
fe_model_structural <- feols(
  log_trade ~ AUSFTA + agree_fta + agree_psa + common_language + colony_ever + contiguity |
    pair_id + exporter_iso3^year + importer_iso3^year,
  data = data, cluster = ~pair_id
)
print(summary(fe_model_structural))

# --- PPML Model with Full Fixed Effects ---
ppml_model <- fepois(
  trade ~ AUSFTA + agree_fta + agree_psa + common_language + colony_ever + contiguity |
    pair_id + exporter_iso3^year + importer_iso3^year,
  data = data, cluster = ~pair_id
)
print(summary(ppml_model))

# --- Difference-in-Differences Model ---
data_did <- data %>%
  mutate(
    Treated = ifelse((exporter_iso3 == "AUS" & importer_iso3 == "USA") |
                       (exporter_iso3 == "USA" & importer_iso3 == "AUS"), 1, 0),
    Post = ifelse(year >= 2005, 1, 0),
    DiD_term = Treated * Post
  )

did_model <- fepois(
  trade ~ DiD_term | pair_id + year,
  data = data_did, cluster = ~pair_id
)
print(summary(did_model))

# --- Event Study (Dynamic Effects) ---
years <- sort(unique(data$year))
data <- data %>%
  mutate(year_str = paste0("aus_us_", year),
         dummy = ifelse(aus_us_pair == 1, 1, 0)) %>%
  pivot_wider(names_from = year_str, values_from = dummy, values_fill = 0)

dynamic_vars <- paste0("aus_us_", years[years != min(years)], collapse = " + ")
dynamic_formula <- as.formula(paste("log_trade ~", dynamic_vars,
                                    "| pair_id + exporter_iso3^year + importer_iso3^year"))

dynamic_model <- feols(dynamic_formula, data = data, cluster = ~pair_id)
print(summary(dynamic_model))

# --- Placebo & Lagged Treatment Tests ---
data <- data %>%
  mutate(
    AUSFTA_placebo_2003 = ifelse(aus_us_pair == 1 & year >= 2003, 1, 0),
    AUSFTA_lag = ifelse(aus_us_pair == 1 & year >= 2006, 1, 0)
  )

placebo_model <- feols(
  log_trade ~ AUSFTA_placebo_2003 |
    pair_id + exporter_iso3^year + importer_iso3^year,
  data = data, cluster = ~pair_id
)
print(summary(placebo_model))

lag_model <- feols(
  log_trade ~ AUSFTA_lag |
    pair_id + exporter_iso3^year + importer_iso3^year,
  data = data, cluster = ~pair_id
)
print(summary(lag_model))