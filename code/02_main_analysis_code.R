# --- Load Libraries ---
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(broom)

# --- Load & Clean Data ---
setwd("~/RIA Work")
data <- readRDS("my_data.rds")

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


















# --- Define Sectors (Adjust 'sector' if named differently) ---
# Check unique sectors first
unique(data$sector)  # adjust this to inspect sector names

# Filter for Agriculture and Manufacturing
data_agri <- data %>% filter(sector == "agriculture")
data_manuf <- data %>% filter(sector == "manufacturing")

# --- Run Sectoral Gravity Models (PPML with full FEs) ---

model_agri <- fepois(
  trade ~ AUSFTA + agree_fta + agree_eia + agree_cu + agree_psa |
    pair_id + exporter_iso3^year + importer_iso3^year,
  data = data_agri,
  cluster = ~pair_id
)

model_manuf <- fepois(
  trade ~ AUSFTA + agree_fta + agree_eia + agree_cu + agree_psa |
    pair_id + exporter_iso3^year + importer_iso3^year,
  data = data_manuf,
  cluster = ~pair_id
)

# --- Print Results ---
cat("=== Agriculture Sector ===\n")
print(summary(model_agri))

cat("\n=== Manufacturing Sector ===\n")
print(summary(model_manuf))

# --- Optional: Coefficient Plot ---
coefs <- data.frame(
  sector = c("Agriculture", "Manufacturing"),
  coef = c(coef(model_agri)["AUSFTA"], coef(model_manuf)["AUSFTA"]),
  se = c(se(model_agri)["AUSFTA"], se(model_manuf)["AUSFTA"])
)

ggplot(coefs, aes(x = sector, y = coef)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of AUSFTA by Sector", y = "Coefficient on AUSFTA (PPML)", x = "") +
  theme_minimal()




#STAR GAZER

install.packages("modelsummary")
library(modelsummary)


# Create a named list of models
models <- list(
  "Naive Gravity" = naive_model,
  "Base Gravity" = base_model,
  "Structural Gravity (FE)" = fe_model_structural,
  "PPML (FE)" = ppml_model,
  "DiD (PPML)" = did_model,
  "Placebo 2003" = placebo_model,
  "Lag 2006" = lag_model
)

# Display table in console or export to LaTeX
modelsummary(models,
             stars = TRUE,
             gof_omit = "IC|Log|Adj|Within|RMSE",
             statistic = "std.error",
             output = "results_table.tex")  # or "markdown", "html", or file path
