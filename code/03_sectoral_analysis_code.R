# Load required packages
library(dplyr)
library(fixest)

# Load datasets
data1 <- readRDS("C:/Users/Sherry/Desktop/usitc_itpd_e_r02.rds")
data2 <- readRDS("C:/Users/Sherry/Desktop/usitc_dgd.rds")

# Merge datasets on year and dynamic country codes
merged_data <- left_join(
  data1, data2,
  by = c(
    "year",
    "exporter_dynamic_code" = "dynamic_code_o",
    "importer_dynamic_code" = "dynamic_code_d"
  )
)

# Keep only AUS-USA bilateral and domestic flows
aus_us_data <- merged_data %>%
  filter(
    (exporter_dynamic_code == "AUS" & importer_dynamic_code == "USA") |
      (exporter_dynamic_code == "USA" & importer_dynamic_code == "AUS") |
      (exporter_dynamic_code == "AUS" & importer_dynamic_code == "AUS") |
      (exporter_dynamic_code == "USA" & importer_dynamic_code == "USA")
  )
# Filter and clean dataset
df <- aus_us_data %>%
  filter(year >= 1988) %>%
  filter(broad_sector %in% c("Agriculture", "Manufacturing", "Mining and Energy")) %>%
  mutate(
    ausfta = ifelse(year >= 2005, 1, 0),
    trade = trade,  # assuming trade is clean here
    broad_sector = factor(broad_sector)  # ensure it's treated as a factor
  ) %>%
  filter(!is.na(trade), !is.na(broad_sector), !is.na(exporter_iso3), !is.na(importer_iso3))

# Estimate PPML model with sector interaction
ppml_sector_model <- fepois(
  trade ~ ausfta * broad_sector |
    interaction(exporter_iso3, year) +
    interaction(importer_iso3, year) +
    exporter_iso3^importer_iso3,
  data = df,
  cluster = ~ interaction(exporter_iso3, year)
)

# View results
summary(ppml_sector_model)


#Only for Services Sector
df_services <- aus_us_data %>%
  filter(year >= 2000) %>%
  filter(broad_sector %in% c("Services", "Manufacturing")) %>%
  mutate(
    ausfta = ifelse(year >= 2005, 1, 0),
    trade = trade,  # assuming trade is clean here
    broad_sector = factor(broad_sector)  # ensure it's treated as a factor
  ) %>%
  filter(!is.na(trade), !is.na(broad_sector), !is.na(exporter_iso3), !is.na(importer_iso3))

# Estimate PPML model with sector interaction
ppml_services <- fepois(
  trade ~ ausfta |
    interaction(exporter_iso3, year) +
    interaction(importer_iso3, year) +
    exporter_iso3^importer_iso3,
  data = df_services,
  cluster = ~ interaction(exporter_iso3, year)
)

# View results
summary(ppml_services)

