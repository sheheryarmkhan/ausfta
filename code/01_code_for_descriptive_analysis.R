# load once
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# 0. Read data
data <- readRDS("C:/Users/Sherry/Desktop/usitc_itpd_e_r02.rds")

### Notes on dataframes
## Data = original dataset from the professor
## filtered_data = filtered for exports or imports from/to USA/AUS to all other countries
## filtered_data = added the string var. of broad sectors "mining, agriculture, etc. 
## hhi_exporters_only = dataset to calculate HHI for only USA and AUS
## filtered_data2 = filtered_data with no intra trade flows, droping AUS-AUS and USA-USA
## filtered_data3 = filtered_data2 with relative trading between sectors for a graph

# 1. Filtering for export or import AUS or USA to other countries, dropping all other obs.
filtered_data <- data %>%
  filter(exporter_iso3 %in% c("USA", "AUS") |
           importer_iso3 %in% c("USA", "AUS"))

# Check
unique(filtered_data$exporter_iso3)
unique(filtered_data$importer_iso3)
summary(filtered_data$trade)



# 3. HHI for USA & AUS exports (note: still has intra-flows)
hhi_exporters_only <- filtered_data %>%
  filter(exporter_iso3 %in% c("USA","AUS")) %>%
  group_by(exporter_iso3, year, broad_sector) %>%
  summarise(total_trade = sum(trade, na.rm=TRUE), .groups="drop") %>%
  group_by(exporter_iso3, year) %>%
  mutate(share = total_trade / sum(total_trade, na.rm=TRUE)) %>%
  summarise(hhi = sum(share^2), .groups="drop")

ggplot(hhi_exporters_only, aes(x=year, y=hhi, color=exporter_iso3)) +
  geom_line(size=1.2) +
  labs(
    title="Sectoral Trade Concentration (HHI) from AUS and USA",
    x="Year", y="Herfindahl Index", color="Exporter"
  ) +
  theme_minimal()

# 4. Pre/Post AUSFTA avg trade (AUS→USA)
prepost_data <- filtered_data %>%
  filter(exporter_iso3=="AUS", importer_iso3=="USA") %>%
  mutate(period = ifelse(year < 2005, "Pre-AUSFTA","Post-AUSFTA")) %>%
  group_by(period) %>%
  summarise(avg_trade = mean(trade, na.rm=TRUE), .groups="drop")

ggplot(prepost_data, aes(x=period, y=avg_trade, fill=period)) +
  geom_col() +
  labs(
    title="Avg AUS → USA Trade Before & After AUSFTA",
    y="Avg Trade Value", x=NULL
  ) +
  theme_minimal()

# 5. Exclude AUS→AUS and USA→USA trade
filtered_data2 <- filtered_data %>%
  filter(!(exporter_iso3==importer_iso3 &
             exporter_iso3 %in% c("AUS","USA")))

# 6. Sectoral analysis with no intra-flows for all destinations
#    first build filtered_data3 with year/exporter/sector shares
filtered_data3 <- filtered_data2 %>%
  group_by(exporter_iso3, year, broad_sector) %>%
  summarise(trade_sum = sum(trade, na.rm=TRUE), .groups="drop") %>%
  group_by(exporter_iso3, year) %>%
  mutate(share = trade_sum / sum(trade_sum, na.rm=TRUE), .groups="drop")

ggplot(filtered_data3, aes(x=year, y=share, fill=broad_sector)) +
  geom_area() +
  geom_vline(xintercept=2005, linetype="dashed", color="gray30") +
  facet_wrap(~exporter_iso3) +
  labs(
    title="Sectoral Composition of Exports (Relative Terms)",
    subtitle="Dashed line marks AUSFTA (2005)",
    y="Share of Total Exports", x="Year", fill="Sector"
  ) +
  theme_minimal()

# 7. Bilateral AUS↔USA sectoral shares (no intra-flows)
filtered_data_bilateral <- filtered_data2 %>%
  filter((exporter_iso3=="AUS" & importer_iso3=="USA") |
           (exporter_iso3=="USA" & importer_iso3=="AUS")) %>%
  group_by(year, exporter_iso3, broad_sector) %>%
  summarise(trade_value = sum(trade, na.rm=TRUE), .groups="drop") %>%
  group_by(year, exporter_iso3) %>%
  mutate(share = trade_value / sum(trade_value, na.rm=TRUE), .groups="drop")

ggplot(filtered_data_bilateral, aes(x=year, y=share, fill=broad_sector)) +
  geom_area() +
  geom_vline(xintercept=2005, linetype="dashed", color="gray30") +
  facet_wrap(~exporter_iso3) +
  labs(
    title="Bilateral Sectoral Composition of Exports (AUS ↔ USA)",
    subtitle="Dashed line marks AUSFTA (2005)",
    y="Share of Bilateral Exports", x="Year", fill="Sector"
  ) +
  theme_minimal()

# 8. New HHI analysis with no intra-flows
## 8a. by destination
hhi_aus_usa_destinations <- filtered_data2 %>%
  filter(exporter_iso3 %in% c("AUS","USA")) %>%
  group_by(exporter_iso3, year, importer_iso3) %>%
  summarise(trade_sum = sum(trade, na.rm=TRUE), .groups="drop") %>%
  group_by(exporter_iso3, year) %>%
  mutate(share = trade_sum / sum(trade_sum, na.rm=TRUE)) %>%
  summarise(HHI = sum(share^2), .groups="drop")

## 8b. by sector
hhi_aus_usa_sectors <- filtered_data2 %>%
  filter(exporter_iso3 %in% c("AUS","USA")) %>%
  group_by(exporter_iso3, year, broad_sector) %>%
  summarise(trade_sum = sum(trade, na.rm=TRUE), .groups="drop") %>%
  group_by(exporter_iso3, year) %>%
  mutate(share = trade_sum / sum(trade_sum, na.rm=TRUE)) %>%
  summarise(HHI = sum(share^2), .groups="drop")

# plots
plot_dest <- ggplot(hhi_aus_usa_destinations, aes(x=year, y=HHI, color=exporter_iso3)) +
  geom_line(size=1) +
  geom_vline(xintercept=2005, linetype="dashed", color="gray40") +
  labs(
    title="Export Market Concentration (by Destination)",
    subtitle="Vertical line = AUSFTA (2005)",
    y="Herfindahl Index", x="Year", color="Exporter"
  ) +
  theme_minimal()

plot_sector <- ggplot(hhi_aus_usa_sectors, aes(x=year, y=HHI, color=exporter_iso3)) +
  geom_line(size=1) +
  geom_vline(xintercept=2005, linetype="dashed", color="gray40") +
  labs(
    title="Export Concentration by Sector",
    subtitle="Vertical line = AUSFTA (2005)",
    y="Herfindahl Index", x="Year", color="Exporter"
  ) +
  theme_minimal()

plot_dest + plot_sector

# 9. Change in avg sectoral share Pre vs Post AUSFTA
filtered_data_bilateral %>%
  mutate(period = ifelse(year < 2005, "Pre-AUSFTA","Post-AUSFTA")) %>%
  group_by(exporter_iso3, broad_sector, period) %>%
  summarise(avg_share = mean(share, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=period, values_from=avg_share) %>%
  mutate(change = `Post-AUSFTA` - `Pre-AUSFTA`) %>%
  ggplot(aes(x=broad_sector, y=change, fill=exporter_iso3)) +
  geom_bar(stat="identity", position="dodge") +
  labs(
    title="Change in Sectoral Export Share (Post - Pre AUSFTA)",
    y="Change in Share", x="Sector"
  ) +
  theme_minimal()

# 10. Export partner share over time (example: top 6 + Other)
export_share_by_destination <- filtered_data2 %>%
  filter(exporter_iso3 %in% c("AUS","USA")) %>%
  group_by(year, exporter_iso3, importer_iso3) %>%
  summarise(trade_value = sum(trade, na.rm=TRUE), .groups="drop") %>%
  group_by(year, exporter_iso3) %>%
  mutate(share = trade_value / sum(trade_value, na.rm=TRUE), .groups="drop")

top_partners <- export_share_by_destination %>%
  group_by(exporter_iso3, importer_iso3) %>%
  summarise(total = sum(trade_value), .groups="drop") %>%
  group_by(exporter_iso3) %>%
  slice_max(total, n=6)

export_share_plot <- export_share_by_destination %>%
  mutate(partner = ifelse(
    paste(exporter_iso3, importer_iso3) %in%
      paste(top_partners$exporter_iso3, top_partners$importer_iso3),
    importer_iso3, "Other"
  )) %>%
  group_by(year, exporter_iso3, partner) %>%
  summarise(share = sum(share, na.rm=TRUE), .groups="drop")

ggplot(export_share_plot, aes(x=year, y=share, fill=partner)) +
  geom_area() +
  geom_vline(xintercept=2005, linetype="dashed", color="gray30") +
  facet_wrap(~exporter_iso3) +
  labs(
    title="Export Partner Share Over Time",
    subtitle="Top 6 partners + Others; Dashed line = AUSFTA (2005)",
    y="Share of Total Exports", x="Year", fill="Importing Partner"
  ) +
  theme_minimal()
