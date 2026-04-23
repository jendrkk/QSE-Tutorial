# 1. Packages ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(modelsummary)
library(ggplot2)
library(sf)
library(stringr)

# 2. Paths -------------------------------------------------------------------

# assumes script is run from: QSE-Tutorial/Topic_1
topic_dir <- getwd()
plot_dir  <- file.path(topic_dir, "Plots")
data_dir  <- file.path(topic_dir, "..", "Data")
shape_dir <- file.path(data_dir, "Shapefiles-2022", "Germany", "2024", "vg2500")

# create Plots folder if it does not exist
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 3. Data import -------------------------------------------------------------

wage_data <- read_excel(
  file.path(data_dir, "county-wages-2022", "entgelt-dwolk-0-202412-xlsx.xlsx"),
  sheet = "8.1",
  range = "A9:K6295"
)

# rename & select variables
wage_data <- wage_data %>%
  select(1:4, 11) %>%
  setNames(c("AGS", "region", "merkmale", "N", "wages"))

# filter counties + variables
wage_data <- wage_data %>%
  mutate(
    AGS = as.character(AGS),
    RS = AGS,
    wages = as.numeric(wages),
    N = as.numeric(N)
  ) %>%
  filter(
    nchar(AGS) == 5,
    merkmale %in% c(
      "ohne Berufsabschluss",
      "anerkannter Berufsabschluss",
      "akademischer Berufsabschluss",
      "Insgesamt"
    )
  )

# recode labels
wage_data <- wage_data %>%
  mutate(
    merkmale = recode(
      merkmale,
      "ohne Berufsabschluss" = "No vocational qualification",
      "anerkannter Berufsabschluss" = "Recognized vocational qualification",
      "akademischer Berufsabschluss" = "Academic degree",
      "Insgesamt" = "Overall"
    ),
    merkmale = factor(
      merkmale,
      levels = c(
        "No vocational qualification",
        "Recognized vocational qualification",
        "Academic degree",
        "Overall"
      )
    )
  )

# 4. Wage boxplot ------------------------------------------------------------

p_wages <- ggplot(wage_data, aes(x = merkmale, y = wages)) +
  geom_boxplot() +
  labs(
    title = "Distribution of County Median Wages",
    x = "Education group",
    y = "Median wage (€)"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_wages)

ggsave(
  filename = file.path(plot_dir, "2_boxplot_wages_by_education.pdf"),
  plot = p_wages,
  width = 8,
  height = 5
)

# 5. Worker percentile plot --------------------------------------------------

worker_stats <- wage_data %>%
  filter(merkmale != "Overall") %>%
  group_by(merkmale) %>%
  summarise(
    p10_workers = quantile(N, 0.10, na.rm = TRUE),
    p25_workers = quantile(N, 0.25, na.rm = TRUE),
    p50_workers = quantile(N, 0.50, na.rm = TRUE),
    p75_workers = quantile(N, 0.75, na.rm = TRUE),
    p90_workers = quantile(N, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    merkmale = factor(
      merkmale,
      levels = c(
        "No vocational qualification",
        "Recognized vocational qualification",
        "Academic degree"
      )
    )
  )

p_workers <- ggplot(worker_stats, aes(x = merkmale, y = p50_workers)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = p25_workers, ymax = p75_workers), width = 0.2) +
  geom_errorbar(aes(ymin = p10_workers, ymax = p90_workers), width = 0.1, alpha = 0.5) +
  labs(
    title = "Percentile Ranges of Number of Workers Across Counties",
    x = "Education group",
    y = "Number of workers"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_workers)

ggsave(
  filename = file.path(plot_dir, "2_percentile_ranges_workers_by_education.pdf"),
  plot = p_workers,
  width = 8,
  height = 5
)

# 6. Shape integration -------------------------------------------------------

shape <- st_read(file.path(shape_dir, "VG2500_KRS.shp"))

shape <- shape %>%
  mutate(AGS = as.character(AGS))

map_data <- left_join(shape, wage_data, by = "AGS")

# 7. Combined map ------------------------------------------------------------

p_map_all <- ggplot(map_data) +
  geom_sf(aes(fill = wages), color = NA) +
  facet_wrap(~ merkmale) +
  scale_fill_viridis_c(
    name = "Gross monthly earnings (€)",
    na.value = "lightgrey"
  ) +
  theme_void()

print(p_map_all)

ggsave(
  filename = file.path(plot_dir, "2_map_wages.pdf"),
  plot = p_map_all,
  width = 10,
  height = 6
)

# 8. Individual maps ---------------------------------------------------------

# Academic
p_map_academic <- ggplot(map_data %>% filter(merkmale == "Academic degree")) +
  geom_sf(aes(fill = wages), color = NA) +
  scale_fill_viridis_c(name = "Gross monthly earnings (€)", na.value = "lightgrey") +
  ggtitle("Academic degree") +
  theme_void()

ggsave(file.path(plot_dir, "2_map_academic.pdf"), p_map_academic, width = 6, height = 6)

# Vocational
p_map_vocational <- ggplot(map_data %>% filter(merkmale == "Recognized vocational qualification")) +
  geom_sf(aes(fill = wages), color = NA) +
  scale_fill_viridis_c(name = "Gross monthly earnings (€)", na.value = "lightgrey") +
  ggtitle("Recognized vocational qualification") +
  theme_void()

ggsave(file.path(plot_dir, "2_map_vocational.pdf"), p_map_vocational, width = 6, height = 6)

# No qualification
p_map_noqual <- ggplot(map_data %>% filter(merkmale == "No vocational qualification")) +
  geom_sf(aes(fill = wages), color = NA) +
  scale_fill_viridis_c(name = "Gross monthly earnings (€)", na.value = "lightgrey") +
  ggtitle("No vocational qualification") +
  theme_void()

ggsave(file.path(plot_dir, "2_map_no_qualification.pdf"), p_map_noqual, width = 6, height = 6)