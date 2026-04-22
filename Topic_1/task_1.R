# 1. Packages ----------------------------------------------------------------

packages <- c("readxl", "dplyr", "modelsummary", "sf", "ggplot2")

# If not installed, install packages
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

for (pkg in new_packages) {
  install.packages(pkg)
}

# Load packages

for (pkg in packages) {
  library(pkg, character.only = TRUE)
}

# Add "QSE-Tutorial" to the current directory

current_dir <- getwd()
repo_dir <- file.path(current_dir, "QSE-Tutorial")
data_dir <- file.path(repo_dir, "Data")

# 2. Data import ------------------------------------------------------------

data <- read_excel(file.path(data_dir, "county-wages-2022/country-wages-2024-entgelt-dwolk-0-202412-xlsx.xlsx"), sheet = "8.1", range = "A9:K6295")
# Stichtag 31.12.2024

#rename & choose relevant variables
data <- data  %>%
  select(1:4, 11) %>%
  setNames(c(
    "RS",#Region Schlüssel
    "region",
    "merkmale",
    "N", #"insgesamt workers"
    "wages" #median euro"
  ))

#select counties only
data <- data %>%
  mutate(RS = as.character(RS)) %>%
  filter(nchar(RS) == 5)

#select correct qualifications only
data <- data %>%
  filter(merkmale %in% c(
    "ohne Berufsabschluss",
    "anerkannter Berufsabschluss",
    "akademischer Berufsabschluss"
  ))

#
data <- data %>%
mutate(wages = as.numeric(wages))


# 3. DescriptiveStat ---------------------------------------------------------

# Wage distribution by education
tab <- data %>%
  group_by(merkmale) %>%
  summarise(
    Mean = mean(wages, na.rm = TRUE),
    Median = median(wages, na.rm = TRUE),
    SD = sd(wages, na.rm = TRUE),
    N = sum(!is.na(wages)),
    .groups = "drop"
  )

datasummary_df(
  tab,
  output = "latex",
  title = "Wage Distribution by Education"
)
 
# 4. Shape files integration
shape <- st_read(file.path(data_dir, "Shapefiles-2022/Germany/COUNTIES/VG250_KRS_clean_final.shp"))

data <- data %>%
  mutate(RS = as.character(RS))

shape <- shape %>%
  mutate(RS = as.character(RS))

map_data <- left_join(shape, data, by = "RS")


# 5. Maps -----------------------------------------------------------------

ggplot(map_data) +
  geom_sf(aes(fill = wages), color = NA) +
  facet_wrap(~ merkmale) +
  scale_fill_viridis_c(
    name = "Gross monthly earnings (€)",
    na.value = "lightgrey"
  ) +
  theme_void()
