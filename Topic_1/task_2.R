# task_2.R
# Translation of Topic_1/task_2.ipynb from Python to R

gc()

library(sf)
library(dplyr)
library(haven)
library(ggplot2)
library(viridis)
library(stringr)

data_dir <- file.path(getwd(), "QSE-Tutorial", "Data")
plot_dir <- file.path(getwd(), "QSE-Tutorial", "Topic_1", "Plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Shapefiles
kreise <- st_read(
  file.path(data_dir, "Shapefiles-2022", "Germany", "COUNTIES", "VG250_KRS_clean_final.shp"),
  quiet = TRUE
)

# Load the data
data_wk <- read_dta(file.path(data_dir, "RWI-GEO-RED-2022", "cross_section", "CampusFile_WK_2022.dta"))
data_hk <- read_dta(file.path(data_dir, "RWI-GEO-RED-2022", "cross_section", "CampusFile_HK_2022.dta"))
data_wm <- read_dta(file.path(data_dir, "RWI-GEO-RED-2022", "cross_section", "CampusFile_WM_2022.dta"))

# Price/rent in logs
data_wk$log_price_sqm <- log(data_wk$price_sqm)
data_wk$type <- "WK"
data_hk$log_price_sqm <- log(data_hk$price_sqm)
data_hk$type <- "HK"

data_wm$log_rent_sqm <- log(data_wm$rent_sqm)

# Concatenate the dataframes wk and hk
data_k <- bind_rows(data_wk, data_hk)
data_k$kid2019 <- str_pad(as.character(data_k$kid2019), width = 5, side = "left", pad = "0")

rm(data_wk, data_hk)
gc()

mean_log_price_sqm <- data_k %>%
  group_by(kid2019) %>%
  summarise(log_price_sqm = mean(log_price_sqm, na.rm = TRUE), .groups = "drop")

# Which indexes of mean_log_price_sqm are not in kreise$county_id?
missing_kids <- mean_log_price_sqm$kid2019[!(mean_log_price_sqm$kid2019 %in% kreise$county_id)]
print(missing_kids)

# KID2019 "03152", "03156" are actually 03159 -> Landkreis Gottingen

# Replace "03152" and "03156" with "03159" in all data
data_k$kid2019[data_k$kid2019 %in% c("03152", "03156")] <- "03159"
data_wm$kid2019 <- str_pad(as.character(data_wm$kid2019), width = 5, side = "left", pad = "0")
data_wm$kid2019[data_wm$kid2019 %in% c("03152", "03156")] <- "03159"

mean_log_price_sqm <- data_k %>%
  group_by(kid2019) %>%
  summarise(log_price_sqm = mean(log_price_sqm, na.rm = TRUE), .groups = "drop")

# Join the geometries to the data
mean_log_price_sqm <- kreise %>%
  left_join(mean_log_price_sqm, by = c("county_id" = "kid2019"))

# Set up for plotting
# Params for clean and minimalistic plots
theme_set(theme_bw(base_size = 14, base_family = "Palatino"))

# Plot the means of log price per sqm
p_mean <- ggplot(mean_log_price_sqm) +
  geom_sf(aes(fill = log_price_sqm), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c() +
  labs(title = "Mean Log Price per Square Meter by County", fill = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = file.path(plot_dir, "mean_log_price_sqm_R.png"),
  plot = p_mean,
  width = 12,
  height = 9,
  dpi = 300
)
print(p_mean)

# Estimation of the model for the properties for sale

# Helpers
impute_median <- function(x) {
  valid_median <- median(x[x >= 0], na.rm = TRUE)
  x[x < 0] <- valid_median
  x
}

n_neg <- function(x) sum(x < 0, na.rm = TRUE)
n_na <- function(x) sum(is.na(x))

# Data cleaning and preparations

# wohnflaeche
cat("Number of missing values in wohnflaeche:", n_neg(data_k$wohnflaeche), n_na(data_k$wohnflaeche), "\n")

# zimmeranzahl
cat("Number of missing values in zimmeranzahl:", n_neg(data_k$zimmeranzahl), n_na(data_k$zimmeranzahl), "\n")
# replace by county median
data_k <- data_k %>%
  group_by(kid2019) %>%
  mutate(zimmeranzahl = impute_median(zimmeranzahl)) %>%
  ungroup()
cat("After imputation, number of missing values in zimmeranzahl:", n_neg(data_k$zimmeranzahl), n_na(data_k$zimmeranzahl), "\n")

# baujahr
cat("Number of missing values in baujahr:", n_neg(data_k$baujahr), n_na(data_k$baujahr), "\n")
# binning the years into categories
bins <- c(0, 1900, 1918, 1945, 1960, 1978, 1990, 2000, 2010, Inf)
labels <- c(
  "pre_1900", "1900_1918", "1919_1945",
  "1946_1960", "1961_1978", "1979_1990",
  "1991_2000", "2001_2010", "2011_plus"
)

# bin valid years
data_k$baujahr_bin <- cut(
  data_k$baujahr,
  breaks = bins,
  labels = labels,
  right = TRUE
)

# add unknown category and assign all invalid/missing to it
data_k$baujahr_bin <- as.character(data_k$baujahr_bin)
data_k$baujahr_bin[is.na(data_k$baujahr) | data_k$baujahr < 1500] <- "unknown"
data_k$baujahr_bin <- factor(data_k$baujahr_bin, levels = c(labels, "unknown"))
cat("After binning and imputation, number of missing values in baujahr_bin:", n_na(data_k$baujahr_bin), "\n")

# ausstattung
cat("Number of missing values in ausstattung:", n_neg(data_k$ausstattung), n_na(data_k$ausstattung), "\n")
# replace <0 by 0 and create dummy variable for missing
data_k$ausstattung[data_k$ausstattung < 0] <- 0
data_k$ausstattung_missing <- as.integer(data_k$ausstattung %in% c(0))
cat("After imputation, number of missing values in ausstattung:", n_neg(data_k$ausstattung), n_na(data_k$ausstattung), "\n")

# objektzustand
cat("Number of missing values in objektzustand:", n_neg(data_k$objektzustand), n_na(data_k$objektzustand), "\n")
# replace <0 and 9 by 0 and create dummy variable for missing
data_k$objektzustand_val <- data_k$objektzustand
data_k$objektzustand_val[data_k$objektzustand %in% c(-7, 9)] <- NA
data_k$objektzustand_missing <- as.integer(data_k$objektzustand %in% c(-7, 9))
data_k$objektzustand_val[is.na(data_k$objektzustand_val)] <- 0
cat("After imputation, number of missing values in objektzustand_val:", n_neg(data_k$objektzustand_val), n_na(data_k$objektzustand_val), "\n")

# apartment
data_k$apartment <- as.integer(data_k$type == "WK")

# balkon
cat("Number of missing values in balkon:", n_neg(data_k$balkon), n_na(data_k$balkon), "\n")
cat(
  "Number of missing values in balkon (WK):",
  n_neg(data_k$balkon[data_k$type == "WK"]),
  n_na(data_k$balkon[data_k$type == "WK"]),
  "\n"
)
cat(
  "Number of missing values in balkon (HK):",
  n_neg(data_k$balkon[data_k$type == "HK"]),
  n_na(data_k$balkon[data_k$type == "HK"]),
  "\n"
)
# replace NA by 0, since it will be interacted with apartment variable
data_k$balkon[is.na(data_k$balkon)] <- 0

# Aufzug
cat("Number of missing values in aufzug:", n_neg(data_k$aufzug), n_na(data_k$aufzug), "\n")
cat(
  "Number of missing values in aufzug (WK):",
  n_neg(data_k$aufzug[data_k$type == "WK"]),
  n_na(data_k$aufzug[data_k$type == "WK"]),
  "\n"
)
cat(
  "Number of missing values in aufzug (HK):",
  n_neg(data_k$aufzug[data_k$type == "HK"]),
  n_na(data_k$aufzug[data_k$type == "HK"]),
  "\n"
)
# replace NA by 0, since it will be interacted with apartment variable
data_k$aufzug[is.na(data_k$aufzug)] <- 0

# Keller
cat("Number of missing values in keller:", n_neg(data_k$keller), n_na(data_k$keller), "\n")
cat(
  "Number of missing values in keller (WK):",
  n_neg(data_k$keller[data_k$type == "WK"]),
  n_na(data_k$keller[data_k$type == "WK"]),
  "\n"
)
cat(
  "Number of missing values in keller (HK):",
  n_neg(data_k$keller[data_k$type == "HK"]),
  n_na(data_k$keller[data_k$type == "HK"]),
  "\n"
)
# no NA replacement in notebook

# Parkplatz
cat("Number of missing values in parkplatz:", n_neg(data_k$parkplatz), n_na(data_k$parkplatz), "\n")
cat(
  "Number of missing values in parkplatz (WK):",
  n_neg(data_k$parkplatz[data_k$type == "WK"]),
  n_na(data_k$parkplatz[data_k$type == "WK"]),
  "\n"
)
cat(
  "Number of missing values in parkplatz (HK):",
  n_neg(data_k$parkplatz[data_k$type == "HK"]),
  n_na(data_k$parkplatz[data_k$type == "HK"]),
  "\n"
)
# replace <0 by 0 and create dummy variable for missing
data_k$parkplatz[data_k$parkplatz < 0] <- 0
data_k$parkplatz_missing <- as.integer(data_k$parkplatz %in% c(0))
cat("After imputation, number of missing values in parkplatz:", n_neg(data_k$parkplatz), n_na(data_k$parkplatz), "\n")

# Model setup
# Define the formula for the regression model
data_k$baujahr_bin <- relevel(factor(data_k$baujahr_bin), ref = "1991_2000")

formula <- log_price_sqm ~
  baujahr_bin +
  zimmeranzahl +
  wohnflaeche +
  ausstattung + ausstattung_missing +
  objektzustand_val + objektzustand_missing +
  apartment:balkon + apartment:aufzug + apartment:keller +
  parkplatz + parkplatz_missing +
  apartment +
  factor(kid2019)

model <- lm(formula, data = data_k)
print(summary(model))

# --- extract county fixed effects ---
params <- coef(model)
county_fe <- params[grepl("^factor\\(kid2019\\)", names(params))]

# extract as string, keep consistent
names(county_fe) <- sub("^factor\\(kid2019\\)", "", names(county_fe))

# add reference county as string too
reference_county <- names(sort(table(data_k$kid2019), decreasing = TRUE))[1]
county_fe[reference_county] <- 0.0

county_fe_df <- data.frame(
  kid2019 = names(county_fe),
  purchase_price_index = as.numeric(county_fe),
  stringsAsFactors = FALSE
)
county_fe_df$kid2019_num <- suppressWarnings(as.integer(county_fe_df$kid2019))
county_fe_df <- county_fe_df[order(county_fe_df$kid2019_num), ]
county_fe_df$purchase_price_index <- county_fe_df$purchase_price_index -
  mean(county_fe_df$purchase_price_index, na.rm = TRUE)

gc()

# Change the index of county_fe_demeaned to string
county_fe_df$kid2019 <- sprintf("%05d", county_fe_df$kid2019_num)

# Plot the data
# Join the geometries to the data
county_fe_gdf <- kreise %>%
  left_join(county_fe_df %>% select(kid2019, purchase_price_index), by = c("county_id" = "kid2019"))

p_fe <- ggplot(county_fe_gdf) +
  geom_sf(aes(fill = purchase_price_index), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c() +
  labs(title = "County Fixed Effects (Demeaned)", fill = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = file.path(plot_dir, "county_fixed_effects_R.png"),
  plot = p_fe,
  width = 12,
  height = 9,
  dpi = 300
)
print(p_fe)

# Plot the number of observations per county to check
obs_per_county <- data_k %>%
  count(kid2019, name = "count") %>%
  arrange(kid2019)

obs_per_county_gdf <- kreise %>%
  left_join(obs_per_county, by = c("county_id" = "kid2019"))

p_obs <- ggplot(obs_per_county_gdf) +
  geom_sf(aes(fill = count), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c() +
  labs(title = "Number of Observations per County", fill = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = file.path(plot_dir, "observations_per_county_R.png"),
  plot = p_obs,
  width = 12,
  height = 9,
  dpi = 300
)
print(p_obs)
