# ==============================================================================
# TASK 1: House Price Index (Final Coordinate Fix)
# ==============================================================================
library(housepriceindex)
library(sf)
library(data.table)
library(dplyr)
library(haven)
library(readr)
library(stringr)

# -------------------------
# 1. Helpers & Numeric Logic
# -------------------------
as_postal_num <- function(x) {
    as.numeric(as.character(x))
}

clean_num <- function(x) {
    x <- as.character(x)
    x[x %in% c("Implausible value", "No information", "Not specified", "Other missing", "", "NA")] <- NA_character_
    readr::parse_number(x)
}

# -------------------------
# 2. City Specifications
# -------------------------
cities_dt <- data.table(
    city = c("Berlin", "Hamburg", "Muenchen", "Koeln", "Frankfurt am Main"),
    admin_code3 = c("11000", "02000", "09162", "05315", "06412")
)

# -------------------------
# 3. Shapefile & GeoNames (Numeric Join)
# -------------------------
pc_sf <- st_read("POSTCODES/postcode_clean_final.shp", quiet = TRUE)
geo <- fread("geonames-postal-code.csv", sep = ";",
             colClasses = list(character = c("postal code", "admin code3", "admin name3")))

setnames(geo,
         old = c("postal code", "admin name3", "admin code3"),
         new = c("postal_code", "admin_name3", "admin_code3"))

geo_filtered <- geo[admin_code3 %in% cities_dt$admin_code3, .(
    postal_num = as_postal_num(postal_code),
    city_name = admin_name3,
    admin_code3
)]

# FIXED: Ensure GeoNames has strictly unique postal codes before joining
geo_filtered <- unique(geo_filtered, by = "postal_num")

final_shapefile <- pc_sf %>%
    mutate(postal_num = as_postal_num(ZIP_CODE)) %>%
    inner_join(geo_filtered, by = "postal_num") %>%
    mutate(target_id = as.character(postal_num))

# Calculate Centroids - FIXED: Added distinct() to ensure AHS toolkit gets unique target IDs
centroids <- final_shapefile %>%
    st_centroid() %>%
    mutate(target_X = st_coordinates(.)[,1],
           target_Y = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    select(target_id, target_X, target_Y, city_name) %>%
    distinct(target_id, .keep_all = TRUE)

# -------------------------
# 4. Transaction Cleaning (No Trimming, 100m Jitter, Strict Coord Filter)
# -------------------------
DATA_PATH_PANEL <- 'Data/panel'
grid_data <- read_dta("Data/GridGeoreferences/grid.coordinaten_xy.dta")

process_submarket <- function(file_name, type, submarket_id) {
    price_col <- if(type == "rent") "mietekalt" else "kaufpreis"

    raw_df <- read_csv(file.path(DATA_PATH_PANEL, file_name), show_col_types = FALSE)

    df_clean <- raw_df %>%
        left_join(grid_data, by = "ergg_1km") %>%
        mutate(
            year = as.numeric(substr(as.character(adat), 1, 4)),
            postal_num = as_postal_num(plz),
            target_id = as.character(postal_num),
            raw_p = clean_num(.data[[price_col]]),
            space = clean_num(wohnflaeche),
            price = raw_p / space,

            # FIXED: Retained original column names for toolkit compatibility
            origin_X = origin_X + runif(n(), -100, 100),
            origin_Y = origin_Y + runif(n(), -100, 100),
            submarket = as.integer(submarket_id)
        ) %>%
        # Filter for non-NA coordinates, valid price/year, and target ZIPs
        filter(
            !is.na(origin_X), !is.na(origin_Y),
            !is.na(price), is.finite(price), price > 0,
            !is.na(year),
            postal_num %in% unique(final_shapefile$postal_num)
        ) %>%
        select(target_id, price, year, origin_X, origin_Y, submarket)

    return(df_clean)
}

trans_hk <- suppressWarnings(process_submarket("CampusFile_HK_cities.csv", "sale", 1))
trans_wk <- suppressWarnings(process_submarket("CampusFile_WK_cities.csv", "sale", 2))
trans_wm <- suppressWarnings(process_submarket("CampusFile_WM_cities.csv", "rent", 3))

# -------------------------
# 5. AHS Index Execution
# -------------------------
run_index <- function(target_centroids, transactions) {
    # FIXED: Coerce tibbles to raw data.frames so the toolkit's base R matrix logic does not fail
    target_centroids <- as.data.frame(target_centroids)
    transactions <- as.data.frame(transactions)

    valid_c <- target_centroids[target_centroids$target_id %in% unique(transactions$target_id), ]
    if(nrow(valid_c) == 0) return(NULL)

    # observations_outer and inner set to ensure convergence in Campus File
    calculate_index(valid_c, transactions,
                    observations_outer = 6000,
                    observations_inner = 600)
}

index_hk <- run_index(centroids, trans_hk)
index_wk <- run_index(centroids, trans_wk)
index_wm <- run_index(centroids, trans_wm)

# -------------------------
# 6. Save Outputs
# -------------------------
if(!is.null(index_hk)) write_dta(index_hk, "index_hk_long.dta")
if(!is.null(index_wk)) write_dta(index_wk, "index_wk_long.dta")
if(!is.null(index_wm)) write_dta(index_wm, "index_wm_long.dta")

st_write(final_shapefile, "city_postcodes_filtered.shp", delete_layer = TRUE)
