# ==============================================================================
# TASK 1: House Price Index (Corrected Parameters & Submarkets)
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
pc_sf <- st_read("Data_/Shapefiles/postcode_clean_final.shp", quiet = TRUE)
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

geo_filtered <- unique(geo_filtered, by = "postal_num")

final_shapefile <- pc_sf %>%
    mutate(postal_num = as_postal_num(ZIP_CODE)) %>%
    inner_join(geo_filtered, by = "postal_num") %>%
    mutate(target_id = as.character(postal_num))

# Calculate Centroids
centroids <- final_shapefile %>%
    st_centroid() %>%
    mutate(target_X = st_coordinates(.)[,1],
           target_Y = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    select(target_id, target_X, target_Y, city_name) %>%
    distinct(target_id, .keep_all = TRUE)

# -------------------------
# 4. Transaction Cleaning (Jitter, City Submarkets)
# -------------------------
DATA_PATH_PANEL <- 'Data_/panel'
grid_data <- read_dta("Data_/GridGeoreferences/grid.coordinaten_xy.dta")

process_submarket <- function(file_name, type) {
    price_col <- if(type == "rent") "mietekalt" else "kaufpreis"

    raw_df <- read_csv(file.path(DATA_PATH_PANEL, file_name), show_col_types = FALSE)

    df_clean <- raw_df %>%
        mutate(postal_num = as_postal_num(plz)) %>%
        # Join with our filtered cities to ensure we only keep targets in our 5 cities
        # and attach the city_name so we can define isolated submarkets!
        inner_join(geo_filtered %>% select(postal_num, city_name), by = "postal_num") %>%
        left_join(grid_data, by = "ergg_1km") %>%
        mutate(
            year = as.numeric(substr(as.character(adat), 1, 4)),
            target_id = as.character(postal_num),
            raw_p = clean_num(.data[[price_col]]),
            space = clean_num(wohnflaeche),
            price = raw_p / space,

            origin_X = origin_X + runif(n(), -100, 100),
            origin_Y = origin_Y + runif(n(), -100, 100),

            # FIX: Submarkets should separate non-continuous markets.
            # We map the 5 cities into factors 1 through 5.
            submarket = as.integer(as.factor(city_name))
        ) %>%
        filter(
            !is.na(origin_X), !is.na(origin_Y),
            !is.na(price), is.finite(price), price > 0,
            !is.na(year)
        ) %>%
        select(target_id, price, year, origin_X, origin_Y, submarket)

    return(df_clean)
}

# Notice we dropped the submarket_id argument since it's handled dynamically now.
trans_hk <- suppressWarnings(process_submarket("CampusFile_HK_cities.csv", "sale"))
trans_wk <- suppressWarnings(process_submarket("CampusFile_WK_cities.csv", "sale"))
trans_wm <- suppressWarnings(process_submarket("CampusFile_WM_cities.csv", "rent"))

# -------------------------
# 5. AHS Index Execution
# -------------------------
run_index <- function(target_centroids, transactions) {
    target_centroids <- as.data.frame(target_centroids)
    transactions <- as.data.frame(transactions)

    valid_c <- target_centroids[target_centroids$target_id %in% unique(transactions$target_id), ]
    if(nrow(valid_c) == 0) return(NULL)

    submarkets <- unique(transactions$submarket)
    index_results <- list()

    for(sm in submarkets) {
        sm_trans <- transactions[transactions$submarket == sm, ]
        sm_targets <- valid_c[valid_c$target_id %in% unique(sm_trans$target_id), ]

        if(nrow(sm_targets) == 0) next

        total_obs <- nrow(sm_trans)

        # Target 5% for outer, 0.5% for inner, while enforcing minimum bounds
        # to prevent the algorithm from failing on very small cities/samples.
        dynamic_outer <- max(500, floor(total_obs * 0.1))
        dynamic_inner <- max(100, floor(total_obs * 0.01))

        message(sprintf("Calculating index for submarket %d: %d outer, %d inner observations", sm, dynamic_outer, dynamic_inner))

        sm_index <- calculate_index(sm_targets, sm_trans,
                                    observations_outer = dynamic_outer,
                                    observations_inner = dynamic_inner,
                                    max_radius_outer = 30,
                                    skip_errors = TRUE)

        index_results[[as.character(sm)]] <- sm_index
    }

    # Combine all city results back into a single dataframe
    return(bind_rows(index_results))
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
