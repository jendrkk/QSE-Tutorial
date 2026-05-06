# Generate city-level postcode shapefiles by merging Germany postcode polygons
# with GeoNames postal metadata.
# Cities are identified via GeoNames `admin code3` / official Kreis code.

suppressPackageStartupMessages({
    library(sf)
    library(data.table)
})

# -------------------------
# Paths
# -------------------------
base_dir <- "C:/Users/geppe/vsc_projects/QSE_local"
shape_path <- file.path(base_dir, "Data/Shapefiles-2022/Germany/POSTCODES/postcode_clean_final.shp")
geonames_csv <- file.path(base_dir, "Data/geonames-postal-code.csv")
out_dir <- file.path(base_dir, "Topic02/Output/1_postcode_city_shapefiles")

# -------------------------
# City specification
# admin code3 identifies the Kreis / kreisfreie Stadt.
# -------------------------
cities <- data.table(
    city = c(
        "Berlin",
        "Hamburg",
        "Muenchen",
        "Koeln",
        "Frankfurt am Main"
    ),
    admin_code3 = c(
        "11000",  # Berlin
        "02000",  # Hamburg
        "09162",  # München
        "05315",  # Köln
        "06412"   # Frankfurt am Main
    )
)

# -------------------------
# Helpers
# -------------------------
as_postal5 <- function(x) {
    x_num <- suppressWarnings(as.integer(x))
    out <- ifelse(is.na(x_num), NA_character_, sprintf("%05d", x_num))
    out
}

as_code5 <- function(x) {
    x_num <- suppressWarnings(as.integer(x))
    out <- ifelse(is.na(x_num), NA_character_, sprintf("%05d", x_num))
    out
}

safe_filename <- function(x) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- tolower(trimws(x))
    gsub("[^A-Za-z0-9_]+", "_", x)
}

# -------------------------
# Load postcode polygons
# -------------------------
pc <- st_read(shape_path, quiet = TRUE)

zip_candidates <- c("ZIP_CODE", "PLZ", "POSTCODE", "postal_code", "ZIP")
zip_col <- zip_candidates[zip_candidates %in% names(pc)][1]


if (is.na(zip_col)) {
    stop("No postcode field found in shapefile. Expected one of: ",
         paste(zip_candidates, collapse = ", "))
}

pc_dt <- as.data.table(pc)
pc_dt[, postal5 := as_postal5(get(zip_col))]
pc_dt <- pc_dt[!is.na(postal5)]

# -------------------------
# Load and prepare GeoNames CSV
# -------------------------
geo <- fread(
    geonames_csv,
    sep = ";",
    encoding = "UTF-8",
    na.strings = c("", "NA"),
    colClasses = list(character = c("country code", "postal code", "admin code3"))
)

required_cols <- c(
    "country code",
    "postal code",
    "admin name3",
    "admin code3"
)

missing_cols <- setdiff(required_cols, names(geo))

if (length(missing_cols) > 0) {
    stop("Missing columns in GeoNames CSV: ",
         paste(missing_cols, collapse = ", "))
}

# Keep only Germany
geo_de <- geo[`country code` == "DE"]

# Standardize postal code and admin code
geo_de[, postal5 := as_postal5(`postal code`)]
geo_de[, admin_code3 := as_code5(`admin code3`)]

geo_de <- geo_de[
    !is.na(postal5) & !is.na(admin_code3)
]

# Keep unique postcode-admin-code mappings
geo_de <- unique(
    geo_de[, .(
        postal5,
        city_name = `admin name3`,
        admin_code3
    )]
)

# -------------------------
# Filter GeoNames to target city codes
# -------------------------
city_postcodes <- merge(
    geo_de,
    cities,
    by = "admin_code3",
    all.x = FALSE,
    all.y = FALSE,
    allow.cartesian = TRUE
)

# Sanity check
missing_cities <- cities[
    !(admin_code3 %in% city_postcodes$admin_code3),
    city
]

if (length(missing_cities) > 0) {
    message("No GeoNames postcodes found for: ",
            paste(missing_cities, collapse = ", "))
}

# -------------------------
# Merge polygons with GeoNames city attributes
# -------------------------
merged_dt <- merge(
    pc_dt,
    city_postcodes,
    by = "postal5",
    all.x = FALSE,
    all.y = FALSE,
    allow.cartesian = TRUE
)

merged_sf <- st_as_sf(merged_dt)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------
# Export one shapefile per city
# -------------------------
for (i in seq_len(nrow(cities))) {
    city_raw <- cities$city[i]
    city_code <- cities$admin_code3[i]

    subset_sf <- merged_sf[
        merged_sf$admin_code3 == city_code,
    ]

    if (nrow(subset_sf) == 0) {
        message("No polygons found for city: ", city_raw)
        next
    }

    out_path <- file.path(
        out_dir,
        paste0("postcodes_", safe_filename(city_raw), ".shp")
    )

    st_write(subset_sf, out_path, delete_layer = TRUE, quiet = TRUE)

    message("Wrote ", nrow(subset_sf), " polygons: ", out_path)
}

# -------------------------
# Also save full merged layer for all target cities
# -------------------------
full_out <- file.path(out_dir, "postcodes_geonames_merged_target_cities.shp")

st_write(merged_sf, full_out, delete_layer = TRUE, quiet = TRUE)

message("Wrote merged target-city layer: ", full_out)

