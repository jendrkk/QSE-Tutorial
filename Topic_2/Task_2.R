### Import libraries
library(ggplot2)
library(sf)
library(dplyr)
library(haven)
library(data.table)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Function to pivot long index data to wide format
wide_index <- function(index_long) {
    index_wide <- data.table::dcast(
        setDT(index_long),
        target_id ~ year,
        value.var = c('price', 'price_se')
    )
    return(index_wide)
}

# Flexible plotting function
plot_index_dynamic <- function(map_df, var_to_plot, plot_title, legend_title, data_type, save_path = NULL) {
    # Extract cities, ensuring we handle the spatial object correctly
    # Using st_drop_geometry to avoid metadata issues during unique extraction
    cities <- unique(st_drop_geometry(map_df)$city_name)
    cities <- cities[!is.na(cities) & cities != ""]

    if (length(cities) == 0) {
        message("--------------------------------------------------")
        message("ERROR: No valid cities found in the dataset for ", data_type, "!")
        message("Available columns in the merged data: ", paste(names(map_df), collapse = ", "))
        message("If 'city_name' is missing entirely, Task 1 failed to save it to the shapefile.")
        message("--------------------------------------------------")
        return(NULL)
    }

    if (!is.null(save_path)) {
        dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
    }

    for(city in cities) {
        city_data <- map_df %>% filter(city_name == city)

        # Check if the specific variable exists for this city/dataset
        if(!var_to_plot %in% names(city_data)) {
            message(paste("Variable", var_to_plot, "not found for", city))
            next
        }

        p <- ggplot(data = city_data) +
            geom_sf(aes(fill = .data[[var_to_plot]]), color = "white", linewidth = 0.1) +
            theme_void() +
            labs(
                title = paste(plot_title, "-", city),
                subtitle = paste("Dataset:", data_type, "| Variable:", var_to_plot)
            )

        if (grepl("change|growth|diff", var_to_plot, ignore.case = TRUE)) {
            p <- p + scale_fill_distiller(
                palette = "RdBu",
                direction = -1,
                name = legend_title,
                na.value = "grey90"
            )
        } else {
            p <- p + scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = legend_title,
                na.value = "grey90"
            )
        }

        # Print to viewer
        print(p)

        # Save to disk
        if (!is.null(save_path)) {
            safe_city_name <- gsub("[^[:alnum:]]", "_", city)
            file_name <- paste0(safe_city_name, "_", data_type, "_", var_to_plot, ".png")
            full_path <- file.path(save_path, file_name)
            ggsave(full_path, plot = p, width = 8, height = 6, dpi = 300)
            message(paste("Saved:", full_path))
        }
    }
}

# ==============================================================================
# DATA PREPARATION (Outside Functions)
# ==============================================================================

### 1. Load the data
final_shapefile <- st_read('city_postcodes_filtered.shp')

# Use base R for renaming to avoid the "can't find agr columns" error in sf/dplyr
names(final_shapefile) <- tolower(names(final_shapefile))

# SMART FALLBACK: Fix 'city_name'
if (!"city_name" %in% names(final_shapefile)) {
    # Try 1: Look for common column name fragments
    possible_city <- grep("city|plac|nam|admin", names(final_shapefile), value = TRUE)
    if (length(possible_city) > 0) {
        names(final_shapefile)[names(final_shapefile) == possible_city[1]] <- "city_name"
    } else {
        # Try 2: Deep scan the actual row contents for city names!
        content_match <- sapply(st_drop_geometry(final_shapefile), function(col) {
            any(grepl("Berlin|Hamburg|München|Muenchen|Köln|Koeln|Frankfurt", as.character(col), ignore.case = TRUE))
        })
        if (any(content_match, na.rm = TRUE)) {
            found_col <- names(final_shapefile)[which(content_match)[1]]
            names(final_shapefile)[names(final_shapefile) == found_col] <- "city_name"
        }
    }
}

# SMART FALLBACK: Fix 'target_id'
if (!"target_id" %in% names(final_shapefile)) {
    possible_id <- grep("target|zip|plz|post", names(final_shapefile), value = TRUE)
    if (length(possible_id) > 0) {
        names(final_shapefile)[names(final_shapefile) == possible_id[1]] <- "target_id"
    } else {
        names(final_shapefile)[1] <- "target_id" # Last resort: pick the first column
    }
}

index_wm <- read_dta('index_wm_long.dta')
index_wk <- read_dta('index_wk_long.dta')
index_hk <- read_dta('index_hk_long.dta')

### 2. Transform to Wide and Create Variables
process_dataset <- function(dt_long) {
    if(is.null(dt_long) || nrow(dt_long) == 0) return(NULL)

    dt_wide <- wide_index(dt_long)
    price_cols <- grep("^price_\\d{4}$", names(dt_wide), value = TRUE)

    for (col in price_cols) {
        log_name <- paste0("log_", col)
        dt_wide[, (log_name) := log(get(col))]
    }

    price_mat <- as.matrix(dt_wide[, ..price_cols])
    log_price_mat <- log(price_mat)
    log_diffs <- log_price_mat[, 2:ncol(log_price_mat)] - log_price_mat[, 1:(ncol(log_price_mat) - 1)]
    dt_wide[, ave_annual_change := rowMeans(log_diffs, na.rm = TRUE)]

    return(dt_wide)
}

index_wm_p <- process_dataset(index_wm)
index_wk_p <- process_dataset(index_wk)
index_hk_p <- process_dataset(index_hk)

### 3. Merge with Spatial Data (Robust Approach)
merge_spatial <- function(shape, processed_dt) {
    if(is.null(processed_dt)) return(NULL)

    # To bypass 'agr' errors, we strip geometry, join as dataframes, then restore geometry
    geom <- st_geometry(shape)
    attr_only <- st_drop_geometry(shape)

    joined_df <- attr_only %>%
        mutate(target_id = as.character(target_id)) %>%
        left_join(processed_dt, by = "target_id")

    # Re-attach the original geometry to the joined attributes
    st_set_geometry(joined_df, geom)
}

map_data_wm <- merge_spatial(final_shapefile, index_wm_p)
map_data_wk <- merge_spatial(final_shapefile, index_wk_p)
map_data_hk <- merge_spatial(final_shapefile, index_hk_p)

# ==============================================================================
# TASK 2: VISUALIZATION
# ==============================================================================

# Note: Using log_price_2020 assumes your data goes up to 2020.
# If it ends earlier, replace with the latest available year (e.g., log_price_2018).

if(!is.null(map_data_wm)) {
    plot_index_dynamic(map_data_wm, "log_price_2020", "Rental Log-Levels", "ln(Price)", "Rent", "Plots/Levels/")
    plot_index_dynamic(map_data_wm, "ave_annual_change", "Mean Annual Rental Change", "Avg Delta ln(Price)", "Rent", "Plots/Changes/")
}

if(!is.null(map_data_wk)) {
    plot_index_dynamic(map_data_wk, "log_price_2020", "Apartment Log-Levels", "ln(Price)", "Apartment", "Plots/Levels/")
    plot_index_dynamic(map_data_wk, "ave_annual_change", "Mean Annual Apartment Change", "Avg Delta ln(Price)", "Apartment", "Plots/Changes/")
}

if(!is.null(map_data_hk)) {
    plot_index_dynamic(map_data_hk, "log_price_2020", "House Log-Levels", "ln(Price)", "House", "Plots/Levels/")
    plot_index_dynamic(map_data_hk, "ave_annual_change", "Mean Annual House Change", "Avg Delta ln(Price)", "House", "Plots/Changes/")
}
