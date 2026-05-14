### Task 3 ###

# Import packages
library(ggplot2)
library(sf)
library(dplyr)
library(haven)
library(data.table)
library(stringr)
library(tidyr)
library(stargazer) # <-- Added for LaTeX tables

# Global Settings to protect RAM
options(mc.cores = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)

# =========================================================
# 1. IMPORT & WIDE TRANSFORMATION
# =========================================================
# Load datasets into a named list for efficient processing
raw_datasets <- list(
    hk = read_dta('index_hk_long.dta'),
    wk = read_dta('index_wk_long.dta'),
    wm = read_dta('index_wm_long.dta')
)

wide_index <- function(index_long) {
    index_wide_tran <- data.table::dcast(
        setDT(index_long),
        target_id + target_X + target_Y ~ year,
        value.var = c('price', 'price_se', 'lprice', 'lprice_se')
    )

    index_wide <- index_wide_tran %>%
        rename(plz = target_id) %>%
        mutate(plz = as.character(plz))

    return(index_wide)
}

# Apply transformation to all datasets simultaneously
wide_datasets <- lapply(raw_datasets, wide_index)

# =========================================================
# 2. SPATIAL DATA MERGE
# =========================================================
geo <- fread("geonames-postal-code.csv", sep = ";",
             colClasses = list(character = c("postal code", "place name", "country code", "coordinates")))

geo_clean <- geo %>%
    rename(
        plz = `postal code`,
        city = `place name`,
        country_code = `country code`,
        coordinates = coordinates
    ) %>%
    filter(country_code == "DE") %>%
    mutate(
        city = case_when(
            str_detect(tolower(city), "berlin")    ~ "berlin",
            str_detect(tolower(city), "hamburg")   ~ "hamburg",
            str_detect(tolower(city), "frankfurt") ~ "frankfurt",
            str_detect(tolower(city), "münchen")   ~ "munich",
            TRUE ~ city
        )
    ) %>%
    filter(city %in% c("berlin","hamburg", "frankfurt", "munich")) %>%
    select(plz, city)

# Join spatial data efficiently across the list
city_datasets <- lapply(wide_datasets, function(df) {
    left_join(df, geo_clean, by = 'plz') %>% distinct(plz, .keep_all = TRUE)
})

# =========================================================
# 3. PRIME LOCATION DISTANCES
# =========================================================
sh_berlin       <- st_read("prime_locations/PL_11.shp", quiet = TRUE) %>% mutate(city = "berlin")
sh_munich       <- st_read("prime_locations/PL_75.shp", quiet = TRUE) %>% mutate(city = "munich")
sh_frankfurt    <- st_read("prime_locations/PL_37.shp", quiet = TRUE) %>% mutate(city = "frankfurt")
sh_hamburg      <- st_read("prime_locations/PL_43.shp", quiet = TRUE) %>% mutate(city = "hamburg")

prime_locations_stacked <- bind_rows(sh_berlin, sh_munich, sh_frankfurt, sh_hamburg)

pl_final <- prime_locations_stacked %>%
    st_transform(crs = 25832) %>% # Aligning UTM zones to 32N to match index data
    st_centroid() %>%
    mutate(
        pl_x = st_coordinates(.)[,1],
        pl_y = st_coordinates(.)[,2]
    ) %>%
    st_drop_geometry() %>%
    group_by(city) %>%
    mutate(pl_number = row_number()) %>%
    ungroup()

pl_A <- pl_final %>% group_by(city) %>% slice(1) %>% ungroup()
pl_B <- pl_final %>% group_by(city) %>% slice(n()) %>% ungroup()

index_merge <- function(index, prime_locations){
    left_join(index, prime_locations, by = 'city', relationship = "many-to-many") %>%
        # Convert the distance from meters to kilometers
        mutate(distance = sqrt((target_X - pl_x)^2 + (target_Y - pl_y)^2) / 1000)
}

# Merge A and B prime locations across all datasets
data_A <- lapply(city_datasets, index_merge, prime_locations = pl_A)
data_B <- lapply(city_datasets, index_merge, prime_locations = pl_B)


# =========================================================
# 4. PANEL/TREND LONG FORMAT PREPARATION
# =========================================================
index_long <- function(index_data) {
    long_data <- index_data %>%
        pivot_longer(
            cols = matches("\\d{4}$"),
            names_to = c(".value", "year"),
            names_pattern = "(.*)_(\\d{4})"
        ) %>%
        mutate(year = as.numeric(year)) %>%
        filter(!is.na(lprice))

    return(long_data)
}

# Apply long transformation in bulk
long_data_A <- lapply(data_A, index_long)
long_data_B <- lapply(data_B, index_long)


# =========================================================
# 5. VISUALIZATIONS & LATEX TABLES
# =========================================================
# Combine all data into one master dataset for ggplot to color naturally
# Pluralized market labels and mapped as factors to ensure strict plot order
combined_data_A <- bind_rows(
    long_data_A$hk %>% mutate(market = "Houses"),
    long_data_A$wk %>% mutate(market = "Apartments"),
    long_data_A$wm %>% mutate(market = "Rents")
) %>%
    mutate(market = factor(market, levels = c("Houses", "Apartments", "Rents")))

# Function: Flexible Table Generator (LaTeX or Text for Markdown)
generate_table <- function(list_long_A, target_city, model_type = "gradient", target_year = 2022, base_year = 2007, output_format = "latex") {

    # Filter datasets dynamically AND fix the "Year Zero" issue by centering on 2007
    df_hk <- list_long_A$hk %>% filter(city == target_city) %>% mutate(year_centered = year - base_year)
    df_wk <- list_long_A$wk %>% filter(city == target_city) %>% mutate(year_centered = year - base_year)
    df_wm <- list_long_A$wm %>% filter(city == target_city) %>% mutate(year_centered = year - base_year)

    if(model_type == "gradient") {
        df_hk <- df_hk %>% filter(year == target_year)
        df_wk <- df_wk %>% filter(year == target_year)
        df_wm <- df_wm %>% filter(year == target_year)
        form <- as.formula(lprice ~ distance)
        cov_labels <- c("Distance (km)")
    } else {
        # CRITICAL FIX: Use the centered year for the trend formula
        form <- as.formula(lprice ~ distance * year_centered)
        cov_labels <- c("Distance (km)", paste0("Year (Centered on ", base_year, ")"), "Distance (km) x Year")
    }

    mod_hk <- lm(form, data = df_hk)
    mod_wk <- lm(form, data = df_wk)
    mod_wm <- lm(form, data = df_wm)

    stargazer(
        mod_hk, mod_wk, mod_wm,
        type = output_format,
        dep.var.caption = "", # Removes "Dependent variable:"
        column.labels = c("Houses", "Apartments", "Rents"),
        dep.var.labels = paste("Log Price (per sqm)", str_to_title(target_city)),
        covariate.labels = cov_labels,
        omit = "Constant", # Removes the intercept
        omit.stat = c("f", "ser"),
        star.cutoffs = c(0.05, 0.01, 0.001),
        header = FALSE # Removes the "% Table created by stargazer" comment
    )
}

# Function: Cross-Sectional Scatter Plot
plot_city_scatter <- function(combined_df, target_city, target_year = 2022) {
    city_data <- combined_df %>% filter(city == target_city, year == target_year)

    p <- ggplot(city_data, aes(x = distance, y = lprice, color = market)) +
        geom_point(alpha = 0.5, size = 1.5) +
        geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
        facet_wrap(~ market, scales = "free_y") +
        theme_minimal() +
        scale_color_brewer(palette = "Set1") +
        labs(
            title = paste("Price Distance Gradients -", str_to_title(target_city)),
            subtitle = paste("Year:", target_year, "| OLS line represents distance penalty"),
            x = "Distance to Prime Location (km)",
            y = "Log Price per Sqm"
        ) +
        theme(
            legend.position = "none",
            axis.line = element_line(color = "black")
        )

    return(p)
}

# Function: Gradient Change Over Time Plot
plot_gradient_over_time <- function(combined_df, target_city) {
    yearly_gradients <- combined_df %>%
        filter(city == target_city) %>%
        group_by(market, year) %>%
        summarise(
            # Calculate the gradient per year for the plot (inherently avoids Year 0 problem)
            gradient = coef(lm(lprice ~ distance))["distance"],
            std_error = summary(lm(lprice ~ distance))$coefficients["distance", "Std. Error"],
            .groups = 'drop'
        )

    p <- ggplot(yearly_gradients, aes(x = year, y = gradient, color = market)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = gradient - 1.96*std_error, ymax = gradient + 1.96*std_error), width = 0.2) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed", alpha = 0.5) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        theme_minimal() +
        scale_color_brewer(palette = "Set1") +
        labs(
            title = paste("Evolution of the Distance Gradient -", str_to_title(target_city)),
            subtitle = "Dashed line = Overall Trend | Horizontal Line = No Distance Penalty",
            x = "Year",
            y = "Distance Coefficient (Price change per km)",
            color = "Market"
        )

    return(p)
}

# --- EXECUTE AND SAVE PLOTS / TABLES ---
dir.create("Plots_Results", showWarnings = FALSE)
cat("\nGenerating Visualizations, LaTeX, and Markdown Tables...\n")

# Clear the output files if they exist
cat("% --- LATEX TABLES FOR ALL CITIES ---\n\n", file = "Plots_Results/latex_tables_master.tex")
cat("# Regression Results\n\nThis file contains the plain-text markdown tables for easy review.\n\n", file = "Plots_Results/markdown_tables_master.md")

city_names <- c("berlin", "hamburg", "frankfurt", "munich")

for (c_name in city_names) {
    # 1. Scatter Plot (2022)
    p_scatter <- plot_city_scatter(combined_data_A, c_name, 2022)
    ggsave(paste0("Plots_Results/Scatter_2022_", c_name, ".png"), plot = p_scatter, width = 10, height = 4)

    # 2. Trend Plot (All Years)
    p_trend <- plot_gradient_over_time(combined_data_A, c_name)
    ggsave(paste0("Plots_Results/Trend_Gradient_", c_name, ".png"), plot = p_trend, width = 8, height = 5)

    # 3. Table Output: Gradients
    # ---> LaTeX Output
    capture.output(
        generate_table(long_data_A, c_name, "gradient", target_year = 2022, output_format = "latex"),
        file = "Plots_Results/latex_tables_master.tex", append = TRUE
    )
    # ---> Markdown Output
    cat(paste("\n\n### Distance Gradients:", str_to_title(c_name), "(2022)\n```text\n"), file = "Plots_Results/markdown_tables_master.md", append = TRUE)
    capture.output(
        generate_table(long_data_A, c_name, "gradient", target_year = 2022, output_format = "text"),
        file = "Plots_Results/markdown_tables_master.md", append = TRUE
    )
    cat("\n```\n", file = "Plots_Results/markdown_tables_master.md", append = TRUE)

    # 4. Table Output: Trends
    # ---> LaTeX Output
    capture.output(
        generate_table(long_data_A, c_name, "trend", base_year = 2007, output_format = "latex"),
        file = "Plots_Results/latex_tables_master.tex", append = TRUE
    )
    # ---> Markdown Output
    cat(paste("\n\n### Gradient Trends over Time:", str_to_title(c_name), "\n```text\n"), file = "Plots_Results/markdown_tables_master.md", append = TRUE)
    capture.output(
        generate_table(long_data_A, c_name, "trend", base_year = 2007, output_format = "text"),
        file = "Plots_Results/markdown_tables_master.md", append = TRUE
    )
    cat("\n```\n", file = "Plots_Results/markdown_tables_master.md", append = TRUE)
}

cat("Finished! Check the 'Plots_Results' folder for your graphs, LaTeX code, and Markdown document.\n")
