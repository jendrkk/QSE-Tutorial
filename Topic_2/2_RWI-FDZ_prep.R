library(readr)
library(dplyr)
library(haven)

base_dir <- "C:/Users/geppe/vsc_projects/QSE_local"

HK_path <- file.path(base_dir, "Data/RWI-GEO-RED-2022/panel/CampusFile_HK_cities.csv")
WK_path <- file.path(base_dir, "Data/RWI-GEO-RED-2022/panel/CampusFile_WK_cities.csv")
WM_path <- file.path(base_dir, "Data/RWI-GEO-RED-2022/panel/CampusFile_WM_cities.csv")

coords_path <- file.path(
    base_dir,
    "Data/RWI-GEO-RED-2022/_grid_georeferences/grid.coordinaten_xy.dta"
)

coords <- read_dta(coords_path) %>%
    mutate(ergg_1km = as.character(ergg_1km))

clean_num <- function(x) {
    x <- as.character(x)
    x[x %in% c(
        "Implausible value",
        "No information",
        "Not specified",
        "Other missing",
        "",
        "NA"
    )] <- NA_character_

    parse_number(x)
}

load_campus <- function(path, submarket_id, price_var) {

    df <- read_csv(path, show_col_types = FALSE) %>%
        mutate(across(everything(), as.character))

    df %>%
        mutate(
            ergg_1km = as.character(ergg_1km),
            year = 1L,
            submarket = submarket_id,
            price = clean_num(.data[[price_var]])
        )
}

hk <- load_campus(HK_path, 1L, "price_sqm")
wk <- load_campus(WK_path, 2L, "price_sqm")
wm <- load_campus(WM_path, 3L, "rent_sqm")

panel <- bind_rows(hk, wk, wm)

panel_geo <- panel %>%
    left_join(coords, by = "ergg_1km")

transactions <- panel_geo %>%
    filter(
        !is.na(year),
        !is.na(submarket),
        !is.na(price),
        price > 0,
        !is.na(origin_X),
        !is.na(origin_Y)
    )

targets <- coords %>%
    transmute(
        target_id = ergg_1km,
        target_X = origin_X,
        target_Y = origin_Y
    ) %>%
    distinct(target_id, .keep_all = TRUE) %>%
    filter(
        !is.na(target_id),
        !is.na(target_X),
        !is.na(target_Y)
    )

stopifnot(nrow(targets) == n_distinct(targets$target_id))

out_dir <- file.path(base_dir, "Data/RWI-GEO-RED-2022/ahs_input")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(transactions, file.path(out_dir, "transactions.csv"))
write_csv(targets, file.path(out_dir, "targets.csv"))
