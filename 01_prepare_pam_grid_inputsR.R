# ============================================================
# Atlantic Forest pollination project
#
# Purpose
# - Read local files stored in data_raw/.
# - Join PAM crop production data with the pollination-dependence reference table.
# - Create municipal pollination-demand files.
# - Create the 5-km Atlantic Forest grid used in the final analysis.
#
# Important
# - Run this script from the project root, i.e. the folder that contains data_raw/.
# ============================================================

# ------------------------------------------------------------
# 0) Packages
# ------------------------------------------------------------
required_packages <- c("sf", "dplyr", "stringr", "readr", "readxl", "janitor")

invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------
# 1) Paths and parameters
# ------------------------------------------------------------
target_years <- 2020:2024
project_crs <- 5880  # SIRGAS 2000 / Brazil Polyconic

setwd("~/output_mata_atlantica")
raw_dir <- "data_raw"
output_dir <- "output_mata_atlantica"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Canonical input names expected in data_raw/.
biome_file <- file.path(raw_dir, "atlantic_forest_biome_2019.gpkg")
municipalities_file <- file.path(raw_dir, "atlantic_forest_municipalities_clip_2020.gpkg")
pam_long_file <- file.path(raw_dir, "pam_atlantic_forest_long_2020_2024.csv")
dependence_file <- file.path(raw_dir, "pollination_dependence_reference.xlsx")
mapbiomas_grid_file <- file.path(raw_dir, "mapbiomas_grid_atlantic_forest_2024.csv")

required_files <- c(
  biome_file,
  municipalities_file,
  pam_long_file,
  dependence_file,
  mapbiomas_grid_file
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "The following input files were not found. Check data_raw/ and file names:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 2) Read local spatial inputs
# ------------------------------------------------------------
atlantic_forest <- sf::st_read(biome_file, quiet = TRUE) %>%
  janitor::clean_names() %>%
  sf::st_make_valid() %>%
  sf::st_transform(project_crs) %>%
  dplyr::mutate(biome_name = "Mata Atlantica")

municipalities_af <- sf::st_read(municipalities_file, quiet = TRUE) %>%
  janitor::clean_names() %>%
  sf::st_make_valid() %>%
  sf::st_transform(project_crs)

# Standardize municipality code column for joins.
if ("municipality_code" %in% names(municipalities_af)) {
  municipalities_af <- municipalities_af %>%
    dplyr::mutate(municipality_code = as.character(municipality_code))
} else if ("codigo_municipio" %in% names(municipalities_af)) {
  municipalities_af <- municipalities_af %>%
    dplyr::mutate(municipality_code = as.character(codigo_municipio))
} else if ("code_muni" %in% names(municipalities_af)) {
  municipalities_af <- municipalities_af %>%
    dplyr::mutate(municipality_code = as.character(code_muni))
} else {
  stop("The municipality layer must contain 'municipality_code', 'codigo_municipio', or 'code_muni'.")
}

# State boundaries are derived from clipped municipalities.
state_boundaries_af <- municipalities_af %>%
  dplyr::group_by(code_state, abbrev_state) %>%
  dplyr::summarise(.groups = "drop")

sf::st_write(
  state_boundaries_af,
  file.path(output_dir, "atlantic_forest_state_boundaries_2020.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# 3) Read PAM data and pollination-dependence reference table
# ------------------------------------------------------------
pam_long <- readr::read_csv(pam_long_file, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::transmute(
    year = as.integer(ano),
    crop_type = as.character(tipo_lavoura),
    municipality_code = stringr::str_extract(as.character(codigo_municipio), "\\d{7}"),
    municipality_name = as.character(municipio),
    crop_code = as.character(produto_codigo),
    crop_name = as.character(produto),
    variable_code = as.character(variavel_codigo),
    variable_name = as.character(variavel),
    value = as.numeric(valor)
  ) %>%
  dplyr::filter(year %in% target_years)

dependence_ref <- readxl::read_excel(dependence_file) %>%
  janitor::clean_names() %>%
  dplyr::transmute(
    crop_code = as.character(ibge_crop_code),
    crop_name_english = as.character(crop_name_english),
    pam_crop_name_reference = as.character(original_pam_crop_name),
    pollination_dependence = as.character(pollination_dependence),
    dependence_score = as.numeric(dependence_score),
    dependence_reference = as.character(reference)
  )

# ------------------------------------------------------------
# 4) Check dependence-class coverage
# ------------------------------------------------------------
pam_products <- pam_long %>%
  dplyr::distinct(crop_code, crop_name) %>%
  dplyr::arrange(crop_name)

product_dependency_audit <- pam_products %>%
  dplyr::left_join(dependence_ref, by = "crop_code") %>%
  dplyr::mutate(has_dependence_class = !is.na(dependence_score))

products_missing_dependence <- product_dependency_audit %>%
  dplyr::filter(!has_dependence_class)

readr::write_csv(
  product_dependency_audit,
  file.path(output_dir, "product_dependency_audit.csv")
)

readr::write_csv(
  products_missing_dependence,
  file.path(output_dir, "products_missing_dependence.csv")
)

cat("\nPollination-dependence coverage:\n")
print(table(product_dependency_audit$has_dependence_class, useNA = "ifany"))

# ------------------------------------------------------------
# 5) Join dependence classes to PAM records
# ------------------------------------------------------------
pam_with_dependence <- pam_long %>%
  dplyr::filter(crop_code != "0") %>%
  dplyr::left_join(dependence_ref, by = "crop_code")

readr::write_csv(
  pam_with_dependence,
  file.path(output_dir, "pam_atlantic_forest_long_2020_2024_dependence.csv")
)

# ------------------------------------------------------------
# 6) Select production metric used to estimate pollination demand
# ------------------------------------------------------------
production_rows <- pam_with_dependence %>%
  dplyr::mutate(
    variable_name_clean = variable_name %>%
      stringr::str_to_lower() %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
      stringr::str_replace_all("[^[:alnum:]]+", " ") %>%
      stringr::str_squish()
  ) %>%
  dplyr::filter(stringr::str_detect(variable_name_clean, "quantidade produzida"))

pollinator_dependent_production <- production_rows %>%
  dplyr::filter(!is.na(dependence_score), dependence_score > 0) %>%
  dplyr::mutate(weighted_production = value * dependence_score)

# ------------------------------------------------------------
# 7) Municipal pollination demand
# ------------------------------------------------------------
municipal_demand_by_year <- pollinator_dependent_production %>%
  dplyr::group_by(year, municipality_code, municipality_name) %>%
  dplyr::summarise(
    gross_production = sum(value, na.rm = TRUE),
    weighted_pollination_demand = sum(weighted_production, na.rm = TRUE),
    crop_count = dplyr::n_distinct(crop_code),
    .groups = "drop"
  )

municipal_demand_mean <- municipal_demand_by_year %>%
  dplyr::group_by(municipality_code, municipality_name) %>%
  dplyr::summarise(
    gross_production_mean_2020_2024 = mean(gross_production, na.rm = TRUE),
    weighted_demand_mean_2020_2024 = mean(weighted_pollination_demand, na.rm = TRUE),
    crop_count_mean_2020_2024 = mean(crop_count, na.rm = TRUE),
    .groups = "drop"
  )

municipal_demand_2024_for_gee <- municipal_demand_by_year %>%
  dplyr::filter(year == 2024) %>%
  dplyr::select(
    municipality_code,
    municipality_name,
    gross_production,
    weighted_pollination_demand,
    crop_count
  )

readr::write_csv(
  municipal_demand_mean,
  file.path(output_dir, "municipal_pollination_demand_mean_2020_2024.csv")
)

readr::write_csv(
  municipal_demand_2024_for_gee,
  file.path(output_dir, "municipal_pollination_demand_2024_for_gee.csv")
)

# Quick check
municipalities_demand_check <- municipalities_af %>%
  dplyr::left_join(municipal_demand_2024_for_gee, by = "municipality_code")

cat("\nMunicipal demand 2024 summary:\n")
print(summary(municipal_demand_2024_for_gee$weighted_pollination_demand))
cat(
  "Municipalities with valid demand:",
  sum(!is.na(municipalities_demand_check$weighted_pollination_demand)),
  "\n"
)
cat(
  "Municipalities with missing demand:",
  sum(is.na(municipalities_demand_check$weighted_pollination_demand)),
  "\n"
)

# ------------------------------------------------------------
# 8) Create the 5-km Atlantic Forest grid
# ------------------------------------------------------------
grid_5km <- sf::st_make_grid(
  atlantic_forest,
  cellsize = 5000,
  square = TRUE,
  what = "polygons"
)

grid_5km_af <- sf::st_sf(
  id_grid = seq_along(grid_5km),
  geometry = grid_5km
) %>%
  sf::st_intersection(atlantic_forest) %>%
  dplyr::mutate(
    grid_area_m2 = as.numeric(sf::st_area(geometry)),
    grid_area_km2 = grid_area_m2 / 1e6
  )

sf::st_write(
  grid_5km_af,
  file.path(output_dir, "atlantic_forest_grid_5km.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)


