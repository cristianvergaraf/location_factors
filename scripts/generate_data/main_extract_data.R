###### Este pequeño permite genera un data frame que extrae los valores del script ######

#### load package
source("config/load_packages.R")
source("config/paths.R")
source("scripts/generate_data/extract_data_helper.R")

## load spatial predictor raster
variables <- list.files(
    "data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)

spatial_variables <- terra::rast(variables)


csv_results_path = "data/processed/csv"

vector_results_path = "data/processed/vector"

data_extraction_from_points_to_spatial_raster(spatial_variables,,"train_data_pl8715", csv_results_path, vector_results_path)


## load test sample vector data

points_sf <- st_read("data/processed/vector/test_gan_8715_500m.gpkg", layer = "test_lingue_8715_sin_plantaciones")

points_sf_32718 <- st_set_crs(points_sf,32718)

st_crs(points_sf_32718) == st_crs(spatial_variables)

csv_results_path = "data/processed/csv"

vector_results_path = "data/processed/vector"

data_extraction_from_points_to_spatial_raster(spatial_variables, points_sf_32718,"test_data_pl8715", csv_results_path, vector_results_path)




