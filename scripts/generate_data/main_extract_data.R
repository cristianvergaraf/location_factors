###### Este pequeño permite genera un data frame que extrae los valores del script ######

#### load package

# setwd("~/github/location_factors")

source("config/load_packages.R")

## Set working directory

source("config/paths.R")

source("scripts/generate_data/extract_data_helper.R")


## load spatial predictor raster

variables <- list.files(
    "data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)


spatial_variables <- terra::rast(variables)


# load sample vector data

points_vect <- vect("data/processed/vector/test_gan_8715_500m.gpkg", layer = "test_lingue_8715_sin_plantaciones")


dir_salida <- "data/processed/vector"

data_extraction_from_points_to_spatial_raster(spatial_variables, points_vect,dir_salida)

