#### load package

# setwd("~/github/location_factors")

source("config/load_packages.R")

source("scripts/helpers/model_tracking.R")

## Set working directory

source("config/paths.R")

source("scripts/models/model_assessment.R")

# Load necessary spatial data

# Real gain of forest plantation 

gan_patches_8715 <- rast(gan_patches_8715_file)

plantation_1987 <- rast("data/processed/raster/validation_required_images/plantation_1987_mask_final.tif")

real_plantation_2015 <- rast("data/processed/raster/validation_required_images/real_plantation_2015.tif")


# We calculate the gain in pixels for forest plantation from 1987-2015

plantation_gain_pixel_8715 = calculate_quantity_pixel_for_category(gan_patches_8715,1)

# Import mask

lingue_mask_negative <- rast("data/processed/raster/mask/mask_lingue_1987.tif")

plantation_1987_na_mask <- rast("data/processed/raster/mask/mask_plantation_1987.tif")


lingue_mask_positive <- rast("data/processed/raster/mask/lingue_mask_positive.tif")


## load spatial data

variables <- list.files(
    "~/github/location_factors/data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)

# AUC, ROC, TOC

spatial_variables <- terra::rast(variables)

# Load models

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_20250815_094328.rds")


## simulation gains


sim_gain <- simulations_gains(model = glmulti_models@objects[[287]], spatial_predict_variables = spatial_variables,
                  pixel_number = plantation_gain_pixel_8715, original_plantation_mask = plantation_1987_na_mask)


plot(terra::predict(spatial_variables, glmulti_models@objects[[299]], type = "response"))

### I need to define a function for this stage

plot(sim_gain)

sim_plantation_2015 <- prepare_simulation_raster(sim_img = sim_gain, lingue_mask_positive = lingue_mask_positive, plantation_1987 = plantation_1987)

# FOM CALCULATION

calculate_fom(ref_img=real_plantation_2015, sim_img = sim_plantation_2015)

stack_sim <- c(real_plantation_2015, sim_plantation_2015)
cross_sim <- terra::crosstab(stack_sim)


plot(calculate_fom_image_binary(plantation_1987,real_plantation_2015,sim_plantation_2015))

figure_list <- list()

for (i in 250:260){
    sim_gain <- simulations_gains(glmulti_models@objects[[i]], spatial_variables, plantation_gain_pixel_8715, original_plantation_mask = plantation_1987_na_mask)
    sim_plantation_2015 <- prepare_simulation_raster(sim_img = sim_gain, lingue_mask_positive = lingue_mask_positive, plantation_1987 = plantation_1987)
    fom = calculate_fom(ref_img = real_plantation_2015, sim_plantation_2015)
    figure_list <- c(figure_list, list(fom))
    
}


# REPETIR EL PROCESO PARA CALCULAR LAS OTRAS MÉTRICAS ROC, AUC, TOC,
# NECESITO GUARDAR TAMBIÉN EL AIC DE CADA MODELO, LAS VARIABLES, ROC, AUC, BOLEAN.









