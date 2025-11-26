####### load package ##############


## setwd("~/github/location_factors")

# Import packages
source("config/load_packages.R")

# Import model tracking function
source("scripts/helpers/model_tracking.R")

## Set working directory

# Import paths
source("config/paths.R")

# Import models assessment functions
source("scripts/models/model_assessment.R")

# Load necessary spatial data

# Real gain of forest plantation 

real_plantation_gains_8715 <- rast(gan_patches_8715_file)

datatype(real_plantation_gains_8715)
plot(real_plantation_gains_8715)

# Real forest plantation in 1987

plantation_1987 <- rast("data/processed/raster/validation_required_images/plantation_1987_mask_final.tif")

datatype(plantation_1987)

plot(plantation_1987)

real_plantation_2015 <- rast("data/processed/raster/validation_required_images/real_plantation_2015.tif")

plot(real_plantation_2015)

datatype(real_plantation_2015)

# We calculate the gain in pixels for forest plantation from 1987-2015
# Quantity of change

plantation_gain_pixel_8715 = calculate_quantity_pixel_for_category(real_plantation_gains_8715,1)

# Import mask

lingue_mask_negative <- rast("data/processed/raster/mask/mask_lingue_1987.tif")

datatype(lingue_mask_negative)

plot(lingue_mask_negative)

# Import plantation na mask
plantation_1987_na_mask <- rast("data/processed/raster/mask/mask_plantation_1987.tif")

datatype(plantation_1987_na_mask)

plot(plantation_1987_na_mask)

## Import positive mask
lingue_mask_positive <- rast("data/processed/raster/mask/lingue_mask_positive.tif")

datatype(lingue_mask_positive)

plot(lingue_mask_positive)

## load spatial data

variables <- list.files(
    "~/github/location_factors/data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)

## Load spatial predictions

spatial_variables <- terra::rast(variables)

# Load models

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_all_20250926_192822.rds")

# Choose a model

glmulti_models@objects[[1021]]

## Iterative process to compute and save spatial and non spatial metrics for each model

datatype(spatial_variables[[12]])

### Function to calculate spatial ROC from real expansion and simulated expansion

### Generar mapas

# Images are

plot(plantation_1987)
plot(real_plantation_gains_8715)
plot(real_plantation_2015)
plot(plantation_gain_pixel_8715)
plot(lingue_mask_negative)
plot(plantation_1987_na_mask)
plot(plantation_gain_pixel_8715)
plot(lingue_mask_positive)
plot(spatial_variables)

# Pixel predictions

pred_plantation_gains_8715 <- terra::predict(spatial_variables, glmulti_models@objects[[320]], type = "response")

plot(pred_plantation_gains_8715)


### Apply simulations gains 

sim_gain <- simulations_gains(
    glmulti_models@objects[[380]],
    spatial_variables,
    plantation_gain_pixel_8715,
    original_plantation_mask = plantation_1987_na_mask
)

plot(sim_gain)


for (i in (1:1204)){
    sim <- simulations_gains(
        glmulti_models@objects[[i]],
        spatial_variables,
        plantation_gain_pixel_8715,
        original_plantation_mask = plantation_1987_na_mask
    )
}



#### Esto se podría aplicar para calcular todas las simulaciones y guardarlas como imagen 
#### en una carpeta

# Build plantation total for 2015
sim_plantation_2015 <- prepare_simulation_raster(
    sim_img = sim_gain,
    lingue_mask_positive = lingue_mask_positive,
    plantation_1987 = plantation_1987
)

## The sim_plantation_2015 image has not NA values

plot(sim_plantation_2015)

### Compute FOM image from real_plantation_gains_8715
plot(real_plantation_gains_8715)

### Should we probable standarize NA VALUES before calculating the image

fom = calculate_figure_of_merits_image_2(real_plantation_gains_8715, sim_plantation_2015)

plot(fom)

# Create a vector with images results
images_results = c(real_plantation_gains_8715, sim_plantation_2015, fom)

plot(images_results)

plot(sim_plantation_2015)

# Standarize images is a necesary function before calculating TOC_VALUES

standarize_image <- image_standarization_function(pred_plantation_gains_8715,sim_plantation_2015,lingue_mask_positive)

plot(standarize_image)

TOC_VALUE <- TOC::TOC(standarize_image[["pred_plantation_gains"]],standarize_image[["sim_plantation_2015"]],mask= standarize_image[["lingue_mask_positive"]], nthres = 100)

TOC_VALUE

TOC_AUC = TOC_VALUE@AUC

TOC_AUC

