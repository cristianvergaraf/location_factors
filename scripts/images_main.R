#### load package

# setwd("~/github/location_factors")

source("config/load_packages.R")

source("scripts/helpers/model_tracking.R")

## Set working directory

source("config/paths.R")

source("scripts/models/model_assessment.R")

# Load necessary spatial data

# Real gain of forest plantation 

real_plantation_gains_8715 <- rast(gan_patches_8715_file)

datatype(real_plantation_gains_8715)
plot(real_plantation_gains_8715)

plantation_1987 <- rast("data/processed/raster/validation_required_images/plantation_1987_mask_final.tif")

datatype(plantation_1987)

real_plantation_2015 <- rast("data/processed/raster/validation_required_images/real_plantation_2015.tif")

plot(real_plantation_2015)

datatype(real_plantation_2015)

# We calculate the gain in pixels for forest plantation from 1987-2015

plantation_gain_pixel_8715 = calculate_quantity_pixel_for_category(real_plantation_gains_8715,1)
# Import mask

lingue_mask_negative <- rast("data/processed/raster/mask/mask_lingue_1987.tif")
datatype(lingue_mask_negative)
plantation_1987_na_mask <- rast("data/processed/raster/mask/mask_plantation_1987.tif")

datatype(plantation_1987_na_mask)

lingue_mask_positive <- rast("data/processed/raster/mask/lingue_mask_positive.tif")
datatype(lingue_mask_positive)

## load spatial data

variables <- list.files(
    "~/github/location_factors/data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)


spatial_variables <- terra::rast(variables)

# Load models

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_all_20250926_192822.rds")

aic_null <- AIC(glmulti_models@objects[[1]]) #null model
glmulti_models@objects[[1021]]

# Iterative process to compute and save spatial and non spatial metrics for each model

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

pred_plantation_gains_8715 <- terra::predict(spatial_variables, glmulti_models@objects[[320]], type = "response")

plot(pred_plantation_gains_8715)


###

sim_gain <- simulations_gains(
    glmulti_models@objects[[1]],
    spatial_variables,
    plantation_gain_pixel_8715,
    original_plantation_mask = plantation_1987_na_mask
)

plot(sim_gain)


# Build plantation total for 2015
sim_plantation_2015 <- prepare_simulation_raster(
    sim_img = sim_gain,
    lingue_mask_positive = lingue_mask_positive,
    plantation_1987 = plantation_1987
)

fom = calculate_figure_of_merits_image_2(real_plantation_gains_8715, sim_plantation_2015)

images_results = c(real_plantation_gains_8715, sim_plantation_2015, fom)

plot(images_results)

plot(sim_plantation_2015)

standarize_image <- image_standarization_function(pred_plantation_gains_8715,sim_plantation_2015,lingue_mask_positive)
plot(standarize_image)
TOC_VALUE <- TOC::TOC(standarize_image[["pred_plantation_gains"]],standarize_image[["sim_plantation_2015"]],mask= standarize_image[["lingue_mask_positive"]], nthres = 100)
TOC_AUC = TOC_VALUE@AUC

TOC_AUC
plot(sim_plantation_2015)

plot(real_plantation_gain_8715)

##### real_plantation_gains_8715 ###


calculate_figure_of_merits_image()


plot(real_plantation_gains_8715)

plot(fom)

#plot(fomplot(sim_plantation_2015)
#plot(gan_patches_8715)

calculate_figure_of_merits_image_labeled(sim_plantation_2015)

# Run the function
fom_image <- calculate_figure_of_merits_image_labeled(
    observed_r = real_plantation_gains_8715,
    simulated_r = sim_plantation_2015,
    mask_r = lingue_mask_positive
)


plot(fom_image)
plot(lingue_mask_positive)

# See legend
attr(fom_image, "legend_table")

# Quick plot in R
terra::plot(fom_image, col = attr(fom_image, "legend_table")$color,
            legend = FALSE, main = "Figure of Merit Classification")
legend("bottomleft", legend = attr(fom_image, "legend_table")$meaning,
       fill = attr(fom_image, "legend_table")$color, cex = 0.8)

