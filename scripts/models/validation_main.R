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
plantation_1987 <- rast("data/processed/raster/validation_required_images/plantation_1987_mask_final.tif")
datatype(plantation_1987)
real_plantation_2015 <- rast("data/processed/raster/validation_required_images/real_plantation_2015.tif")
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

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_20250815_094328.rds")


# Iterative process to compute and save spatial and non spatial metrics for each model

datatype(spatial_variables[[14]])
### Function to calculate spatial ROC from real expansion and simulated expansion

spatial_variables

pred_plantation_gains_8715 <- terra::predict(spatial_variables, glmulti_models@objects[[1]], type = "response")

plot(pred_plantation_gains_8715)

values(pred_plantation_gains_8715)

datatype(pred_plantation_gains_8715)

terra::datatype(pred_plantation_gains_8715)

#### We are going to compute training and testing AUC 

source("config/paths.R")

#### Load Data Training Data

training_data <- read.csv(training_data_file)
test_data <- read.csv(test_data_file)

test_data

# Run simulation for this model
sim_gain <- simulations_gains(
    glmulti_models@objects[[1]],
    spatial_variables,
    plantation_gain_pixel_8715,
    original_plantation_mask = plantation_1987_na_mask
)

datatype(sim_gain)

# Build plantation total for 2015

sim_plantation_2015 <- prepare_simulation_raster(
    sim_img = sim_gain,
    lingue_mask_positive = lingue_mask_positive,
    plantation_1987 = plantation_1987
)


validate_model <- function(i, glmulti_models, spatial_variables,
                           plantation_gain_pixel_8715,
                           plantation_1987_na_mask,
                           lingue_mask_positive,
                           plantation_1987,
                           real_plantation_2015,
                           real_plantation_gains_8715,
                           training_data,
                           test_data){
    
    # Model i
    model_i <- glmulti_models@objects[[i]]
    
    pred_plantation_gains_8715 <- terra::predict(spatial_variables, model_i, type = "response")
    
    
    # Extract variables used in this model
    vars_i <- all.vars(formula(model_i))[-1] # drop response variable
    
    # Extract AIC for this model and null model
    aic <- AIC(model_i)
    aic_null <- AIC(glmulti_models@objects[[1]]) #null model
    delta_aic <- aic_null - aic
    
    # Deviances
    null_dev <- model_i$null.deviance
    resid_dev <- model_i$deviance
    dev_explained <- null_dev - resid_dev
    
    # 9. Compute MCFaddens pseudo-R2
    pseudoR2 <- 1 - resid_dev / null_dev
    
    # Run simulation for this model
    sim_gain <- simulations_gains(
        model_i,
        spatial_variables,
        plantation_gain_pixel_8715,
        original_plantation_mask = plantation_1987_na_mask
    )
    
    # Build plantation total for 2015
    sim_plantation_2015 <- prepare_simulation_raster(
        sim_img = sim_gain,
        lingue_mask_positive = lingue_mask_positive,
        plantation_1987 = plantation_1987
    )
    
    # Calculate FoM
    fom_val <- calculate_fom(
        ref_img = real_plantation_2015,
        sim_plantation_2015
    )
    
    ## compute AUC with training data
    
    train_auc = compute_auc(training_data, model_i, 'gan_plant_8715')
    
    ## compute AUC with test data
    
    test_auc = compute_auc(test_data, model_i, 'ras_gan_patches_8715_exp87')
    
    # Calculate spatial ROC
    
    spatial_auc = compute_spatial_auc_from_raster_images(real_plantation_gains_8715,pred_plantation_gains_8715)
    
    # Calculate TOC
    standarize_image <- image_standarization_function(pred_plantation_gains_8715,sim_plantation_2015,lingue_mask_positive)
    TOC_VALUE <- TOC::TOC(standarize_image[["pred_plantation_gains"]],standarize_image[["sim_plantation_2015"]],mask= standarize_image[["lingue_mask_positive"]], nthres = 100)
    TOC_AUC = TOC_VALUE@AUC
    
    
    
    # Return as data.frame with list-column
    tibble::tibble(
        model_id = i,
        fom = fom_val,
        variables = list(vars_i),       # list-column
        aic = aic,
        null_aic = aic_null,
        delta_aic = delta_aic,
        null_deviance = null_dev,
        residual_dev = resid_dev,
        dev_explained = dev_explained,
        MCFaddenPseudoR2 = pseudoR2,
        spatial_auc = as.numeric(spatial_auc),
        training_auc = as.numeric(train_auc),
        test_auc = as.numeric(test_auc),
        toc_auc = TOC_AUC
    )
    
}

results_tibble <- purrr::map_dfr(1:10, ~ validate_model(
    .x,
    glmulti_models,
    spatial_variables,
    plantation_gain_pixel_8715,
    plantation_1987_na_mask,
    lingue_mask_positive,
    plantation_1987,
    real_plantation_2015,
    real_plantation_gains_8715,
    training_data,
    test_data
))

View(results_tibble)

