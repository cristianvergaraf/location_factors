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

plantation_1987 <- rast("data/processed/raster/validation_required_images/plantation_1987_mask_final.tif")

real_plantation_2015 <- rast("data/processed/raster/validation_required_images/real_plantation_2015.tif")


# We calculate the gain in pixels for forest plantation from 1987-2015

plantation_gain_pixel_8715 = calculate_quantity_pixel_for_category(real_plantation_gains_8715,1)

# Import mask

lingue_mask_negative <- rast("data/processed/raster/mask/mask_lingue_1987.tif")

plantation_1987_na_mask <- rast("data/processed/raster/mask/mask_plantation_1987.tif")


lingue_mask_positive <- rast("data/processed/raster/mask/lingue_mask_positive.tif")


## load spatial data

variables <- list.files(
    "~/github/location_factors/data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)


spatial_variables <- terra::rast(variables)

# Load models

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_20250815_094328.rds")


# Iterative process to compute and save spatial and non spatial metrics for each model


### Function to calculate spatial ROC from real expansion and simulated expansion

pred_plantation_gains_8715 <- terra::predict(spatial_variables, glmulti_models@objects[[1]], type = "response")


#################


validate_model <- function(i, glmulti_models, spatial_variables,
                           plantation_gain_pixel_8715,
                           plantation_1987_na_mask,
                           lingue_mask_positive,
                           plantation_1987,
                           real_plantation_2015,
                           real_plantation_gains_8715){
    
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
    
    # Calculate spatial ROC
    
    spatial_auc = compute_spatial_auc_from_raster_images(real_plantation_gains_8715,pred_plantation_gains_8715)
    
    # Return structured output
    data.frame(
        model_id = i,
        fom = fom_val,
        variables = paste(vars_i, collapse = ","),
        aic = aic,
        null_aic = aic_null,
        delta_aic = delta_aic,
        null_deviance = null_dev,
        residual_dev = resid_dev,
        dev_explained = dev_explained,
        MCFaddenPseudoR2 = pseudoR2,
        spatial_auc = as.numeric(spatial_auc),
        stringsAsFactors = FALSE
    )
    
}



## Run validation across top 10 models

results_list <- lapply(1:10, function(i){
    validate_model(
        i,
        glmulti_models,
        spatial_variables,
        plantation_gain_pixel_8715,
        plantation_1987_na_mask,
        lingue_mask_positive,
        plantation_1987,
        real_plantation_2015,
        real_plantation_gains_8715
    )
})



##TODO: CHECK WHY IS NOT FUNCTIONING
results_df <- do.call(rbind, lapply(results_list, function(x){
    
    data.frame(
        model_id = x$model_id,
        fom = x$fom,
        aic = x$aic,
        #TODO: ADD TRAINING AUC TO ASSESS OVERFITTING
        #TODO: ADD TESTING AUC TO ASSESS OVERFITTING
        #TODO: ADD TOC HOW TO CALCULATE TOC
        spatial_auc = x$spatial_auc,
        variables = paste(x$variables, collapse = ","),
        stringsAsFactors = FALSE
    )
}))


results_df

results_df <- do.call(rbind, lapply(results_list, function(x) {
    data.frame(
        model_id        = x$model_id,
        fom             = x$fom,
        aic             = x$aic,
        null_aic        = x$null_aic,
        delta_aic       = x$delta_aic,
        null_deviance   = x$null_deviance,
        residual_dev    = x$residual_dev,
        dev_explained   = x$dev_explained,
        MCFaddenPseudoR2= x$MCFaddenPseudoR2,
        spatial_auc     = as.numeric(x$spatial_auc$auc),  # extract the AUC
        variables       = paste(x$variables, collapse = ","),  # collapse into string
        stringsAsFactors = FALSE
    )
}))



results_list

source("config/paths.R")

### Load Data Training Data

spdf_train <- read.csv(training_data_file)

# Here we are calculating the train AUC, then we will calculate the test AUC
compute_auc(spdf_train, glmulti_models@objects[[1]], 'gan_plant_8715')

# How can I calculate the 

