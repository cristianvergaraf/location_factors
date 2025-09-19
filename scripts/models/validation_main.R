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


# Prepare the data we need sim_gain withou na inside
# What does mask do

plot(lingue_mask_positive)


values(pred_plantation_gains_8715)[is.nan(values(pred_plantation_gains_8715))] <- NA
values(sim_plantation_2015)[is.nan(values(sim_plantation_2015))] <- NA
values(lingue_mask_positive)[is.nan(values(lingue_mask_positive))] <- NA

NAflag(pred_plantation_gains_8715)
NAflag(sim_plantation_2015)
NAflag(lingue_mask_positive)


template <- pred_plantation_gains_8715

# Find the common set of non-NA cells across all rasters.
# This ensures NA values are in the exact same locations.
valid_cells <- !is.na(pred_plantation_gains_8715) & !is.na(sim_plantation_2015) & !is.na(lingue_mask_positive)
na_mask <- is.na(pred_plantation_gains_8715) | is.na(sim_plantation_2015) | is.na(lingue_mask_positive)

# Create a clean version of the first raster, where cells that are NA in ANY
# of the rasters are set to NA.

pred_clean1 <- pred_plantation_gains_8715
pred_clean1[!valid_cells] <- NA

sim_plantation_2015_clean <- sim_plantation_2015
sim_plantation_2015_clean[!valid_cells] <- NA

lingue_mask_positive_clean <- lingue_mask_positive
lingue_mask_positive_clean[!valid_cells] <- NA


TOC_values = TOC::TOC(pred_clean1,sim_plantation_2015_clean,mask= lingue_mask_positive_clean, nthres = 100)

TOC_values

plot(TOC_values, labelThres = TRUE, cex = 0.8, posL = 4)

# Resample the other two rasters to match the template's extent, resolution, and CRS.
# Use 'ngb' (nearest neighbor) for your mask, as it's likely categorical data.


# Create a logical mask where TRUE indicates a valid pixel in all three rasters
valid_cells <- !is.na(pred_plantation_gains_8715) & !is.na(sim_plantation_2015) & !is.na(lingue_mask_positive)

# Use ifel() to create a new raster where pixels that are not valid
# in all three rasters are set to NA.
pred_clean <- terra::ifel(valid_cells, pred_plantation_gains_8715, NA)
sim_clean <- terra::ifel(valid_cells, sim_plantation_2015, NA)
lingue_clean <- terra::ifel(valid_cells, lingue_mask_positive, NA)



sim_plantation_2015_resampled <- resample(sim_plantation_2015, template, method = 'near')
lingue_mask_positive_resampled <- resample(lingue_mask_positive, template, method = 'near')


datatype(pred_plantation_gains_8715, bylyr=TRUE)

values_pred <- values(pred_plantation_gains_8715)

class(values_pred)


# https://www.youtube.com/watch?v=1JRwVOi0FSE


#################


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
        test_auc = as.numeric(test_auc)
        #TODO: ADD TOC HOW TO CALCULATE TOC
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

