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


spatial_variables <- terra::rast(variables)

# Load models

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_20250815_094328.rds")


## simulation gains calculate based on model, spatial predict variables, pixel number of change, mask_plantation_1987 in na

sim_gain <- simulations_gains(model = glmulti_models@objects[[287]], spatial_predict_variables = spatial_variables,
                  pixel_number = plantation_gain_pixel_8715, original_plantation_mask = plantation_1987_na_mask)


plot(terra::predict(spatial_variables, glmulti_models@objects[[299]], type = "response"))

### I need to define a function for this stage

plot(sim_gain)

## sim_gain has na values, it need to be standarize to be use in the calculation ob
## na values where plantation in 1987 was presented is fill with 1 values, and 
## na values where no plantation was present in 1987 are na

sim_plantation_2015 <- prepare_simulation_raster(sim_img = sim_gain, lingue_mask_positive = lingue_mask_positive, plantation_1987 = plantation_1987)

plot(sim_plantation_2015)

# FOM CALCULATION

calculate_fom(ref_img=real_plantation_2015, sim_img = sim_plantation_2015)

plot(calculate_fom_image_binary(plantation_1987,real_plantation_2015,sim_plantation_2015))

figure_list <- list()

for (i in 250:260){
    sim_gain <- simulations_gains(glmulti_models@objects[[i]], spatial_variables, plantation_gain_pixel_8715, original_plantation_mask = plantation_1987_na_mask)
    sim_plantation_2015 <- prepare_simulation_raster(sim_img = sim_gain, lingue_mask_positive = lingue_mask_positive, plantation_1987 = plantation_1987)
    fom = calculate_fom(ref_img = real_plantation_2015, sim_plantation_2015)
    figure_list <- c(figure_list, list(fom))
    
}



figure_list_apply <- lapply(1:10, function(i){ 
    simulations_gains(
        glmulti_models@objects[[i]], 
        spatial_variables, 
        plantation_gain_pixel_8715, 
        original_plantation_mask = plantation_1987_na_mask)
    
    sim_plantation_2015 <- prepare_simulation_raster(sim_img = sim_gain, lingue_mask_positive = lingue_mask_positive, plantation_1987 = plantation_1987)
    
    fom = calculate_fom(ref_img = real_plantation_2015, sim_plantation_2015)
})


formula(glmulti_models@objects[[1]])

# REPETIR EL PROCESO PARA CALCULAR LAS OTRAS MÉTRICAS ROC, AUC, TOC,
# NECESITO GUARDAR TAMBIÉN EL AIC DE CADA MODELO, LAS VARIABLES, ROC, AUC, BOLEAN.



validate_model <- function(i, glmulti_models, spatial_variables,
                           plantation_gain_pixel_8715,
                           plantation_1987_na_mask,
                           lingue_mask_positive,
                           plantation_1987,
                           real_plantation_2015){
    
    # Model i
    model_i <- glmulti_models@objects[[i]]
    
    # Extract variables used in this model
    vars_i <- all.vars(formula(model_i))[-1] # drop response variable
    
    # Extract AIC for this model and null model
    aic_total <- AIC(model_i)
    aic_null <- AIC(glmulti_models@objects[[1]]) #null model
    delta_aic <- aic_null - aic_total
    
    # Deviances
    null_dev <- model_i$null.deviance
    resid_dev <- model_i$deviance
    dev_explained <- null_dev - resid_dev
    
    
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
    
    # Return structured output
    list(
        model_id = i,
        fom = fom_val,
        aic_total = aic_total,
        aic_null = aic_null,
        delta_aic = delta_aic,
        null_deviance = null_dev,
        residual_dev = resid_dev,
        dev_explained = dev_explained,
        variables = vars_i
    )
    
}

# Run validation across top 10 models

results_list <- lapply(1:10, function(i){
    validate_model(
        i,
        glmulti_models,
        spatial_variables,
        plantation_gain_pixel_8715,
        plantation_1987_na_mask,
        lingue_mask_positive,
        plantation_1987,
        real_plantation_2015
    )
    
})




