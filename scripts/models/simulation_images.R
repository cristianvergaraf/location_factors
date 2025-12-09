#### load packages

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
    "data/raw/raster/factores_localizacion", 
    pattern = ".tif$", full.names = TRUE
)


spatial_variables <- terra::rast(variables)

# Load models

glmulti_models <- readRDS("model_outputs/glmulti_model_all_20250926_192822.rds")


#####


# Model i
model_number = 1
model_i <- glmulti_models@objects[[model_number]]


sim_gain <- simulations_gains(
    model_i,
    spatial_variables,
    plantation_gain_pixel_8715,
    original_plantation_mask = plantation_1987_na_mask
)

plot(sim_gain)

(as.character(model_number))

output_folder = "data/processed/raster/simulation_images"
name <- paste0(output_folder,"/simulation_image_model_", as.character(model_number),".tif")

name

terra::writeRaster(sim_gain, filename="data/processed/raster/simulation_images/simulation_image_1.tif", overwrite=TRUE)


#' @title Run Simulations and Save Results for Multiple Models
#'
#' @description Iterates through a list of models, runs the 'simulations_gains'
#' function for each, and saves the resulting raster to a file.
#'
#' @param model_list A list containing the model objects (e.g., glmulti_models@objects).
#' @param spatial_vars Raster stack/data frame of spatial variables used in the model.
#' @param plantation_gain_data Raster/data for plantation gain pixels.
#' @param original_mask The mask used to constrain the simulation area.
#' @param output_dir The directory where the simulation rasters will be saved.
#' @param overwrite_files Logical. Should existing output files be overwritten?
#'
#' @return Invisible NULL. The primary output is saved raster files.
#'
generate_model_images_simulations <- function(
        model_list,
        spatial_vars,
        plantation_gain_data,
        original_mask,
        output_dir = "data/processed/raster/simulation_images",
        overwrite_files = TRUE) {
    
    # 1. Check and create the output directory
    if (!dir.exists(output_dir)) {
        cat(paste("Creating output directory:", output_dir, "\n"))
        dir.create(output_dir, recursive = TRUE)
    }
    
    # 2. Start the loop
    for (i in seq_along(model_list)) {
        
        model_number <- i
        model_i <- model_list[[model_number]]
        
        cat(paste0("--- Processing Model ", model_number, " (", i, "/", length(model_list), ") ---\n"))
        
        # 3. Run the simulation
        sim_gain <- simulations_gains(
            model_i,
            spatial_vars,
            plantation_gain_data,
            original_plantation_mask = original_mask
        )
        
        # 4. Construct the dynamic filename
        file_name <- paste0(
            output_dir,
            "/simulation_image_model_",
            as.character(model_number),
            ".tif"
        )
        
        # 5. Write the raster
        tryCatch({
            terra::writeRaster(
                sim_gain,
                filename = file_name,
                overwrite = overwrite_files
            )
            cat(paste("✅ Saved to:", file_name, "\n"))
        }, error = function(e) {
            cat(paste("❌ ERROR saving model", model_number, ":", conditionMessage(e), "\n"))
        })
    }
    
    return(invisible(NULL))
}

generate_model_image_simulations
generate_model_images_simulations(
    model_list = glmulti_models@objects,
    spatial_vars = spatial_variables,
    plantation_gain_data = plantation_gain_pixel_8715,
    original_mask = plantation_1987_na_mask,
    output_dir = "data/processed/raster/simulation_images", # Optional: uses default if omitted
    overwrite_files = TRUE                                   # Optional: uses default if omitted
)


generate_model_image_simulations(model_list,
                                 spatial_vars,
                                 plantation_gain_data,
                                 original_mask,)


### Revisar imagenes. Necesito una funcion para leer y plotear
path ="data/processed/raster/simulation_images/"
model_image = "simulation_image_model_10.tif"

plot_simulation_image(path, model_image)


plot_simulation_image <-function(path,model_image){
    simulation_images <- rast(paste0(path,model_image))
    return(plot(simulation_images))
}



simulation_images_1 <- rast("data/processed/raster/simulation_images/simulation_image_model_2.tif")

plot(simulation_images_1)

##### Me gustaría calcular el número de pixels
##### Para comprobar que todos han simulada la misma area 

# Crear un gift

library(magick)

frames <- image_read(list.files(path="data/processed/raster/simulation_images",
                                pattern = "tif$", full.names = TRUE))


frames
gif <- image_animate(frames, fps = 10)  # 12 frames por segundo (puedes ajustar)

image_write(gif, "simulacion.gif")


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









#########



aic_null <- AIC(glmulti_models@objects[[1]]) #null model
glmulti_models@objects[[1021]]

# Iterative process to compute and save spatial and non spatial metrics for each model

datatype(spatial_variables[[12]])
### Function to calculate spatial ROC from real expansion and simulated expansion


source("config/paths.R")
