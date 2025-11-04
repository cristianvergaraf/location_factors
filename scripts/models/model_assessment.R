#########################
###### Functions ########

ranking_values_raster <- function(imagen){
  imagen_values <- terra::values(imagen)
  imagen_ranked_values <- rank(imagen_values, na.last = 'keep', ties.method = 'average')
  terra::values(imagen) <- imagen_ranked_values
  return(imagen)
} 

ranking_values_raster_review <- function(imagen) {
    # Get a copy of the original raster to maintain metadata
    out_raster <- imagen
    
    # Extract values and rank them
    imagen_values <- terra::values(out_raster)
    imagen_ranked_values <- rank(imagen_values, na.last = 'keep', ties.method = 'average')
    
    # Safely set the new values using terra::setValues
    # This prevents metadata corruption
    out_raster <- terra::setValues(out_raster, imagen_ranked_values)
    
    return(out_raster)
}


calculate_quantity_pixel_for_category <- function(raster_image, numeric_category){
    use_freq = terra::freq(raster_image)
    return (use_freq['count'][use_freq["value"] ==numeric_category])
}    


quantity_reclasify <- function(image_ranking,quantity,mask){
  gan_sim_pl <- ifel(image_ranking > quantity, 1,0)
  mask_gan_sim_pl <- gan_sim_pl*mask
  return(mask_gan_sim_pl)
}


custom_scale <- function(imagen) {
  if (any(!is.na(imagen))) {
    scaled_values <- (imagen - min(imagen, na.rm = TRUE)) / (max(imagen, na.rm = TRUE) - min(imagen, na.rm = TRUE))
    return(scaled_values)
  } else {
    return(imagen)
  }
}


calculate_figure_of_merits_image <- function(imagen_1, imagen_2, imagen_3) {
  terra::ifel(imagen_1 == 1 & imagen_2 == 1 & imagen_3 ==1,4,
              terra::ifel(imagen_1 == 1 & imagen_2 == 1, 3,
                          terra::ifel(imagen_1 == 0 & imagen_2 == 1, 2,
                                      terra::ifel(imagen_1 == 1 & imagen_2 == 0, 1, 
                                                  terra::ifel(imagen_1 == 0 & imagen_2 == 0, 0, NA)))))
    
}

calculate_figure_of_merits_image_2 <- function(imagen_1, imagen_2) {
    terra::ifel(imagen_1 == 1 & imagen_2 == 1 ,1,
                terra::ifel(imagen_1 == 1 & imagen_2 == 0, 2,
                            terra::ifel(imagen_1 == 0 & imagen_2 == 1, 3,
                                        terra::ifel(imagen_1 == 0 & imagen_2 == 0, 0, NA))))
}


calculate_figure_of_merits_image_labeled <- function(observed_r, simulated_r, mask_r = NULL) {
    # --------------------------------------------------------------------
    # Create a Figure of Merit (FoM) classification raster.
    # Each pixel will be assigned a code:
    #   1 = Hit (True Positive)
    #   2 = Commission Error (False Positive)
    #   3 = Omission Error (False Negative)
    #   0 = Correct Rejection (True Negative)
    # Missing / invalid pixels -> NA
    #
    # Args:
    #   observed_r: terra::SpatRaster, observed (reference) change map
    #   simulated_r: terra::SpatRaster, simulated change map
    #   mask_r: (optional) terra::SpatRaster mask (1 = valid, 0/NA = ignore)
    #
    # Returns:
    #   A SpatRaster with categorical FoM classes and a legend table
    # --------------------------------------------------------------------
    
    # --- Check geometry consistency ---
    if (!terra::compareGeom(observed_r, simulated_r, stopOnError = FALSE)) {
        stop("Observed and simulated rasters do not share the same extent/resolution.")
    }
    
    # --- Compute FoM classes ---
    fom_r <- terra::ifel(
        observed_r == 1 & simulated_r == 1, 1,  # Hit / True Positive
        terra::ifel(
            observed_r == 0 & simulated_r == 1, 2,  # Commission Error / False Positive
            terra::ifel(
                observed_r == 1 & simulated_r == 0, 3,  # Omission Error / False Negative
                0  # Correct Rejection / True Negative
            )
        )
    )
    
    # --- Apply mask if provided ---
    if (!is.null(mask_r)) {
        fom_r <- terra::mask(fom_r, mask_r)
    }
    
    # --- Assign names and categories ---
    names(fom_r) <- "FoM_class"
    
    # Create a data frame for legend (useful for plotting or exporting)
    fom_legend <- data.frame(
        class_code = c(0, 1, 2, 3),
        meaning = c(
            "True Negative (Correct Rejection)",
            "True Positive (Hit)",
            "False Positive (Commission Error)",
            "False Negative (Omission Error)"
        ),
        color = c("#e0e0e0", "#1b9e77", "#d95f02", "#7570b3") # optional colors
    )
    
    # Attach legend as an attribute (for easy access)
    attr(fom_r, "legend_table") <- fom_legend
    
    message("FoM raster created with legend table (see attr(x, 'legend_table')).")
    return(fom_r)
}








calculate_fom <- function(ref_img, sim_img){
  
  # stack images in a vector    
  stack_sim <- c(ref_img, sim_img)
  # Cross-tabulate
  cross_sim <- terra::crosstab(stack_sim, long = FALSE)
  
  # Extract counts
  
  hits <- cross_sim["1","1"]
  misses <- cross_sim["1","0"]
  false_alarm <- cross_sim["0","1"]
  
  fom <- hits / (hits + misses + false_alarm)
  
  return(fom)
  
}

calculate_fom_old <- function(referencia, simulado){
    stack_sim <- c(referencia, simulado)
    cross_sim <- terra::crosstab(stack_sim)
    figure_merits <- cross_sim[4]/(cross_sim[4]+cross_sim[3]+cross_sim[2]) # This indexing is risky
    return(figure_merits)
}


overall_figure_merits <- function(numero, modelo,mask){
    simulacion <- evaluar_simulacion(numero, modelo, mask= mask)
    ras_plantaciones_2015_sim_patches = raster(plantaciones_1987 + simulacion)
    ThreeMaps_plantaciones <-lulcc::ThreeMapComparison(ras_plantaciones_1987, ras_plantaciones_2015_patches, ras_plantaciones_2015_sim_patches, factors  = 30,
                                                       categories = c(0,1), labels = c("no plantacion", "plantacion"))
    Fig_merits_plantaciones <- lulcc::FigureOfMerit(ThreeMaps_plantaciones)
    return (Fig_merits_plantaciones@overall)
    
} 

overall_figure_merits <- function(numero, modelo,mask){
  simulacion <- evaluar_simulacion(numero, modelo, mask= mask)
  ras_plantaciones_2015_sim_patches = raster(plantaciones_1987 + simulacion)
  ThreeMaps_plantaciones <-lulcc::ThreeMapComparison(ras_plantaciones_1987, ras_plantaciones_2015_patches, ras_plantaciones_2015_sim_patches, factors  = 30,
                                                 categories = c(0,1), labels = c("no plantacion", "plantacion"))
  Fig_merits_plantaciones <- lulcc::FigureOfMerit(ThreeMaps_plantaciones)
  return (Fig_merits_plantaciones@overall)
  
} 


evaluar_simulacion <- function(num_pixels,model,mask, mask_plantaciones){
  prob <-  terra::predict(variables_escaladas, model, type = "response")
  m_prob <- prob * mask_plantaciones 
  ranking_prob <- ranking_values_raster(m_prob)
  gan_sim <-reclasificar_cantidad(ranking_prob,num_pixels, mask)
  return(gan_sim)
  
}

assign_na_value_from_raster_image <- function(raster_image,value_if_isna, value_if_notna){
    return (ifel(is.na(raster_image), value_if_isna, value_if_notna))
    
}

prepare_simulation_raster <- function(sim_img, lingue_mask_positive, plantation_1987){
    
    # Replace zeros with NA if needed
    sim_gain_na <- assign_na_value_from_raster_image(sim_img,0,sim_img)
    
    # Apply the positive mask
    sim_gain_mask <- sim_gain_na * lingue_mask_positive
    
    # Add initial plantation to get total simulated plantation
    sim_plantation_2015 <- sim_gain_mask + plantation_1987
    
    values(sim_plantation_2015)[is.nan(values(sim_plantation_2015))] <- NA
    
    return (sim_plantation_2015)
    
}

# Function para calcular el AUC a partir de un modelo glm

# TODO: AUC ESPACIAL TENGO USAR TODOS LOS DATOS DEL RASTER. PASARLOS A VALOR Y HACER ROC
# TODO: AUC NO ESPACIAL ES DE CALIBRACION CON LOS PUNTOS DE VALIDACIÓN Y TEST.

calculate_auc <- function(testing_set, modelo){
  labels <- testing_set$response_var
  predictions <- predict(modelo, type = "response", newdata = testing_set)
  roc_curve <- pROC::roc(labels, predictions)
  return(roc_curve$auc[1])
}

compute_auc <- function(testing_set, modelo, response_var = "gan_plant"){
    labels <- testing_set[[response_var]]
    if (length(unique(labels)) < 2){
        warning("Labels contain only one class. AUC connot be computed.")
    }
    
    predictions <- predict(modelo, type = "response", newdata = testing_set)
    roc_curve <- pROC::roc(labels, predictions)
    return(as.numeric(roc_curve$auc))
    
}

simulation_assessment <- function(num_pixels,model,mask){
    prob = terra::predict(variables_escaladas, model, type = "response")
    ranking_prob <- ranking_values_raster(prob)
    gan_sim <-reclasificar_cantidad(ranking_prob,num_pixels, mask)
    return(gan_sim)
    
}

simulations_gains <- function(model,spatial_predict_variables,pixel_number, original_plantation_mask){
    prob <-  terra::predict(spatial_predict_variables, model, type = "response")
    m_prob <- prob * original_plantation_mask 
    ranking_prob <- ranking_values_raster_review(m_prob)
    gan_sim <-select_top_pixels_review(ranking_prob,pixel_number)
    return(gan_sim)
    
}

simulations_gains_review <- function(model, spatial_predict_variables, pixel_number, original_plantation_mask) {
    
    # Predict probabilities for each pixel
    # This returns a valid SpatRaster
    prob <- terra::predict(spatial_predict_variables, model, type = "response")
    
    # Mask the probabilities to the original plantation area.
    # This uses terra's built-in masking, preserving SpatRaster properties.
    m_prob <- prob * original_plantation_mask
    
    # Identify the top 'pixel_number' of pixels based on probability
    # This is a more direct and efficient way to select top pixels
    top_prob_threshold <- terra::quantile(m_prob, 1 - (pixel_number / ncell(m_prob)), na.rm = TRUE)
    
    # Create a new binary raster where top pixels are 1 and others are 0
    # ifel is a robust terra function that always returns a valid SpatRaster
    gan_sim <- terra::ifel(m_prob >= top_prob_threshold, 1, 0)
    
    # Re-apply the original mask to ensure NA values are correctly placed
    gan_sim <- terra::mask(gan_sim, original_plantation_mask)
    
    return(gan_sim)
}

calculate_gains_area = function(raster_layer){
    gains_patches_8715 = rast(raster)
    freq_land_use = terra::freq(gains_patches_8715)
    df_freq_land_use = as.data.frame(freq_uso)
    df_freq_land_use['area'] = df_freq_land_use['count'] * 0.09
    
    return(df_freq_land_use)
}

create_mask_from_raster <-function(raster_image){
    x <- c(0,10,0)
    mclas = matrix(x, ncol = 3, byrow = TRUE)
    return (classify(raster_image,rcl = mclas))
}

create_mask_from_raster_for_one_category <-function(raster_image,category,total_number_categories){
    x <- c(category-1,category,1,0,category,0,category,total_number_categories,0)
    mclas = matrix(x, ncol = 3, byrow = TRUE)
    return (classify(raster_image,rcl = mclas,))
}


select_top_pixels_review <- function(raster, n_pixels) {
    
    # Ensure the input is a valid SpatRaster
    if (!inherits(raster, "SpatRaster")) {
        stop("Input must be a SpatRaster object.")
    }
    
    # Extract values and handle NAs safely
    vals <- values(raster, mat = FALSE)
    na_mask <- is.na(vals)
    vals[na_mask] <- -Inf # Temporarily replace NAs for sorting
    
    # Order indices by descending values
    ord <- order(vals, decreasing = TRUE)
    
    # Select top N indices
    top_idx <- ord[1:n_pixels]
    
    # Create a new vector for the output raster values, initialized to 0
    out_vals <- rep(0, length(vals))
    
    # Set the values for the top pixels to 1
    out_vals[top_idx] <- 1
    
    # Restore the NA values using the mask
    out_vals[na_mask] <- NA
    
    # Create the output SpatRaster by setting values safely
    # This is the key change to avoid corruption
    out <- setValues(raster, out_vals)
    
    return(out)
}


select_top_pixels <- function(raster, n_pixels) {
    # Extract values
    vals <- values(raster, mat = FALSE)
    
    # Order indices by descending values
    ord <- order(vals, decreasing = TRUE, na.last = NA)
   
    
    # Select top N indices
    top_idx <- ord[1:n_pixels]
    
    #Create output raster
    out_vals <- rep(0,length(vals))
    out_vals[top_idx] <- 1
    # restore NAs
    out_vals[is.na(vals)] <- NA
    
    
    out <- raster
    values(out) <- out_vals
    return(out)
}

calculate_fom_image_binary <- function(t1, t2_real, t2_sim) {
    # Encode each raster as a binary digit
    # im1 = 1, im2 = 2, im3 = 4
    code <- t1 * 1 + t2_real * 2 + t2_sim * 4
    
    # 0 No intersection
    # 1 pixel of forest plantation only t1
    # 2 pixel of forest plantation only t2, misses
    # 3 pixel of forest plantation in t1, and t2
    # 4 pixel of forest plantation only in sim. False alarm.
    # 5 pixel of forest plantation only t1 and t3.
    # 6 pixel of forest plantation in t2_real and t2_sim (hits)
    # 7 pixel of forest plantation in t1 + t2 real + t3 sim
    
    # hits pixel correctly simulated (t2_real and t2_sim = 6)
    # false alarm incorrectly simulated (t2_sim =1 and t2_real = 0)
    # 
    
    # Map the binary code to FOM 0-4
    # 0 = 0, 1 = 1, 2 = 2, 3 = 3, 7 = 4
    fom <- terra::ifel(code == 6, 3,
                       terra::ifel(code == 4, 2,
                                   terra::ifel(code == 2, 1,0
                                                           )))
    
    return(fom)
}

compute_spatial_auc_from_raster_images <- function(real_gain,pred_gain){
    # Real gain raster layer real gain from t1 to t2 values 1-0
    # Pred gain is transition potential image with probability values
    
    
    # Extract values of each pixel
    real_gain_values <- values(real_gain, mat = FALSE)
    pred_gain_values <- values(pred_gain, mat = FALSE)
    
    # Delete NA values 
    
    valid_idx <- !is.na(real_gain_values) & !is.na(pred_gain_values)
    
    valid_real_gains_values <- real_gain_values[valid_idx]
    valid_pred_gains_Values <- pred_gain_values[valid_idx]
    
    # Calculate ROC
    
    spatial_roc_curve <- pROC::roc(response = valid_real_gains_values,predictor = valid_pred_gains_Values)
    
    spatial_auc <- spatial_roc_curve$auc
    
    return(spatial_auc)
    
}

image_standarization_function <- function(pred_plantation_gains_8715,sim_plantation_2015,lingue_mask_positive){
    
    # Convert NAN to NA
    values(pred_plantation_gains_8715)[is.nan(values(pred_plantation_gains_8715))] <- NA
    values(sim_plantation_2015)[is.nan(values(sim_plantation_2015))] <- NA
    values(lingue_mask_positive)[is.nan(values(lingue_mask_positive))] <- NA
    
    # Creates a common mask of na for each raster
    valid_cells <- !is.na(pred_plantation_gains_8715) & !is.na(sim_plantation_2015) & !is.na(lingue_mask_positive)
    
    # Applies the mask to each raster
    pred_clean <- pred_plantation_gains_8715
    pred_clean[!valid_cells] <- NA
    
    sim_plantation_2015_clean <- sim_plantation_2015
    sim_plantation_2015_clean[!valid_cells] <- NA
    
    lingue_mask_positive_clean <- lingue_mask_positive
    lingue_mask_positive_clean[!valid_cells] <- NA
    
    # Create a stack conserving the image names
    images_stack <- c(pred_clean,sim_plantation_2015_clean,lingue_mask_positive_clean)
    names(images_stack) <- c("pred_plantation_gains", "sim_plantation_2015","lingue_mask_positive")
    
    return(images_stack)
    
}





