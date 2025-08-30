#########################
###### Functions ########

ranking_values_raster <- function(imagen){
  imagen_values <- terra::values(imagen)
  imagen_ranked_values <- rank(imagen_values, na.last = 'keep', ties.method = 'average')
  terra::values(imagen) <- imagen_ranked_values
  return(imagen)
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
    
    return (sim_plantation_2015)
    
}

# Function para calcular el AUC a partir de un modelo glm

# TODO: AUC ESPACIAL TENGO USAR TODOS LOS DATOS DEL RASTER. PASARLOS A VALOR Y HACER ROC
# TODO: AUC NO ESPACIAL ES DE CALIBRACION CON LOS PUNTOS DE VALIDACIÓN Y TEST.

auc <- function(testing_set, modelo){
  labels <- testing_set$gan_plant
  predictions <- predict(modelo, type = "response", newdata = testing_set)
  roc_curve <- pROC::roc(labels, predictions)
  return(roc_curve$auc[1])
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
    ranking_prob <- ranking_values_raster(m_prob)
    gan_sim <-select_top_pixels(ranking_prob,pixel_number)
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




