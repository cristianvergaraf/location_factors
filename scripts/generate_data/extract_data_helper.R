
data_extraction_from_points_to_spatial_raster <- function(spatial_predictor_rast_stack,sample_points_vect,results_path){ 
    
    # spatial_predictor_rast_stack: is a rast stack object containing all raster 
    # sample_points_vec: is a vectorial vect containing the sample points to extract the value
    
    ## Reproject if needed
    
   # if (!st_crs(sample_points_vect) == crs(spatial_predictor_rast_stack)){
    #    message("Reprojecting points to match raster CRS...")
     #   sample_points_vect <- project(sample_points_vect, crs(spatial_predictor_rast_stack))
      #  sample_points_vect <- st_transform(lingue_puntos, st_crs(raster_stack))
        
    #}
    
    # Extract raster values at point locations
    
    ##TODO: AGREGAR AQUI QUE SE DE ENTRADA UN SF, Y LUEGO TRANSFORMAR A VECT
    
    value_ext <- terra::extract(spatial_predictor_rast_stack,sample_points_vect, xy = TRUE, cells = TRUE)
    
    # Merge with attributes from the points shapefile
    
    sp_datos <- lingue_puntos %>%
        mutate(ID = row_number()) %>%
        right_join(value_ext, by = c("ID"="ID")) %>%
        select(-c(cell,x,y)) # drop duplicate geometry helper columns
    
    # Prepare tabular version (drop geometry)
    datos_tabla <- st_drop_geometry(sf_datos)
    
    # Build path
    csv_path <- file.path(dir_salida, paste0(nombre_table, ".csv"))
    gpkg_path <- file.path(dir_salida, paste0(nombre_tabla, "gpkg"))
    
    # Write outputs
    write.csv(datos_tabla, csv_path, row.names = FALSE)
    st_write(sf_datos, dsn = gpkg_path, layer = nombre_table, delete_layer = TRUE, quiet = TRUE)
    
    message("Outputs written to :", dir_salida)
    
    invisible(sf_datos)
    
    }