
data_extraction_from_points_to_spatial_raster <- function(
        spatial_predictor_rast_stack,
        sample_points_sf,
        table_name,
        csv_results_path,
        vector_results_path){ 
    
    # spatial_predictor_rast_stack: is a rast stack object containing all raster 
    # sample_points_vec: is a vectorial vect containing the sample points to extract the value
    
    ## Reproject if needed
    
    if (st_crs(sample_points_sf) != st_crs(spatial_predictor_rast_stack)){
        stop("❌ CRS mismatch: sample points and raster stack must have the same CRS.")
    }
        #sample_points_sf <- st_transform(sample_points_sf, st_crs(spatial_predictor_rast_stack))
        
    
    
    # Extract raster values at point locations
    
  
    sample_points_vect = vect(sample_points_sf)
    
    value_ext <- terra::extract(spatial_predictor_rast_stack,sample_points_vect, xy = TRUE, cells = TRUE)
    
    # Merge with attributes from the points shapefile
    #value_ext_sf <- st_as_sf(as.data.frame(value_ext))
    data_sf <- cbind(sample_points_sf, value_ext[,-1])  # drop duplicate ID
    
    # Prepare tabular version (drop geometry)
    data_df <- st_drop_geometry(data_sf)
    
    # Build path
    csv_path <- file.path(csv_results_path, paste0(table_name,".csv"))
    gpkg_path <- file.path(vector_results_path, paste0(table_name,".gpkg"))
    
    #Write outputs
    write.csv(data_df, csv_path, row.names = FALSE)
    st_write(data_sf, dsn = gpkg_path, layer = table_name, delete_layer = TRUE, quiet = TRUE)
    
    message("Outputs written to :", csv_results_path, " and ", vector_results_path)
    
    invisible(data_sf)
    
    }