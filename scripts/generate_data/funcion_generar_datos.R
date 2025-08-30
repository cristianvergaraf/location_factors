# Esta funcion extrae los datos de un conjunto de variables independientes en formato raster
# a partir de una capa de puntos en formato shape (puntos de muestreo) generados a partir de 
# la capa de expansion de plantaciones forestales.
# La funcion tiene como argumentos dir_entrada, dir_salida, y el nombre del documento de salida
# La salida es en formato vectorial, puntos, shape, y geodatabase y un csv. 


generar_data <- function(dir_entrada,dir_salida, nombre_tabla, vector_point_data){
  
  library(terra)
  library(sf)
  library(dplyr)
  
  setwd(dir_entrada)
  
  files <- list.files(path = ".", "*.tif$")
  raster_lingue <- rast(files)

  
  lingue_puntos <- st_read(vector_point_data)
  
  # lingue_puntos_18s <- st_set_crs(lingue_puntos, "epsg:32718")
  
  lingue_pts_vec <- vect(vector_point_data, crs = "epsg:32718")
  
  # vec_sf <- vect(lingue_puntos_18s)
  
  values_ext <- terra::extract(raster_lingue, lingue_pts_vec, xy = TRUE, cells = TRUE)
  
  sf_li_pts_18s_datos <- right_join(lingue_puntos, values_ext, by = c("fid" = "ID")) %>% 
    dplyr::select(!c(cell.y, cell.x,x.1,y.1,fid)) 

  datos_wrlg_8701 <- st_drop_geometry(sf_li_pts_18s_datos)
  
  setwd(dir_salida)
  
  write.csv(datos_wrlg_8701, file = "datos_modelo_completo_2025.csv")
  st_write(sf_li_pts_18s_datos, dsn = "sf_li_pts_18s_datos_completo.shp")
  st_write(sf_li_pts_18s_datos, dsn = "li_datos8715_completo.gpkg", layer = "li_pts_18s", delete_layer = TRUE)
  
}
