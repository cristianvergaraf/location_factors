
###### Este pequeño permite genera un data frame que extrae los valores del script ######


setwd("~/github/location_factors/scripts")

source("funcion_generar_datos.R")

dir_entrada <- "C:/Users/Cristian/Documents/github/location_factors/datos/raster/factores_localizacion"

dir_salida <- "C:/Users/Cristian/Documents/github/location_factors/datos/vector"

generar_data(dir_entrada, dir_salida, nombre_tabla = "does.csv", "muestreo_8715_sin_plantaciones.shp")
