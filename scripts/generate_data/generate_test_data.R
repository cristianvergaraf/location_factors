
###### Este pequeño permite genera un data frame que extrae los valores del script ######


setwd("~/github/location_factors/scripts")

source("funcion_generar_datos.R")

dir_entrada <- "data/processed/vector"

dir_salida <- "data/processed/csv"

generar_data(dir_entrada, dir_salida, nombre_tabla = "process_csv.csv", "muestreo_8715_sin_plantaciones.shp")
