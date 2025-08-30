# El objetivo de este script es generar una función que realiza usa 
# sampleStratified para realizar un muestreo de puntos luego usando la funcion buffer,
# hace un muestreo de puntos evaluando la distancia, y seleccionando puntos con un umbral mínimo.
# finalmente se seleccionan

disminuir_puntos <- function(distancia, iteraciones, names_intervalos, directorio, archivo){

library(rgeos)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)

library(data.table)
library(maptools)

setwd("C:/Users/FONDECYT/Dropbox/copia scripts/spatial R")

source("buffer.R")

setwd(directorio)

ganancia <- raster(archivo)


## Vamos a hacer un muestreo estratificado de las ganancias

test15 <- sampleStratified(ganancia, 5000, exp=1, na.rm=TRUE, xy=TRUE, ext=NULL, sp=TRUE)

test15$id <- 1:nrow(test15)

# Exportar los puntos a shape

# writeSpatialShape(test15, "c:/rlog/budi/test15.shp", factor2char = TRUE, max_nchar=254)


## vamos a transformar el SpatialPointsDAtaframe a un dataframe

test1 <- as.data.table(test15)

# Vamos a limpiar las columnas dejando solo las coordenadas

test2 <- test1[,.(x,y)]

print("inicio de la funcion buffer")

puntos1 <-buffer.f(test2, distancia, iteraciones)

print("buffer finalizado")

# hacemos un joint unir los puntos con la columna de ganancia (0-1)

puntos_valid1 <- left_join(puntos1, test1, by = c("x", "y"))

buffer_15 <- SpatialPointsDataFrame(puntos_valid1[,c("x","y")], puntos_valid1[,4:5])

buffer_15$id <- 1:nrow(buffer_15)

#writeSpatialShape(buffer_15, "c:/rlog/budi/buffer15.shp", factor2char = TRUE, max_nchar=254)

# Agrupar para poder contar las observaciones

group_valid1 <- group_by(puntos_valid1, puntos_valid1[,4])

#Contar los casos

a <- count(group_valid1)
num <- as.numeric(a[2,2])

# seleccionar igual numero de casos positivos que negativos

training_points<- sample_n(group_valid1, num)

# Transformar a un data.frame sin agrupar

training_points_libre <- as.data.frame(training_points)

training_points_libre$id <- 1:nrow(training_points_libre)

# Crear el data frame de puntos espaciales

puntos_validacion_15 <- SpatialPointsDataFrame(training_points_libre[,c("x","y")], training_points_libre[,3:7])

# Exportar como shapefile

#salida <- paste("budi0215",distancia, sep="")

writeSpatialShape(puntos_validacion_15, paste(names_intervalos, distancia,".shp", sep=""), factor2char = TRUE, max_nchar=254)

print(paste(names_intervalos, distancia, "exportado"), sep = "")

}

################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################



