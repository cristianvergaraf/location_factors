## Este script permite hacer un muestreo de datos separados por una distancia
## TODO: Transformar esto en una función.


#---------------------------------------------------------------------------#

#####

### Flujo de trabajo para hacer un muestreo por distancias ###


# 1.- Cargar librerias 

library(terra)
library(raster)
library(sf)
library(dplyr)

# Cargar y primerar funciones y capas 


setwd("~/github/location_factors")

gan_patches_8715_expl87 <- rast("datos/processed/raster/patches/ras_gan_patches_8715_exp87.tif")

ras_gan_patches_8715_expl87 <- raster("datos/processed/raster/patches/ras_gan_patches_8715_exp87.tif")

source("scripts/muestreo/buffer.R")


s <- spatSample(gan_patches_8715_expl87, 4000, method = "stratified", as.points=TRUE, na.rm = TRUE, warn = TRUE)

test <- sampleStratified(ras_gan_patches_8715_expl87, 2000, exp=1, na.rm=TRUE, xy=TRUE, ext=NULL, sp=TRUE)

plot(test)

test1 <- as.data.frame(test)

test2 <- select(test1, x,y)

#test2 <- select(test1, x,y)

sf_test <- st_as_sf(test)

puntos1 <-buffer.f(test2, 500, 1000)

puntos_valid <- inner_join(test1, puntos1, by = c("x", "y"))

dplyr::count(puntos_valid,ras_gan_patches_8715_exp87)

puntos_valid_gr <- group_by(puntos_valid, ras_gan_patches_8715_exp87)

# Para que lo deje al mismo numero de puntos hay que agruparlo antes #

puntos_valid_select <- sample_n(puntos_valid_gr, 150)

puntos_500_m_sf <- st_as_sf(puntos_valid_select, coords = c("x","y"))

## Exportar

st_write(puntos_500_m_sf, dsn = "datos/processed/vector/test_gan_8715_500m.gpkg", layer = "test_lingue_8715_sin_plantaciones", crs = 32718)

#########################################################################################

