### Como podemos utilizar la funcion predict para generar el modelo de probabilidades 
### Tanto del modelo global como el geograficamente ponderado
### 

library(GWmodel)
library(sp)
library(dplyr)
library(scales)
library(pROC)
library(ape)
library(gridExtra)
library(sf)
library(terra)
library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(car)
library(gstat)
library(sp)
library(spatstat)
library(knitr)
library(kableExtra)
library(DT)
library(glmulti)
library(raster)


#### Leer funciones de otro script

source("F:/modelo/datos/modelo_8715/script/funciones_evaluar_modelo.R")

### Cantidad de superficie a nivel original ###

setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- rast('m_lingue_Q_0.5ha_t.tif')
freq_uso <- terra::freq(gan_patches_8715)
dt_freq_uso = as.data.frame(freq_uso)
dt_freq_uso['area'] = dt_freq_uso['count'] * 0.09 

## importar datos para calibrar el modelo

setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final")

naturalidad <- rast("naturalidad_600.tif")

naturalidad_escalada <- app(naturalidad,custom_scale)

naturalidad

plot(naturalidad_escalada)

names(naturalidad_escalada) <- "pland_1"

terra::writeRaster(naturalidad_escalada, filename = "F:/modelo/datos/modelo_8715/datos_modelo/modelo_final/escaladas/pland_1.tif")

plot(naturalidad)


### Imagen Pland_1

rast("Pland_1.tif")

pl_data <- read.csv("datos_modelo_completo.csv")

### Eliminamos na ##

pl_data <- na.omit(pl_data)

### Eliminamos puntos repetidos

pl_data_2 <- distinct(pl_data, x_1, y_1, .keep_all = TRUE)

## Escalar datos

for (i in 5:24){
  pl_data_2[,i] <- scales::rescale(pl_data_2[,i], c(0,1)) 
  
}

## Generar datos de train y de test

d_train <- pl_data_2

d_test <- pl_data_2

## Coordenadas, spatialdataframe y calculo de distancias

coords_train <- cbind(d_train$x_1, d_train$y_1)

spdf_train <- SpatialPointsDataFrame(coords_train, d_test)

DM_train <- gw.dist(dp.locat = coords_train, rp.locat = coords_train) 

## Coordenadas, spatialdataframe y calculo de distancias

coords_test <- cbind(d_test$x_1, d_test$y_1)

spdf_test <- SpatialPointsDataFrame(coords_test, d_test)

DM_test <- gw.dist(dp.locat = coords_test, rp.locat = coords_test) 


# Importar capas espaciales escaladas
setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final/escaladas")

variables = list.files(".", pattern = ".tif")

variables_escaladas = rast(variables)


# Modelos 

options(scipen=999)


### Generar distintos modelos.

modelo_1 <- glm(layer ~ aptitud_agricola_lingue + dem_lingue +
                   bn_lingue_87 + mat_lingue_87 + cul_prad_lingue_87 + 
                   dis_urbano_lingue + dis_comunidades_lingue +
                    division_600 + pen_lingue15 + dis_plant87_lingue , spdf_train, 
                 family = binomial(), na.action = "na.omit")


modelo_2 <- glm(layer ~ aptitud_agricola_lingue +
                  bn_lingue_87 + mat_lingue_87 + cul_prad_lingue_87 + 
                  dis_urbano_lingue + division_600 +
                  econ_mn_600 + pen_lingue15, spdf_train, 
                family = binomial(), na.action = "na.omit")


modelo_3 <- glm(layer ~ aptitud_agricola_lingue +
                  bn_lingue_87 + mat_lingue_87 + cul_prad_lingue_87 + 
                   division_600 +
                  pen_lingue15 + dis_plant87_lingue, spdf_train, 
                family = binomial(), na.action = "na.omit")


modelo_4 <- glm(layer ~ dem_lingue + bn_lingue_87 + mat_lingue_87 + cul_prad_lingue_87,  spdf_train, 
                family = binomial(), na.action = "na.omit")


prob1 = terra::predict(variables_escaladas, modelo_1, type = "response")
prob2 = terra::predict(variables_escaladas, modelo_2, type = "response")
prob3 = terra::predict(variables_escaladas, modelo_3, type = "response")
prob4 = terra::predict(variables_escaladas, modelo_4, type = "response")

mask = terra::ifel(is.na(gan_patches_8715), NA, 1)

### Funcion ranking

ranking_prob1 <- ranking_values_raster(prob1)
ranking_prob2 <- ranking_values_raster(prob2)
ranking_prob3 <- ranking_values_raster(prob3)
ranking_prob4 <- ranking_values_raster(prob4)


### Funcion reclasificar

gan_sim1 <-reclasificar_cantidad(ranking_prob1,404905, mask)
gan_sim2 <-reclasificar_cantidad(ranking_prob2,404905, mask)
gan_sim3 <-reclasificar_cantidad(ranking_prob3,404905, mask)
gan_sim4 <-reclasificar_cantidad(ranking_prob4,404905, mask)


plot(gan_sim1)
plot(gan_sim2)

## Modelo sin urbano 
plot(gan_sim3)
plot(gan_sim4)



terra::freq(gan_sim1)
terra::freq(gan_sim2)
terra::freq(gan_sim3)
terra::freq(gan_sim4)


plot(gan_patches_8715)

# Exportar.

setwd("F:/modelo/datos/modelo_8715/datos_modelo")

terra::writeRaster(test_mask, filename = "sim_gan_plant8715_global.tif", 
                   overwrite = TRUE)


#################################################################################

### Calibrar el modelo con todos los puntos.


library(GWmodel)
library(sp)
library(dplyr)
library(scales)
library(pROC)
library(ape)
library(gridExtra)
library(sf)
library(terra)
library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(car)
library(gstat)
library(sp)
library(spatstat)
library(knitr)
library(kableExtra)
library(DT)
library(glmulti)
library(raster)


setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- raster('m_lingue_Q_0.5ha_t.tif')

gan_patches_8715_points <- raster::rasterToPoints(gan_patches_8715, spatial = TRUE)

# Transformamos de sptialdatapoints a vector para poder usar el terra:: extract

gan_patches_8715_vect <- vect(gan_patches_8715_points)

## Estos estan sin datos. Debe extraer para cada punto el valor en la informacion espacial

### Importamos las capas espaciales ###

setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final/escaladas")

variables = list.files(".", pattern = ".tif")

variables_escaladas = rast(variables)

muestra_imagen_completa <- terra::extract(variables_escaladas, gan_patches_8715_vect, xy = TRUE)

### Debemos eliminar los NA de los datos muestra imagen completa. Esto porque las capas### TRUEDebemos eliminar los NA de los datos muestra imagen completa. Esto porque las capas
### de aptitud del suelo son más pequeñas.

d_train <- na.omit(muestra_imagen_completa)

# debo retroceder un paso para tener el valor de las coordenadas


coords_train <- cbind(d_train$x, d_train$y)
spdf_train <- SpatialPointsDataFrame(coords_train, d_train)  

## No es valida en este punto debido a problemas de memoria.

DM_train <- gw.dist(dp.locat = coords_train)

#####  4- Probaremos el enfoque de poder cargar el objeto de glmulti y a partir de los mejores modelos
##### poder realizar una simulacion


prob1 = terra::predict(variables_escaladas, glmulti_model@objects[[1]], type = "response")
prob2 = terra::predict(variables_escaladas, glmulti_model@objects[[2]], type = "response")
prob3 = terra::predict(variables_escaladas, glmulti_model@objects[[3]], type = "response")
prob4 = terra::predict(variables_escaladas, glmulti_model@objects[[4]], type = "response")

library(terra)


setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- rast('m_lingue_Q_0.5ha_t.tif')

mask = terra::ifel(is.na(gan_patches_8715), NA, 1)


evaluar_simulacion <- function(num_pixels,model,mask){
  prob = terra::predict(variables_escaladas, model, type = "response")
  ranking_prob <- ranking_values_raster(prob)
  gan_sim <-reclasificar_cantidad(ranking_prob,num_pixels, mask)
  return(gan_sim)
  
}

simulacion <- evaluar_simulacion(404905, glmulti_model@objects[[1]], mask= mask)


for (i in 1:10){
  
  evaluar_simulacion(404905, glmulti_model@objects[[i]], mask= mask)

}

# Falta calcular aqui como calcular figure of merits. 


#############################################################################################


### Funcion ranking

ranking_prob1 <- ranking_values_raster(prob1)
ranking_prob2 <- ranking_values_raster(prob2)
ranking_prob3 <- ranking_values_raster(prob3)
ranking_prob4 <- ranking_values_raster(prob4)

### Funcion reclasificar

gan_sim1 <-reclasificar_cantidad(ranking_prob1,404905, mask)
gan_sim2 <-reclasificar_cantidad(ranking_prob2,404905, mask)
gan_sim3 <-reclasificar_cantidad(ranking_prob3,404905, mask)
gan_sim4 <-reclasificar_cantidad(ranking_prob4,404905, mask)

plot(gan_sim1)
plot(gan_sim2)

## Modelo sin urbano 

plot(gan_sim3)
plot(gan_sim4)


terra::freq(gan_sim1)
terra::freq(gan_sim2)
terra::freq(gan_sim3)
terra::freq(gan_sim4)


plot(gan_patches_8715)

########################################################
##### Figure of merits #################################


library(terra)

setwd("F:/modelo/datos/clasificacion_lingue")


lingue_1987 <- rast("c_pl_1987_class_final.tif")

# reclasificar solo a las plantaciones 

plot(lingue_1987)

reclass <- c(0,1,0,1,2,1,2,9,0)
m_reclass <- matrix(reclass, ncol = 3, byrow = TRUE)

plantaciones_1987 <- terra::classify(lingue_1987, m_reclass)

terra::writeRaster(plantaciones_1987, filename = "F:/modelo/datos/modelo_8715/glmulti8715/validacion/plantaciones_1987.tif")

# La plantacion original es la suma de la plantacion de 1987 mas la ganancia utilizada

setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- rast('m_lingue_Q_0.5ha_t.tif')

plantaciones_2015_patches <- gan_patches_8715 + plantaciones_1987

terra::writeRaster(plantaciones_2015_patches, filename = "F:/modelo/datos/modelo_8715/glmulti8715/validacion/plantaciones_2015_patches.tif")
                   
plantaciones_2015_sim_patches <- plantaciones_1987 + simulacion

terra::writeRaster(plantaciones_2015_sim_patches, filename = "F:/modelo/datos/modelo_8715/glmulti8715/validacion/plantaciones_2015_sim_patches.tif")


#### Ahora podemos hacer los ThreeMaps comparison ####

library(lulcc)

three_maps_plantaciones <- ThreeMapComparison(plantaciones_1987,plantaciones_2015_patches, plantaciones_2015_sim_patches)

ras_plantaciones_1987 <- raster(plantaciones_1987)
ras_plantaciones_2015_patches <- raster(plantaciones_2015_patches)
ras_plantaciones_2015_sim_patches <- raster(plantaciones_2015_sim_patches)

ThreeMaps_plantaciones <- ThreeMapComparison(ras_plantaciones_1987, ras_plantaciones_2015_patches, ras_plantaciones_2015_sim_patches, factors  = 30,
                   categories = c(0,1), labels = c("no plantacion", "plantacion"))


### Ahora a partir de las threemaps comparison podemos hacer las figuras of merits.

Fig_merits_plantaciones <- lulcc::FigureOfMerit(ThreeMaps_plantaciones)

# Podemos sacar el valor de la figure of mertis global y colocarlo en una fila para seleccionar
# la reclasificacion de las imagenes debe ser manualmente

# 1. Reclasificar:
# simulado y real (1-1) son hits
# simulado y no real (1-1) errors
# no simulado y real (0-1) son miss
# no-simulado y no real (0-0) nada ejje4


figure_image <- function(imagen_1, imagen_2) {
  ifel(imagen_1 == 1 & imagen_2 == 1, 3,
       ifel(imagen_1 == 0 & imagen_2 == 1, 2,
            ifel(imagen_1 == 1 & imagen_2 == 0, 1,  # Corrected condition
                 ifel(imagen_1 == 0 & imagen_2 == 0, 0, NA))))  # Added condition for 0 & 0
}


 <- figure_image(plantaciones_2015_patches,plantaciones_2015_sim_patches)

plot(plantaciones)


# Ahora tenemos todo el flujo de trabajo.

# Falta programar que los resultados 






