### Como podemos utilizar la funcion predict para generar el modelo de probabilidades 
### Tanto del modelo global como el geograficamente ponderado

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

setwd("F:/modelo/datos/clasificacion_lingue/patches") # Colocar estas rutas en configuraciones


## CArgas capas. Calculamos la superficie de plantaciones.numero, de pixel.
## Esto se puede transformar en una funcion

# STEP 1: CALCULATE AREA OF REAL GAINS OF FOREST PLANTATION BETWEEN 1987-2015 


## importar datos para calibrar el modelo

setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final") # Rutas a configuracion

### Hacemos la predicciones a partir de los modelos creados. 
## ¿Se puede hacer predict directamente sobre los modelos de glmulti?
## Transformar el predict, el ranking y el reclasificar en una sola funcion
## PAra obtener la ganancia para una superficie determinada que es la superficie
## de ganancia original.


mask = terra::ifel(is.na(gan_patches_8715), NA, 1)


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

gan_patches_8715 <- rast('m_lingue_Q_0.5ha_t.tif')

mask = terra::ifel(is.na(gan_patches_8715), NA, 1)

simulacion <- evaluar_simulacion(404905, glmulti_model@objects[[1]], mask= mask)


for (i in 1:10){
  
  evaluar_simulacion(404905, glmulti_model@objects[[i]], mask= mask)

}

# Calcular la Figure of merits de forma iterativa para cada modelo

# Falta calcular aqui como calcular como calcular the figure of merits para cada glmulti_model

# Revisar esto, porque esta trabajado en una funcion. 

#############################################################################################


ranking_prob1 <- ranking_values_raster(prob1)

### Funcion reclasificar

gan_sim1 <-reclasificar_cantidad(ranking_prob1,404905, mask)

plot(gan_sim1)

## Modelo sin urbano 

plot(gan_sim3)


terra::freq(gan_sim1)

plot(gan_patches_8715)

########################################################
##### Figure of merits #################################

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






