###
### Flujo de trabajo para evaluar multiples modelos con la figure of merits. 
### y generar una imagen con los hits, omission, errors. 

## 1.- Cargar librerias

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
library(lulcc)

#### Cargar y primerar funciones y capas ###

###########################################################################
###########################################################################

source("F:/modelo/datos/modelo_8715/script/funciones_evaluar_modelo.R")

setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- rast("ras_gan_patches_8715_exp87.tif")

setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

###################################################################################

plantaciones_1987 <- rast('plantaciones_1987.tif')

ras_plantaciones_1987 <- raster('plantaciones_1987.tif')

plantaciones_2015_patches <- rast('plantaciones_2015_sim_patches.tif')

ras_plantaciones_2015_patches <- raster('plantaciones_2015_patches.tif')

setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final/escaladas")

variables = list.files(".", pattern = ".tif")

variables_escaladas = rast(variables)

######## Load the glmodel #########

load("F:/modelo/datos/modelo_8715/glmulti8715/glmulti_model_1000.RData")

# load("F:/modelo/datos/modelo_8715/glmulti8715/glmulti_model.RData")

# load("F:/modelo/datos/modelo_8715/glmulti8715/glmulti_model_sin_urbano.RData")

mask = terra::ifel(is.na(gan_patches_8715), NA, 1)

#### ejemplo de un transition potential map from a global model #####

transition_potential <- terra::predict(variables_escaladas,glmulti_model@objects[[1]], type = "response")

plot(transition_potential)


###### Mascara para quitar las zonas donde ya existian plantaciones ######

#### El proceso quedo dentro de la funcion simulacion ####

#### pero tenemos que darle como argumento la mascara de las plantaciones a eliminar #####

setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

library(terra)

plant_87 <- rast("plantaciones_1987.tif")

eliminar <-   c(1,NA,0,1)

reclassify <- matrix(eliminar, ncol = 2, byrow = TRUE)

mask_plantaciones_na <- terra::classify(plantaciones_1987, reclassify)

plot(mask_plantaciones_na)

# Evaluar simulacion

sim <- evaluar_simulacion(404905, glmulti_model@objects[[70]], mask= mask, mask_plantaciones = mask_plantaciones_na)

plot(sim)


######## Valor figure of merits con la funcion fom ####
### debe ir el mapa de referencia primero, y la simulacion segundo

fom(gan_patches_8715, sim)

##### Esto funciona y tiene mucho mejor variacion que el ROC ####

figure_list <- list()


for (i in 1:300){
  sim <- evaluar_simulacion(404905, glmulti_model@objects[[i]], mask= mask, mask_plantaciones = mask_plantaciones_na)
  fom_value <- fom(gan_patches_8715,sim)
  figure_list <- c(figure_list, list(fom_value))

}


####  Aqui voy: Transformar a DataFrame ####

df_Overall_figure <- data.frame(do.call(rbind, figure_list))

df_Overall_figure

### Ahora el aic


wgt <- weightable(glmulti_model)

AIC_modelos <- wgt$aic


AIC_modelos_300 <- AIC_modelos[1:300]

AIC_modelos_300

df_Overall_figure["AIC_modelos_300"] <- AIC_modelos_300


######## Ahora solo faltaria el ROC #######

df_Overall_figure

#################################################################

###### Evaluar AUC #######

setwd("F:/modelo/datos/modelo_8715/glmulti8715")

training_set <- read.csv("training_set.csv")
training_set <- na.omit(training_set)


testing_set <- read.csv("testing_set.csv")
testing_set <- na.omit(testing_set)


#############################################################################

training_AUC = list()

for (i in 1:300){
  AUC_value <- auc(training_set, glmulti_model@objects[[i]])
  training_AUC <- c(training_AUC, list(AUC_value))
}

training_AUC[1]

df_training_AUC <- data.frame(do.call(rbind, training_AUC))

#################################################

df_Overall_figure["training_AUC"] <- df_training_AUC

df_Overall_figure["Overfitting"] <- abs(df_Overall_figure$Training.AUC -df_Overall_figure$Testing.AUC)


###############################################################################

testing_AUC = list()

for (i in 1:300){
  AUC_value <- auc(testing_set, glmulti_model@objects[[i]])
  testing_AUC <- c(testing_AUC, list(AUC_value))
}

df_testing_AUC <- data.frame(do.call(rbind, testing_AUC))

df_Overall_figure["testing_AUC"] <- df_testing_AUC

colnames(df_Overall_figure) <- c("FOM","AIC", "Training.AUC","Testing.AUC")

setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

write.csv(df_Overall_figure)

####### Graficos de los AIC ######

library(ggplot2)

ggplot(data = df_Overall_figure, aes(x = AIC, y = FOM)) +
  geom_point()

ggplot(data = df_Overall_figure, aes(x = Training.AUC, y = FOM)) +
  geom_point()

ggplot(data = df_Overall_figure, aes(x = Testing.AUC, y = FOM)) +
  geom_point()

ggplot(data = df_Overall_figure, aes(x = AIC, y = Testing.AUC)) +
  geom_point()

ggplot(data = df_Overall_figure, aes(x = AIC, y = Testing.AUC)) +
  geom_point()


df_FM <- dplyr::select(df_Overall_figure, c("FM","AIC_modelos_100"))


# Generar un grafico que en una columna tenga los valores de AUC, y en otra columna tenga si 
# corresponde a Training.AUC o Testing AUC


dplyr::select(df_Overall_figure, Testing.AUC, training_AUC_300)


setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")





write.csv(df_FM, file = "DF_FM_lingue.csv")

## THE END ##

######## A NEW HOPE ###########

# SELECCIONAR MODELOS CON EL MEJOR FOM

# Tenemos que crear un identificador 

dplyr::filter(df_Overall_figure, FOM > 0.22)


glmulti_model@objects[[49]]$coefficients

glmulti_model@objects[[67]]$coefficients

glmulti_model@objects[[68]]$coefficients

glmulti_model@objects[[69]]$coefficients

glmulti_model@objects[[157]]$coefficients

glmulti_model@objects[[226]]$coefficients

glmulti_model@objects[[227]]$coefficients

glmulti_model@objects[[242]]$coefficients

####################

### Grafico de frecuencia de las variables 

setwd("F:/modelo/datos/modelo_8715/glmulti8715")


variables <- read.csv("variables_count.csv")

names(variables) <- c("Variables", "Count", "Frequency")

library(ggplot2)


library(forcats)



ggplot(data = variables, aes(y = Frequency, x = (forcats::fct_infreq(Variables)))) +
  geom_col() 


ggplot(data = variables, aes(y = Frequency, x = Variables)) +
  geom_col(fill = "#E9BB3E") + coord_flip()



ggplot(data = variables, aes(y = frequency())) +
  geom_bar()
  
### Vamos a crear las im?genes para las figures de figure of mertis para cada modelo seleccionado entre 
### entre los mejores modelos ###



################################################################################

### De aqui para abajo son pruebas y calculos antiguos

labels <- testing_set$gan_plant


predictions <- predict(glmulti_model@objects[[1]], type = "response", newdata = testing_set)

roc_curve <- pROC::roc(labels, predictions)
  
##############################

sim <- evaluar_simulacion(404905, glmulti_model@objects[[68]], mask= mask, mask_plantaciones = mask_plantaciones_na)


plot(evaluar_simulacion(404905, glmulti_model@objects[[68]], mask= mask, mask_plantaciones = mask_plantaciones_na))
plot(evaluar_simulacion(404905, glmulti_model@objects[[92]], mask= mask, mask_plantaciones = mask_plantaciones_na))

###### Ahora necesito colocar la funci?n FOM dentro de un bucle que me permita ##### 
###### calcular FOM dentro  de un loop #### 

#### Tengo que tener las imagenes

fom(gan_patches_8715,gan_sim)

fom(gan_patches_8715,gan_sim)

fom(gan_patches_8715,gan_sim)


source("F:/modelo/datos/modelo_8715/script/funciones_evaluar_modelo.R")

plot(figure_merits_images(gan_patches_8715, gan_sim))

plot(figure_merits_images_corregidas(gan_patches_8715, gan_sim))


## Simulaci?n ##

source("F:/modelo/datos/modelo_8715/script/funciones_evaluar_modelo.R")

setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

simulacion <- evaluar_simulacion(404905, glmulti_model@objects[[1]], mask= mask, mask_plantaciones = mask_plantaciones_na)

plot(simulacion)

ras_plantaciones_2015_sim_patches = raster(plantaciones_1987 + simulacion)

######### Simulacion #########

##################################

# Evaluar AUC

setwd("F:/modelo/datos/modelo_8715/glmulti8715")

training_set <- read.csv("training_set.csv")
training_set <- na.omit(training_set)

testing_set <- read.csv("testing_set.csv")
testing_set <- na.omit(testing_set)

#############################################################################

training_AUC = list()

for (i in 1:1000){
  AUC_value <- auc(training_set, glmulti_model@objects[[i]])
  training_AUC <- c(training_AUC, list(AUC_value))
}

training_AUC



###############################################################################

testing_AUC = list()


for (i in 1:100){
  AUC_value <- auc(testing_set, glmulti_model@objects[[i]])
  testing_AUC <- c(testing_AUC, list(AUC_value))
}

testing_AUC


#################################################################################
## Otra alternativa ser?a geenrar un nuevo muestreo para no reducir la muestra del ajuste
## Hacerlo al final.


## Lamentablemente la diferencia de AUC entre los mejores 1000 modelos es despreciable.

## Quizas tenga que volver a correr el modelo con una particion de los datos


###########################

## Ahora tengo que hacer mi propia funcion para calcular la figure of merits entre dos imagenes
## Revisar la formaula en los documentos


evaluar_simulacion


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

##### Creamos las capas para las figures of merits ####

# Para obtener la figure of merits necesitamos 3 imagenes de las cuales solo una cambia
# La de la simulacion. Los pixeles se encuentran fijos.


list_of_simulation = list()

for (i in 1:10){
  simulacion <- evaluar_simulacion(404905, glmulti_model@objects[[i]], mask= mask)
   list_of_simulation <- c(list_of_simulation, list(simulacion))
}


modelo <- c("Modelo")
secuencia_numeros <- seq(1:20)

######################################################
##### Evaluar la figure of merits de varios modelos #####

Overall_figure = list()


for (i in 1:100){
  
  ofm <- overall_figure_merits(404905, glmulti_model@objects[[i]], mask= mask)
  Overall_figure <- c(Overall_figure, list(ofm))
}


Overall_figure


df_Overall_figure <- data.frame(do.call(rbind, Overall_figure))


df_Overall_figure


wgt <- weightable(glmulti_model)


AIC_modelos <- wgt$aic


AIC_modelos_100 <- AIC_modelos[1:100]


AIC_modelos_100


df_Overall_figure["AIC_modelos_100"] <- AIC_modelos_100


df_Overall_figure$FM <- sapply(df_Overall_figure$do.call.rbind..Overall_figure., function(x) as.numeric(unlist(x)))


df_Overall_figure


library(ggplot2)


ggplot(data = df_Overall_figure, aes(x = FM, y = AIC_modelos_100)) +
         geom_point()


df_FM <- dplyr::select(df_Overall_figure, c("FM","AIC_modelos_100"))


setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")


write.csv(df_FM, file = "DF_FM_lingue.csv")

####################################################################################
           ################################################################
####################################################################################

#### Evaluar ROC 



##############################################################################################################################

predict_fm_51_1 <- terra::predict(variables_escaladas,glmulti_model@objects[[1]], type = "response")

predict_fm_51_94 <- terra::predict(variables_escaladas,glmulti_model@objects[[94]], type = "response")

predict_fm_46_70 <- terra::predict(variables_escaladas,glmulti_model@objects[[70]], type = "response")

predict_fm_52_49 <- terra::predict(variables_escaladas,glmulti_model@objects[[49]], type = "response")

predict_fm_39_13 <- terra::predict(variables_escaladas,glmulti_model@objects[[13]], type = "response")



plot(predict_fm_51_1, main ="Figure of merits 51, modelo 1, AIC = 853.8222")
plot(predict_fm_51_94, main = "Figure of merits 51, modelo 94, AIC = 856.6346")
plot(predict_fm_46_70, main = "Figure of merits 46, modelo 70, AIC = 855.9561")
plot(predict_fm_52_49,main = "Figure of merits 52, modelo 52, AIC = 856.0477" )
plot(predict_fm_39_13,main = "Figure of merits 13, modelo 39, AIC = 855.1647" )


simulacion_FM_51_1 <- evaluar_simulacion(404905, glmulti_model@objects[[1]], mask= mask)
simulacion_FM_52_49 <- evaluar_simulacion(404905, glmulti_model@objects[[49]], mask= mask)
simulacion_FM_46_70 <- evaluar_simulacion(404905, glmulti_model@objects[[70]], mask= mask)
simulacion_FM_39_13 <- evaluar_simulacion(404905, glmulti_model@objects[[13]], mask= mask)


plot(simulacion_FM_51_1, main ="Figure of merits 51, modelo 1, AIC = 853.8222" )
plot(simulacion_FM_52_49, main = "Figure of merits 52, modelo 52, AIC = 856.0477")
plot(simulacion_FM_46_70, main = "Figure of merits 46, modelo 70, AIC = 855.9561")
plot(simulacion_FM_39_13, main = "Figure of merits 13, modelo 39, AIC = 855.1647")

##############################################################################################################################

plot(predict_88)
plot(predict_su_43)
plot(predict_fm_52_49)

setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

terra::writeRaster(simulacion_fm_1, filename = "simulacion_urbano_1.tif", overwrite = TRUE)
terra::writeRaster(simulacion_fm_su_43, filename = "prob_sin_urbano_43_nuevo.tif", overwrite = TRUE)

terra::writeRaster(simulacion_fm_43, filename = "prob_urbano_88.tif", overwrite = TRUE)
terra::writeRaster(predict_su_43, filename = "prob_sin_urbano_43.tif", overwrite = TRUE)

terra::freq(simulacion_fm_43)
terra::freq(simulacion_fm_30)

terra::freq(simulacion_fm_su_43)
terra::freq(simulacion_fm_su_30)

###########################################################################
#### ?ltima parte: Validacion y FM modelo local ####


#####################################################################################

#####
# CARGAR DATOS IMPORTANTES #

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
library(lulcc)

#### Cargar y primerar funciones y capas ###

source("F:/modelo/datos/modelo_8715/script/funciones_evaluar_modelo.R")

setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- rast("m_lingue_Q_0.5ha_t.tif")

setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

plantaciones_1987 <- rast('plantaciones_1987.tif')

ras_plantaciones_1987 <- raster('plantaciones_1987.tif')

plantaciones_2015_patches <- rast('plantaciones_2015_sim_patches.tif')

ras_plantaciones_2015_patches <- raster('plantaciones_2015_patches.tif')

setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final/escaladas")

variables = list.files(".", pattern = ".tif")

variables_escaladas = rast(variables)

##################################

setwd("F:/modelo/datos/clasificacion_lingue/patches")

gan_patches_8715 <- rast("m_lingue_Q_0.5ha_t.tif")

setwd("F:/modelo/datos/modelo_8715/resultados/interpolacion/2024/marzo/mask")

files <- list.files(pattern = ".tif")

gwlr_probabilidades <- rast(files)


## probablemente esto no funcion por las dimensiones quizas deberiamos hacer el resample primero.


setwd("F:/modelo/datos/modelo_8715/resultados/interpolacion/2024/26_febrero/mask")

mask <- rast("mask.tif")

resample_gwlr_probabilidades <- terra::resample(gwlr_probabilidades,mask)

plot(resample_gwlr_probabilidades$inter_res_modelo_157)


##############################################################################################
##############################################################################################

## A esta imagen debo quitarle las zonas donde ya existen plantaciones ##


setwd("F:/modelo/datos/modelo_8715/glmulti8715/validacion")

library(terra)

plant_87 <- rast("plantaciones_1987.tif")

eliminar <-   c(1,NA,0,1)

reclassify <- matrix(eliminar, ncol = 2, byrow = TRUE)

mask_plantaciones_na <- terra::classify(plantaciones_1987, reclassify)

plot(mask_plantaciones_na)

mask_resample_gwlr_probabilidades <- (mask_plantaciones_na * resample_gwlr_probabilidades)

plot(mask_resample_gwlr_probabilidades)


##############################################################################################################################
##############################################################################################################################


terra::writeRaster(mask, filename = "mask_byte.rst", overwrite = TRUE, datatype = "INT1U", NAflag = 255)

terra::writeRaster(gwlr_probabilidades, filename = "mask_probabilities.rst", overwrite = TRUE, NAflag = 255)

terra::writeRaster(gan_patches_8715, filename = "gan_patches_8715_byte.rst", overwrite = TRUE, datatype = "INT1U", NAflag = 0)

terra::writeRaster(gan_patches_8715, filename = "gan_patches_8715_byte1.rst", overwrite = TRUE)

terra::writeRaster(resample_gwlr_probabilidades, filename = "resample_gwlr_probabilidades_1.rst", overwrite = TRUE, NAflag = -9999)

terra::writeRaster(simulacion, filename = "simulacion.rst", overwrite = TRUE, datatype = "INT1U")

#########################

ranking_values_raster <- function(imagen){
  imagen_values <- terra::values(imagen)
  imagen_ranked_values <- rank(imagen_values, na.last = 'keep', ties.method = 'average')
  terra::values(imagen) <- imagen_ranked_values
  return(imagen)
}

reclasificar_cantidad <- function(imagen_ranking,cantidad,mask){
  gan_sim_pl <- ifel(imagen_ranking > cantidad, 1,0)
  mask_gan_sim_pl <- gan_sim_pl*mask
  return(mask_gan_sim_pl)
}


ranking_probabilidades <- ranking_values_raster(mask_resample_gwlr_probabilidades)

freq_wglr_ran  <- terra::freq(ranking_probabilidades)

df_freq_wglr_ran <- as.data.frame(freq_wglr_ran)

num_mayor <- dplyr::filter(df_freq_wglr_ran, count > 1)

terra::res(ranking_probabilidades)

###################################################

### freq_wglr_ran = 492677 pixeles

## sim_wglr <- reclasificar_cantidad(ranking_probabilidades,404905, mask = mask)

## gan_patches_8715 total pixeles 584880

sim_wglr_ly1 <- ifel(ranking_probabilidades$lyr1 > 2463378, 1,0) # Numero justo de pixeles

terra::freq(m_sim_wglr_ly1)
terra::freq(gan_patches_8715)

# Hay que ajustar las cantidades de aqui hacia abajo.

##################################################
##################################################

sim_wglr_ly2 <- ifel(ranking_probabilidades$lyr2 > 2454750, 1,0) # Numero justo de pixeles

m_sim_wglr_ly2 <- (sim_wglr_ly2 * mask)

terra::freq(m_sim_wglr_ly2)
terra::freq(gan_patches_8715)

#####################

sim_wglr_ly3 <- ifel(ranking_probabilidades$lyr3 > 2440178, 1,0) # Falta calibrar un poco

m_sim_wglr_ly3 <- (sim_wglr_ly3 * mask)

terra::freq(m_sim_wglr_ly3)
terra::freq(gan_patches_8715) 


######################################

sim_wglr_ly4 <- ifel(ranking_probabilidades$lyr4 > 2410378, 1,0) # Falta calibrar un poco

m_sim_wglr_ly4 <- (sim_wglr_ly4 * mask)

terra::freq(m_sim_wglr_ly4)
terra::freq(gan_patches_8715)

################################################

sim_wglr_ly5 <- ifel(ranking_probabilidades$lyr5 > 2410000, 1,0) # Falta calibrar un poco

m_sim_wglr_ly5 <- (sim_wglr_ly5 * mask)

terra::freq(m_sim_wglr_ly5)
terra::freq(gan_patches_8715) 

######################################################


sim_wglr_ly6 <- ifel(ranking_probabilidades$lyr6 > 2390000, 1,0) # Falta calibrar un poco

m_sim_wglr_ly6 <- (sim_wglr_ly6 * mask)

terra::freq(m_sim_wglr_ly6)
terra::freq(gan_patches_8715) 

################################################################


sim_wglr_ly7 <- ifel(ranking_probabilidades$lyr7 > 2410000, 1,0) # Falta calibrar un poco

m_sim_wglr_ly7 <- (sim_wglr_ly7 * mask)

terra::freq(m_sim_wglr_ly7)
terra::freq(gan_patches_8715) 


############################################################################
############################################################################

## FOM ##

fom(gan_patches_8715,m_sim_wglr_ly1) 
fom(gan_patches_8715,m_sim_wglr_ly2)
fom(gan_patches_8715,m_sim_wglr_ly3)
fom(gan_patches_8715,m_sim_wglr_ly4)
fom(gan_patches_8715,m_sim_wglr_ly5)
fom(gan_patches_8715,m_sim_wglr_ly6)
fom(gan_patches_8715,m_sim_wglr_ly7)


################################################################################################
## FOM IMAGES ##

plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly1))
plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly2))
plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly3))
plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly4))
plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly5))
plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly6))
plot(figure_merits_images(gan_patches_8715, m_sim_wglr_ly7))


################################################################################################

#sim_wglr <- ifel(ranking_probabilidades > 404905, 1,0)


freq_lyr1 <- terra::freq(ranking_probabilidades$lyr1)

df_freq_wglr_ran <- as.data.frame(freq_lyr1)

num_mayor <- dplyr::filter(df_freq_wglr_ran, count > 1)

num_mayor

##################################### 

#### Ahora podemos hacer la figure of merits con la funcion recientemente contruida ####

source("F:/modelo/datos/modelo_8715/script/funciones_evaluar_modelo.R")


fom(gan_patches_8715,m_sim_wglr) 

plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly1), main = "modelo 157, FOM = 0.3455")
plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly2), main = "modelo 226, FOM = 0.3457")
plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly3), main = "modelo 227, FOM = 0.3322")
plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly4), main = "modelo 242,  FOM = 0.3439")
plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly5), main = "modelo 49, FOM = 0.34820")
plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly6), main = "modelo 67, FOM = 0.3488")
plot(figure_merits_images_corregido(gan_patches_8715,m_sim_wglr_ly6), main = "modelo 69, FOM = 0.3522")

###########################

###### ?C?mo podemos usar todos los pixeles para hacer la capa de la simulacion? ######

## Variables originales

### Necesitamos importar las variables originales escaladas



setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final/escaladas")

bn_lingue_87 <- rast("bn_lingue_87.tif")
dis_comunidades_lingue_rescaled <- rast("dis_comunidades_lingue_rescaled.tif")
dis_urbano_lingue_rescaled <- rast("dis_urbano_lingue_rescaled.tif")
division_600_rescaled <- rast("division_600_rescaled.tif")
econ_mn_600_rescaled <- rast("econ_mn_600_rescaled.tif")
pend_lingue_rescaled <- rast("pend_lingue_rescaled.tif") 


plot(bn_lingue_87)
plot(dis_comunidades_lingue_rescaled)
plot(dis_urbano_lingue_rescaled)
plot(division_600_rescaled)
plot(econ_mn_600_rescaled)
plot(pend_lingue_rescaled)

#### Variables originales

setwd("F:/modelo/datos/modelo_8715/datos_modelo/modelo_final")

dis_comunidades_lingue <- rast("dis_comunidades_lingue.tif")
dis_urbano_lingue <- rast("dis_urbano_lingue.tif")
division_600 <- rast("division_600.tif")
econ_mn_600 <- rast("econ_mn_600.tif")
pend_lingue <- rast("pend_lingue.tif") 


########################################################
#######################################################

setwd("F:/modelo/datos/modelo_8715/resultados/interpolacion/2024/marzo/model157/mask")


#files <- list.files(pattern = ".tif")
#gan_patches_8715 <- rast("m_lingue_Q_0.5ha_t.tif")

#ras_files <- terra::rast(files)

m_intercept_152 <- rast("m_intercept_152.tif")
re_m_intercept_152 <- terra::resample(m_intercept_152,dis_comunidades_lingue)


m_bn <- rast("m_bn.tif")
re_m_bn <- terra::resample(m_bn,bn_lingue_87)

plot(re_m_bn)

m_dis_comunidades <- rast("m_dis_comunidades.tif")
re_m_dis_comunidades <- terra::resample(m_dis_comunidades,dis_comunidades_lingue)

plot(m_dis_comunidades)

m_dis_urbano <- rast("m_dis_urbano.tif")
re_m_dis_urbano <- terra::resample(m_dis_urbano,dis_urbano_lingue)

plot(m_dis_urbano)

m_division <- rast("m_division.tif")
re_m_division <- terra::resample(m_division,division_600)

plot(re_m_division)

m_econ <- rast("m_econ_mn.tif")
re_m_econ <- terra::resample(m_econ,econ_mn_600)

plot(re_m_econ)

m_pend <- rast("m_pend_mn.tif")
re_m_pend <- terra::resample(m_pend,pend_lingue)

plot(re_m_pend)

###############################################
####### Tenemos que crear una mascara para las imagenes que vienen de las metricas del paisaje #######

## La mascara ##


eliminar <-   c(0,NA)

re_eliminar <- matrix(eliminar, ncol = 2, byrow = TRUE)

mask_na <- terra::classify(division_600_rescaled, re_eliminar)

reclass <-   c(0,0.9999999,1)


re <- matrix(reclass, ncol = 3, byrow = TRUE)

mask_division <- terra::classify(mask_na, re)

plot(mask_division)

###############################

####### Ahora debemos multiplicar cada una de las capas ############

m_bn_lingue <- bn_lingue_87 * mask_division 

plot(m_bn_lingue)

m_dis_comunidades <- dis_comunidades_lingue_rescaled  * mask_division

m_dis_urbano_rescaled <- dis_urbano_lingue_rescaled  * mask_division

m_division_rescaled <- division_600_rescaled 

m_econ_mn_600_rescaled <- econ_mn_600_rescaled  * mask_division

m_pend_rescaled <- pend_lingue_rescaled * mask_division

####### Multiplicar los coefficientes

m_re_interecept_152 <- re_m_intercept_152 *  mask_division

m_re_m_bn <- re_m_bn * mask_division

m_re_m_dis_comunidades <- re_m_dis_comunidades * mask_division

m_re_m_dis_urbano <- re_m_dis_urbano * mask_division

m_re_m_division <- re_m_division  * mask_division

m_re_m_econ <- re_m_econ  * mask_division

m_re_m_pend <- re_m_pend * mask_division

##################################################################################################



step_1_rescaled <-(bn_lingue_87* re_m_bn) + (dis_comunidades_lingue_rescaled * re_m_dis_comunidades) +(dis_urbano_lingue_rescaled * re_m_dis_urbano)+
  (division_600_rescaled * m_re_m_division )+(econ_mn_600_rescaled * re_m_econ) + (pend_lingue_rescaled * re_m_pend)


plot(step_1_rescaled)

terra::writeRaster(step_1_rescaled, filename = "step_1_rescaled.tif", overwrite = TRUE)

plot(step_1)

plot(m_re_interecept_152)

####  debemos a?adir el intercept #####

step_2 = m_re_interecept_152 + step_1_rescaled

plot(step_2)

terra::writeRaster(step_2, "step_2.tif")

plot(step_1_rescaled)

plot(m_re_interecept_152)

plot(step_2)

step_3 <- exp(step_2)

plot(step_3)

####

step_4 <- step_3 + 1

plot(step_4)

model_1 <- step_3/step_4

plot(model_1)


setwd("F:/modelo/datos/modelo_8715/resultados/interpolacion/2024/marzo/mask")

terra::writeRaster(model_1, filename = "manual_transition_potential.tif")

###########################################################################################################
###########################################################################################################


#############################################################################################################
#############################################################################################################