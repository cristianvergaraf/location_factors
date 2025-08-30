# Este script tiene el objetivo de producir el dataframe con los valores
# tanto de la variable independiente en este caso la expansion de las plantaciones forestales, y las variables depenendientes o factores de localizacion.

# Variables de entrada son un shape de puntos que corresponde al mustreo sobre la capa de expansion de plantaciones forestales.

# Un conjunto de imágenes raster asociadas a las variables dependientes o factores de localización asociadas potencialmente a la expansion de las plantaciones forestales

# Salida del script: Es un dataframe en csv con el valor de la variable dependiente y las variables independientes para cada punto de muestreo (shapefile)

# TODO:: Restructurar esto como una funcion.

library(terra)
library(sf)
library(dplyr)
library(tmap)

#setwd("C:/Users/Cristian uah/Universidad de Alcala/Publicaciones - Spatial logistic model forest plantation chile/datos6/modelo_8701")

setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8701/datos_modelo")

#setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8701/datos_6")

#setwd("C:/Users/Cristian uah/Universidad de Alcala/Publicaciones - Spatial logistic model forest plantation chile/datos/modelo_8701/datos_modelo")

setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8715/datos_modelo")


files <- list.files(path = ".", "*.tif$")
raster_lingue <- rast(files)
rapply_lingue <- lapply(files,rast)

# Cargar datos shape muestreo lingue

setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8715/sample")


lingue_puntos <- st_read("puntos_training_8715.shp")
lingue_puntos_18s <- st_set_crs(lingue_puntos, "epsg:32718")

# Cargar puntos de validacion

#lingue_puntos_validacion <- st_read("../validacion/gali8701_he05_900.shp")
#lingue_puntos_validacion_18s <- st_set_crs(lingue_puntos_validacion, "epsg:32718")

#group_by(lingue_puntos_validacion_18s, gali_8701_) %>% summarise(n())

# Leer los datos usando el formato spatvec

lingue_pts_vec <- vect("puntos_training_8715.shp", crs = "epsg:32718")

#lingue_pts_validacion <- vect("../validacion/gali8701_he05_900.shp", crs = "epsg:32718")

# Pasar de formato sf a Spatvec

vec_sf <- vect(lingue_puntos_18s) 
#vec_sf_val <- vect(lingue_puntos_validacion_18s)

# Extraemos los valores de cada raster utilizando los puntos
# del vector lingue pts vec

values_ext <- terra::extract(raster_lingue, lingue_pts_vec, xy = TRUE, cells = TRUE)

#values_ext_val <- terra::extract(raster_lingue, lingue_pts_validacion, xy = TRUE, cells = TRUE)
# eliminamos una columan duplicada 


sf_li_pts_18s_datos <- right_join(lingue_puntos_18s, values_ext, by = c("id" = "ID"))
#sf_li_pts_18s_datos_validacion <- right_join(lingue_puntos_validacion_18s, values_ext_val, by = c("id" = "ID"))


class(sf_li_pts_18s_datos)
#class(sf_li_pts_18s_datos_validacion)

tmap_mode("view")
tm_shape(sf_li_pts_18s_datos) +
  tm_dots(size = 0.01, col =c("dis_plant87_lingue","dis_hid_lingue","pland_1"))

tmap_mode("view")
tm_shape(sf_li_pts_18s_datos_validacion) +
  tm_dots(size = 0.01, col =c("dis_plant87_lingue","dis_hid_lingue","pland_1"))

setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8715/datos_modelo")

datos_wrlg_8715 <- st_drop_geometry(sf_li_pts_18s_datos)
datos_wrlg_8701_validacion <- st_drop_geometry(sf_li_pts_18s_datos_validacion)



library(dplyr)


datos_cor <- dplyr::select(datos_wrlg_8715, "contag_600","dem_lingue","dis_caminos_lingue","dis_hid_lingue",
                    "dis_plant87_lingue","dis_urbano_lingue", "econ_mn_600",
                  "enn_mn_600", "pland_1", "pend_lingue", "predios_lingue1")




cor_metricas <- cor(datos_cor, method = "pearson", use = "complete")

# Definimos una escala de colores

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))
library(corrplot)

corrplot(cor_metricas, method = "color", 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", number.digits = 2, number.cex = 0.6)




datos_final <- dplyr::select(datos_wrlg_8715, c("layer", "x_1", "y_1", "pen_lingue15","aptitud_agricola_lingue", "aptitud_forVII_lingue", "bn_lingue_87", "mat_lingue_87","cul_prad_lingue_87",
                                                "dem_lingue","dis_caminos_lingue","dis_hid_lingue","dis_plant87_lingue","dis_urbano_lingue", "dis_comunidades_lingue", "contag_600","econ_mn_600",
                                       "enn_mn_600", "division_600", "shdi_600", "pland_1", "pend_lingue" , "predios_lingue1"))

datos_final_validacion <- dplyr::select(datos_wrlg_8701_validacion, c("gali_8701_", "x_1", "y_1", "contag_600","dem_lingue","dis_caminos_lingue","dis_hid_lingue","dis_plant87_lingue","dis_urbano_lingue", "econ_mn_600",
                                                "enn_mn_600", "pland_1", "pend_lingue", "predios_lingue1"))


View(datos_final)
View(datos_final_validacion)

setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8715/datos_modelo")


write.csv(datos_final, file = "datos_modelo_completo.csv")
st_write(sf_li_pts_18s_datos, dsn = "sf_li_pts_18s_datos_completo.shp")
st_write(sf_li_pts_18s_datos, dsn = "li_datos8715_completo.gpkg", layer = "li_pts_18s", delete_layer = TRUE)

setwd("C:/Users/CRISTIAN/Universidad de Alcala/PUBLICACIONES UAH - Spatial logistic model forest plantation chile/datos/modelo_8701/validacion")

write.csv(datos_final_validacion, file = "datos_modelo_mv_validacion.csv")

st_write(sf_li_pts_18s_datos_validacion, dsn = "sf_li_pts_18s_datos_MV_validacion.shp")
st_write(sf_li_pts_18s_datos_validacion, dsn = "li_datos8701_MV_validacion.gpkg", layer = "li_pts_18s", delete_layer = TRUE)



