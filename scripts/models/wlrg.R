# title: "Metodos y resultados preliminares GWLR lingue"
# author: "Cristian Vergara"
# date: "05/31/2022"



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



# Definir directorio de trabajo


### Load Data Training Data

training_data <- read.csv("training_data.csv")
test_data <- read.csv("test_data.csv")

## Split between training and validation data 


# Distance matrix calculation

coords_train <- cbind(d_train$x_1, d_train$y_1)

spdf_train <- SpatialPointsDataFrame(coords_train, d_train)

DM_train <- gw.dist(dp.locat = coords_train)


# Fit the logistic model

modelo_6 <- glm(gan_plant ~ aptitud_agricola_lingue + aptitud_forVII_lingue + bn_lingue_87 +
                    mat_lingue_87 + cul_prad_lingue_87 + dis_comunidades_lingue + dis_urbano_lingue + 
                    dis_caminos_lingue + dis_hid_lingue + contag_600 + division_600  + econ_mn_600 +
                    enn_mn_600 + pland_1  + pen_lingue15 + dis_plant87_lingue + predios_lingue1, 
                spdf_train, family = binomial(), na.action = "na.omit")

# GLMULTI


test_lingue_8701 <- glmulti(gali_8701_ ~ aptitud_agricola_lingue + aptitud_forVII_lingue + bn_lingue_87 + mat_lingue_87 + cul_prad_lingue_87 + 
                                dis_comunidades_lingue + dis_urbano_lingue + dis_caminos_lingue + dis_hid_lingue + contag_600 + shdi_600 + econ_mn_600 + 
                                enn_mn_600 + pland_1  + pen_lingue15 + dis_plant87_lingue + predios_lingue1,
                                level = 1, data = spdf_train, family = binomial, method = "h", confsetsize = 300, 
                                crit = aic, plotty = TRUE, report = TRUE)


# WGLR




# Validation 

summary(modelo_6)
summary(modelo_7)

anova(modelo_6, test ="LRT")
anova(modelo_7, test ="LRT")