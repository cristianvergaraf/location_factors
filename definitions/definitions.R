# Base formula

form_modelo_6 <- as.formula(
    gan_plant_8715 ~ aptitud_forVII_lingue + bn_lingue_87 +
        mat_lingue_87 + cul_prad_lingue_87 + dis_comunidades_lingue + dis_urbano_lingue + 
        dis_caminos_lingue + dis_hid_lingue +  pen_lingue15 + dis_plant87_lingue + predios_lingue1
)


# Parameters for glmulti()

# definitions.R
glmulti_args <- list(
    formula = form_modelo_6,
    data = spdf_train,
    level = 1,
    family = binomial,
    method = "h",
    confsetsize = 300,
    crit = "aic",
    plotty = TRUE,
    report = TRUE
)
