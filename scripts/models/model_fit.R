## load package

setwd("~/github/location_factors")
source("config/load_packages.R")
source("scripts/helpers/model_tracking.R")
source("scripts/helpers/model_name.R")

# Set working directory

source("config/paths.R")

### Load Data Training Data

spdf_train <- read.csv(training_data_file)

# Fit the logistic model

source("definitions/definitions.R")

modelo_6 <- glm(
    formula = form_modelo_6,
    data = spdf_train,
    family = binomial(),
    na.action = "na.omit"
)

# GLMULTI



# TODO: Crossvalidation manualmente y correr el glmulti con for

glmulti_model <- glmulti(
    glmulti_args$formula,
    data = glmulti_args$data,
    level = glmulti_args$level,
    family = glmulti_args$family,
    method = glmulti_args$method,
    confsetsize = glmulti_args$confsetsize,
    crit = glmulti_args$crit,
    plotty = glmulti_args$plotty,
    report = glmulti_args$report
)

# TODO: VALIDACION TENGO HACER LA VALIDACIÓN APARTE: ROC, AUC, OTRAS MÉTRICAS.
# TODO: SOLUCIONAR VALIDACION

# Ahora del modelo necesito Validar cada uno de los modelos usando el ROC,
# El akeike del modelo,

## Export model  

name = model_name(glmulti_model, prefix = "glmulti_model")

file_name = paste0(name,".rds")

# Save model

saveRDS(glmulti_model,file.path("model_outputs", file_name))



#register_model <- function(model_name, registry_file=file.path("model_outputs","model_registry.csv")){

register_model(model_name = name)


# WGLR

# Fijarlos parametros de WGLR.








