#### load package

# setwd("~/github/location_factors")

source("config/load_packages.R")

source("scripts/models/relative_importance_helper.R")

source("scripts/helpers/model_tracking.R")

## Set working directory

source("config/paths.R")

source("scripts/models/model_assessment.R")

# Load models

glmulti_models <- readRDS("~/github/location_factors/model_outputs/glmulti_model_all_20250926_192822.rds")

glmulti_models

# Model average all models

avg_lingue8715 <- coef.glmulti(glmulti_models, select="all", varweighting ="Buckland", icmethod ="Lukacs")

df_avg_lingue8715 <-transform_df(avg_lingue8715)

df_avg_lingue8715[1:10,]

top <- top_N_models(glmulti_models,120)

relative_importance_plot(transform_df(top))

### No hay diferencia entre la importancia relativa entre uno u otro

relative_importance_plot(df_avg_lingue8715[1:10,])

# Select models depending on selection of different metrics and then average and get the 

## Una funcion para 

glmulti_models@objects[c(1,4,5)]

relative_importance_plot(df_300)

# Ahora debemos replicar el cálculo de la importancia relativa a las otras métricas, 
# a lo menos el FOM, y AUC spatial, quizás al TOC.

### NECESITO LEER LOS RESULTADOS ## 

source("config/load_packages.R")
source("scripts/models/model_assessment.R")
source("scripts/viz/viz_helper.R")
source("config/paths.R")


results_models <- read.csv(model_results_all)

results_models

View(results_models)

###################################



weights_fom <- compute_weights(results_models$fom, maximize = TRUE)


results_models$weight_fom <- weights_fom

vars <- unique(unlist(results_models$variables))

importance <- setNames(rep(0, length(vars)), vars)

importance


importance_fom <- compute_variable_importance(results_models$variables, results_models$weight_fom)

importance_fom


results$weight_fom <- compute_weights(results$fom, maximize = TRUE)

results$variables

importance_auc <- compute_variable_importance(results$variables, results$weight_auc)

importance_fom <- compute_variable_importance(results$variables, results$weight_fom)





