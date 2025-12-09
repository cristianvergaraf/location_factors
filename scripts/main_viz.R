source("config/load_packages.R")
source("scripts/models/model_assessment.R")
source("scripts/viz/viz_helper.R")
source("config/paths.R")
source("config/load_packages.R")
source("scripts/models/relative_importance_helper.R")
source("scripts/helpers/model_tracking.R")
## Set working directory
source("config/paths.R")
source("scripts/models/model_assessment.R")

## Functions to load aic

select_top_rows_aic <- function(df, column, n) {
    
    # 1. Ordenar el dataframe de mayor a menor según la columna
    df_sorted <- df[order(df[[column]], decreasing = FALSE), ]
    
    # 2. Seleccionar las primeras n filas
    df_top <- head(df_sorted, n)
    
    return(df_top)
}


select_top_rows <- function(df, column, n) {
    
    # 1. Ordenar el dataframe de mayor a menor según la columna
    df_sorted <- df[order(df[[column]], decreasing = TRUE), ]
    
    # 2. Seleccionar las primeras n filas
    df_top <- head(df_sorted, n)
    
    return(df_top)
}

# Load data results

results_models <- read.csv(model_results_all)

best_300_aic <- select_top_rows_aic(results_models, "aic", 300)
best_100_aic <- select_top_rows_aic(results_models, "aic", 100)
best_10_aic <- select_top_rows_aic(results_models, "aic", 10)


#############

best_300_fom <- select_top_rows(results_models, "fom", 300)
best_100_fom <- select_top_rows(results_models, "fom", 100)
best_10_fom <- select_top_rows(results_models, "fom", 10)


############

best_300_spatial_auc <- select_top_rows(results_models, "spatial_auc", 300)
best_100_spatial_auc <- select_top_rows(results_models, "spatial_auc", 100)


# Crear una funcion que ordene y luego seleccione los mejores X modelos 
# De un dataframe

class(results_models)

# Make the plot

select_top_rows_aic <- function(df, column, n) {
    
    # 1. Ordenar el dataframe de mayor a menor según la columna
    df_sorted <- df[order(df[[column]], decreasing = FALSE), ]
    
    # 2. Seleccionar las primeras n filas
    df_top <- head(df_sorted, n)
    
    return(df_top)
}


select_top_rows <- function(df, column, n) {
    
    # 1. Ordenar el dataframe de mayor a menor según la columna
    df_sorted <- df[order(df[[column]], decreasing = TRUE), ]
    
    # 2. Seleccionar las primeras n filas
    df_top <- head(df_sorted, n)
    
    return(df_top)
}


colnames(results_models)

col_names = c("fom","aic","null_aic","delta_aic","null_deviance","residual_dev","dev_explained","MCFaddenPseudoR2",
"spatial_auc","training_auc","test_auc","toc_auc")

exclude = c("variables","null_aic","delta_aic","null_deviance","residual_dev","dev_explained","MCFaddenPseudoR2","toc_auc")

# Variables to exclude
#col_names = "variables"

spatial_col_names = c("fom","spatial_auc")

col_names = c("training_auc","test_auc")


data_wide_clean_aic <- dplyr::select(results_models, -any_of(exclude))
data_wide_clean_300_aic <- dplyr::select(best_300_aic, -any_of(exclude))
data_wide_clean_100_aic <- dplyr::select(best_100_aic, -any_of(exclude))
data_wide_clean_10_aic <- dplyr::select(best_10_aic, -any_of(exclude))


data_wide_clean_300_fom <- dplyr::select(best_300_fom, -any_of(exclude))
data_wide_clean_300_spatial_auc <- dplyr::select(best_300_spatial_auc, -any_of(exclude))


m <- dplyr::select(best_100_fom, -any_of(exclude))
data_wide_clean_100_spatial_auc <- dplyr::select(best_100_spatial_auc, -any_of(exclude))

length(data_wide_clean_aic$fom)
length(data_wide_clean_300_aic$fom)

df_long_aic <- prepare_df_wide_long(data_wide_clean_aic,spatial_col_names)
df_long_300_aic <- prepare_df_wide_long(data_wide_clean_300_aic,spatial_col_names)
df_long_100_aic <- prepare_df_wide_long(data_wide_clean_100_aic,spatial_col_names)
df_long_10_aic <- prepare_df_wide_long(data_wide_clean_10_aic,spatial_col_names)

df_long_300_fom <- prepare_df_wide_long(data_wide_clean_300_fom,spatial_col_names)
df_long_100_fom <- prepare_df_wide_long(data_wide_clean_100_fom,spatial_col_names)


df_long_300_spatial_auc <- prepare_df_wide_long(data_wide_clean_300_spatial_auc,spatial_col_names)
df_long_100_spatial_auc <- prepare_df_wide_long(data_wide_clean_100_spatial_auc,spatial_col_names)

data_wide_clean_300_spatial_auc

length(df_long_aic$model_id)
length(df_long_300_aic$model_id)
length(df_long_100_aic$model_id)

# Graph
generate_auc_facet_plot_r2(df_long_aic)
generate_auc_facet_plot_r2(df_long_300_aic)
generate_auc_facet_plot(df_long_100_aic)
generate_auc_facet_plot_r2(df_long_100_aic)
generate_auc_facet_plot_r2(df_long_10_aic)



generate

df_long_100_aic

generate_auc_facet_plot_r2(df_long_300_fom)
generate_auc_facet_plot_r2(df_long_100_fom)

generate_auc_facet_plot_r2(df_long_300_spatial_auc)
generate_auc_facet_plot_r2(df_long_100_spatial_auc)


# Export the plots.

a <- generate_fom_plot(data_wide_clean)
b <- generate_fom_plot(data_wide_clean)

combine_model_plots(a,b)

create_model_performance_plots

#### Crear un plot de los mejores 300 mejores modelos

