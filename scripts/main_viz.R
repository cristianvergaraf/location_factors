source("config/load_packages.R")
source("scripts/viz/viz_helper.R")
source("definitions/definitions.R")
source("config/paths.R")

# Load data results

results_models <- read.csv(model_results_all)

results_models

# Make the plot

colnames(results_models)

col_names = c("fom","aic","null_aic","delta_aic","null_deviance","residual_dev","dev_explained","MCFaddenPseudoR2",
"spatial_auc","training_auc","test_auc","toc_auc")

exclude = c("variables","null_aic","delta_aic","null_deviance","residual_dev","dev_explained","MCFaddenPseudoR2")

# Variables to exclude
col_names = "variables"

col_names = c("fom","spatial_auc","training_auc","test_auc","toc_auc")

data_wide_clean <- dplyr::select(results_models, -any_of(exclude))

df_long <- prepare_df_wide_long(data_wide_clean,col_names)

generate_auc_facet_plot(df_long)

# Export the plots.