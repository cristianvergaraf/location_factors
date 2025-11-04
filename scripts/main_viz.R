source("config/load_packages.R")
source("scripts/models/model_assessment.R")
source("scripts/viz/viz_helper.R")
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

spatial_col_names = c("fom","spatial_auc","toc_auc")

col_names = c("training_auc","test_auc")


data_wide_clean <- dplyr::select(results_models, -any_of(exclude))

df_long <- prepare_df_wide_long(data_wide_clean,spatial_col_names)

df_long

generate_auc_facet_plot(df_long)

# Export the plots.

a <- generate_fom_plot(data_wide_clean)

b <- generate_fom_plot(data_wide_clean)

combine_model_plots(a,b)

create_model_performance_plots(data_wide_clean)

