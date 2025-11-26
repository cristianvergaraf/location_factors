relative_importance_plot <- function(df)
{
    return(ggplot(df, aes(x = Variable, y = Importance)) +
               geom_col(fill = "#2c7fb8") +
               coord_flip() +
               labs(
                   y = "Importancia"
               ) +
               theme_minimal(base_size = 14) +
               theme(
                   plot.title = element_text(face = "bold", hjust = 0.5),
                   axis.text.y = element_text(size = 12),
                   axis.text.x = element_text(size = 12)
               ))
    
    
}

top_N_models <- function(glmulti,n){
    topN_ids <- order(glmulti@crits)[1:n]
    
    avg_models <- coef.glmulti(
        glmulti,
        select = topN_ids,
        varweighting = "Buckland",
        icmethod = "Lukacs"
    )
    return(avg_models)
}

compute_weights <- function(metric_values, maximize = TRUE) {
    if (maximize) {
        # best = max metric
        delta <- max(metric_values) - metric_values
    } else {
        # best = min metric (like AIC)
        delta <- metric_values - min(metric_values)
    }
    
    likelihoods <- exp(-0.5 * delta)
    weights <- likelihoods / sum(likelihoods)
    
    return(weights)
}

compute_variable_importance <- function(variable_list, weights) {
    vars <- unique(unlist(variable_list))
    importance <- setNames(rep(0, length(vars)), vars)
    
    for (i in seq_along(variable_list)) {
        importance[variable_list[[i]]] <- importance[variable_list[[i]]] + weights[i]
    }
    
    return(sort(importance, decreasing = TRUE))
}

transform_df <- function(matrix){
    df <- data.frame(matrix)
    df$Variable = c(
        "dis_plant87",
        "dis_hidrology",
        "dis_communities",
        "native_forest",
        "dis_road_network",
        "shrubland",
        "agriculture",
        "property size",
        "slope < 15%",
        "dis_urban", 
        "Intercept"
    )
    
    df$Variable <- factor(df$Variable, levels = df$Variable[order(df$Importance)])
    
    return(df)
}
