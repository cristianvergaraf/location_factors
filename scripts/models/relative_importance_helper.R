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


generate_auc_facet_plot_r2 <- function(df) {
    
    # ---- 1. Compute R² for each metric (UNCHANGED) ----
    r2_table <- df %>%
        group_by(Metric) %>%
        # Calculate R-squared using the linear model: Metric_value ~ aic
        summarize(r2 = summary(lm(Metric_value ~ aic))$r.squared) %>%
        # Create the label string
        mutate(label = paste0("R² = ", round(r2, 3)))
    
    # ---- 2. Choose positions for the text (Slightly adjusted for better placement) ----
    # Compute max x and max y per facet for text positioning
    label_positions <- df %>%
        group_by(Metric) %>%
        summarize(
            # X position for label (e.g., 98% of max x)
            x = max(aic, na.rm = TRUE) * 1,
            # Y position for label (e.g., 95% of max y)
            y = max(Metric_value, na.rm = TRUE) * 1
        )
    
    # Merge with R² labels
    r2_positions <- left_join(label_positions, r2_table, by = "Metric")
    
    
    # ---- 3. Build the plot (MODIFIED) ----
    p_auc <- ggplot(df, aes(x = aic, y = Metric_value)) +
        geom_point(alpha = 0.6, color = "gray50") +
        
        # ADDITION 1: Add the regression line (you already had this, but let's ensure it's clear)
        geom_smooth(
            method = "lm",
            se = FALSE,              # Don't show the standard error shading
            color = "#e377c2",       # Changed color for distinction
            linetype = "solid"       # Use a solid line for the fitted model
        ) +
        
        # ADDITION 2: Add the R² label to the top-right of each facet
        geom_text(
            data = r2_positions,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,     # Use the data from r2_positions, not df
            hjust = 1,               # Right-align the text to the computed x position
            vjust = 1,               # Top-align the text to the computed y position
            size = 4,                # Adjust text size as needed
            color = "#d62728"        # A distinct color for the label
        ) +
        
        facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
        labs(
            title = "B. AUC vs AIC",
            x = "AIC (Akaike Information Criterion)",
            y = "AUC Value"
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        theme_minimal(base_size = 14) +
        theme(
            plot.title = element_text(face = "bold")
            # You can add more theme elements here if needed
        )
    
    return(p_auc)
}





