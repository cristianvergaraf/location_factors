
# --- 1. Load Required Libraries ---
# Note: You must run install.packages("tidyr"), install.packages("patchwork") 
# and install.packages("ggplot2") if you haven't already.
library(ggplot2)
library(tidyr)
library(patchwork)

# ====================================================================
# MODULAR HELPER FUNCTIONS
# ====================================================================

#' @title Reshapes AUC Data from Wide to Long Format
#'
#' @description
#' Pivots the specified AUC columns into a long format suitable for faceting
#' in ggplot2.
#'
#' @param df The data frame in wide format.
#' @param auc_cols A character vector of column names to pivot (e.g., training_auc).
#'
#' @return A long-format data frame with new columns 'Metric' and 'AUC_Value'.
prepare_df_wide_long <- function(df, metrics_cols) {
    df_long <- df %>%
        tidyr::pivot_longer(
            cols = all_of(metrics_cols),
            names_to = "Metric",
            values_to = "Metric_value"
        )
    return(df_long)
}


#' @title Generates the Scatterplot for FOM vs. AIC
#'
#' @description
#' Creates the top panel of the combined visualization, showing the relationship
#' between the Figure of Merit (fom) and AIC.
#'
#' @param df The data frame containing the 'fom' and 'aic' columns.
#'
#' @return A ggplot object (p_fom).
generate_fom_plot <- function(df) {
    # NOTE: Corrected 'AIC' to 'aic' to match the dataframe column case
    p_fom <- ggplot(df, aes(x = fom, y = aic)) +
        geom_point(alpha = 0.7, color = "#1f77b4") + # Blue points
        geom_smooth(method = "loess", se = TRUE, color = "#ff7f0e", linetype = "solid") + # Orange trend line
        labs(
            title = "A. Figure of Merit (FOM) vs. AIC",
            x = "FOM Value",
            y = "AIC (Akaike Information Criterion)"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
    return(p_fom)
}

#' @title Generates the Faceted Scatterplots for AUC Metrics vs. AIC
#'
#' @description
#' Creates the bottom panel, displaying four separate scatterplots comparing
#' different AUC metrics against AIC.
#'
#' @param df_long The long-format data frame from 'prepare_df_wide_long'.
#'
#' @return A ggplot object (p_auc).
generate_auc_facet_plot <- function(df_long) {
    # NOTE: Corrected 'AIC' to 'aic' to match the dataframe column case
    p_auc <- ggplot(df_long, aes(x = aic, y = Metric_value)) +
        geom_point(alpha = 0.6, color = "gray50") +
        geom_smooth(method = "lm", se = FALSE, color = "#2ca02c", linetype = "dashed") + # Green linear trend
        # Use ncol=2 to arrange the 4 plots in a 2x2 grid
        facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
        labs(
            title = "B. AUC vs AIC",
            x = "AIC (Akaike Information Criterion)",
            y = "AUC Value"
        ) +
        # ✅ Add custom axis breaks
        scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10) # show ~5 evenly spaced ticks
        ) +
        scale_y_continuous(
            breaks = scales::pretty_breaks(n = 5)
        ) +
        theme_minimal(base_size = 14) +
        theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            strip.background = element_rect(fill = "gray95", color = NA) # Style facet labels
        )
    
    return(p_auc)
}


#' @title Generates the Faceted Scatterplots for AUC Metrics vs. AIC
#'
#' @description
#' Creates the bottom panel, displaying four separate scatterplots comparing
#' different AUC metrics against AIC.
#'
#' @param df_long The long-format data frame from 'prepare_df_wide_long'.
#'
#' @return A ggplot object (p_auc).
generate_auc_facet_plot_general <- function(df_long, x, y,title,x_axis,y_axis) {
    # NOTE: Corrected 'AIC' to 'aic' to match the dataframe column case
    p_auc <- ggplot(df_long, aes(x = x, y = y)) +
        geom_point(alpha = 0.6, color = "gray50") +
        geom_smooth(method = "lm", se = FALSE, color = "#2ca02c", linetype = "dashed") + # Green linear trend
        # Use ncol=2 to arrange the 4 plots in a 2x2 grid
        facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
        labs(
            title = title,
            x = x_axis,
            y = y_axis
        ) +
        theme_minimal(base_size = 14) +
        theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            strip.background = element_rect(fill = "gray95", color = NA) # Style facet labels
        )
    
    return(p_auc)
}


#' @title Combines the FOM/AIC plot and the Faceted AUC/AIC plot
#'
#' @description
#' Uses patchwork to stack the two main plots, giving the faceted AUC plot 
#' more vertical space (2/3 height) than the FOM plot (1/3 height).
#'
#' @param p_fom The ggplot object for FOM vs. AIC.
#' @param p_auc The ggplot object for the Faceted AUCs vs. AIC.
#'
#' @return A 'patchwork' object (a combined ggplot).
combine_model_plots <- function(p_fom, p_auc) {
    # Stack p_fom on top of p_auc and give the faceted plot more vertical space
    combined_plot <- (p_fom / p_auc) +
        patchwork::plot_layout(heights = c(1, 2)) # 1/3 of space for FOM, 2/3 for AUCs
    
    return(combined_plot)
}

# ====================================================================
# MAIN FUNCTION (Orchestrator)
# ====================================================================

#' @title Creates a Combined Multi-Panel Model Performance Plot
#'
#' @description
#' Generates a multi-panel plot combining the scatterplot of FOM vs. AIC 
#' (top panel) and a faceted view of four different AUC metrics vs. AIC 
#' (bottom panel).
#'
#' @param df A data frame containing the model metrics. Must include columns:
#'   'aic', 'fom', 'training_auc', 'test_auc', 'toc_auc', 'spatial_auc'.
#'
#' @return A 'patchwork' object (a combined ggplot) ready for printing.
create_model_performance_plots <- function(df) {
    
    # Define the AUC columns for reshaping
    auc_cols <- c("training_auc", "test_auc", "toc_auc", "spatial_auc")
    
    # 1. Prepare Data
    df_long <- prepare_df_wide_long(df, auc_cols)
    
    # 2. Generate Plot Components
    p_fom <- generate_fom_plot(df)
    p_auc <- generate_auc_facet_plot(df_long)
    
    # 3. Combine Plots
    combined_plot <- combine_model_plots(p_fom, p_auc)
    
    return(combined_plot)
}

relative_importance_plot <- function(df){
    return(ggplot(df_imp, aes(x = Variable, y = Importance)) +
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















