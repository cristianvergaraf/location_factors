

model_name <- function(model, prefix = "model") {
    
    # Create filename with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    model_name <- paste0(prefix, "_", timestamp)
    
    # Full path
    return (model_name)
  
}
