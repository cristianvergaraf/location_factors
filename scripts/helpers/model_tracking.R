

register_model <- function(model_name, registry_file=file.path("model_outputs","model_registry.csv")){
    
    
    # Create folder if does not exist
    
    dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
    
    # Create initial register if does not exist
    if (!file.exists(registry_file)){
        write.csv(
            data.frame(model_name = character(), created_at = character()),
                  stringsAsFactors = FALSE,
            registry_file, row.names = FALSE)
        
    }
    
    # Create new row
    new_entry <- data.frame(
        model_name = model_name,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
        
    
    )
    
    
    # Save
    
    write.table(new_entry, registry_file, sep = ",",
                row.names = FALSE, col.names = FALSE, append = TRUE)

    # Print message
    
    message("Modelo registrado en: ", normalizePath(registry_file, winslash = "/"))
}
    
