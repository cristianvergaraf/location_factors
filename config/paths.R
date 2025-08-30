# Root path of the project
root_dir <- here::here()  # Usando el paquete "here" para evitar setwd()
# Set working directory


# Subcarpetas


patches_data_dir <- file.path(root_dir, "data/processed/raster/patches")
model_data_dir    <- file.path(root_dir, "data/processed/csv")
scripts_dir <- file.path(root_dir, "scripts")
models_dir <- file.path(root_dir, "scripts/models")

# Archivos clave


# Archivos clave

extract_data_from_raster_file <- file.path(model_data_dir, "extract_data_from_raster.csv")
gan_patches_8715_file <- file.path(patches_data_dir, "ras_gan_patches_8715_exp87.tif")
training_data_file <- file.path(model_data_dir, "extract_data_from_raster.csv")


