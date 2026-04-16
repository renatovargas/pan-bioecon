source("scripts/funciones.R")

#----------------------------------
# Rutas
#----------------------------------

ruta_config <- "data/config/pan_config.xlsx"
ruta_referencia <- "data/config/pan_v02_lookups.xlsx"

output_rds <- "outputs/pan-bioeconomia.RDS"
output_xlsx <- "outputs/pan-bioeconomia.xlsx"

#----------------------------------
# Ejecutar
#----------------------------------

df <- procesar_todo(
  ruta_config = ruta_config,
  ruta_referencia = ruta_referencia,
  data_dir = "data/cou"
)

#----------------------------------
# Guardar
#----------------------------------

saveRDS(df, output_rds)

write.xlsx(df, output_xlsx, overwrite = TRUE)

cat("✔ Procesamiento completo.\n")
cat("Filas:", nrow(df), "\n")
