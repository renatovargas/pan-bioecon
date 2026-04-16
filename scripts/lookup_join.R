library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)

#----------------------------------
# Paths
#----------------------------------

archivo_entrada <- "data/config/pan_v02_lookups.xlsx"
archivo_salida <- "outputs/lookup_join.xlsx"

#----------------------------------
# Leer tablas
#----------------------------------

rows <- read_excel(archivo_entrada, sheet = "rows") %>%
  mutate(Filas = str_trim(Filas))

filas <- read_excel(archivo_entrada, sheet = "filas") %>%
  mutate(Filas = str_trim(Filas))

columns <- read_excel(archivo_entrada, sheet = "columns") %>%
  mutate(Columnas = str_trim(Columnas))

columnas <- read_excel(archivo_entrada, sheet = "columnas") %>%
  mutate(Columnas = str_trim(Columnas))

#----------------------------------
# Uniones
#----------------------------------

rows_join <- rows %>%
  left_join(filas, by = "Filas")

columns_join <- columns %>%
  left_join(columnas, by = "Columnas")

#----------------------------------
# Exportar a Excel
#----------------------------------

wb <- createWorkbook()

addWorksheet(wb, "rows")
writeData(wb, "rows", rows_join)

addWorksheet(wb, "columns")
writeData(wb, "columns", columns_join)

saveWorkbook(wb, archivo_salida, overwrite = TRUE)

cat("lookup_join.xlsx creado en:", archivo_salida, "\n")
