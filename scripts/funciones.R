library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

#-----------------------------
# Helpers
#-----------------------------

parse_indices <- function(x) {
  if (is.na(x) || x == "") {
    return(integer(0))
  }

  as.integer(str_split(x, ",")[[1]] %>% str_trim())
}

leer_tabla_referencia <- function(path, sheet) {
  read_excel(path, sheet = sheet)
}

#-----------------------------
# Core: leer un cuadrante
#-----------------------------

procesar_cuadrante_simple <- function(
  cou_path,
  sheet_name,
  cell_range,
  excl_rows,
  excl_cols,
  filas_ref,
  columnas_ref,
  meta
) {
  raw <- read_excel(
    cou_path,
    sheet = sheet_name,
    range = cell_range,
    col_names = FALSE
  )

  # excluir filas/columnas
  if (length(excl_rows) > 0) {
    raw <- raw[-excl_rows, ]
  }

  if (length(excl_cols) > 0) {
    raw <- raw[, -excl_cols]
  }

  # asignar ids de fila
  raw$row_id <- filas_ref$row_id[seq_len(nrow(raw))]

  df_long <- raw %>%
    pivot_longer(
      -row_id,
      names_to = "col_index",
      values_to = "value"
    ) %>%
    mutate(
      col_index = as.integer(str_remove(col_index, "...")),
      col_id = columnas_ref$col_id[col_index]
    ) %>%
    select(row_id, col_id, value) %>%
    mutate(
      iso3 = meta$iso3,
      year = meta$year,
      cuadrante_codigo = meta$cuadrante_codigo,
      cuadrante = meta$cuadrante,
      tabla_destino = meta$tabla_destino
    )

  df_long
}

#-----------------------------
# Master: procesar todo
#-----------------------------

procesar_todo <- function(
  ruta_config,
  ruta_referencia,
  data_dir = "data/cou"
) {
  config <- read_excel(ruta_config, sheet = "config")
  cuadrantes_ref <- read_excel(ruta_referencia, sheet = "quadrants")

  filas_ref <- leer_tabla_referencia(ruta_referencia, "rows")
  columnas_ref <- leer_tabla_referencia(ruta_referencia, "columns")

  resultados <- map_dfr(seq_len(nrow(config)), function(i) {
    cfg <- config[i, ]

    q <- cuadrantes_ref %>%
      filter(quadrant_code == cfg$quadrant_code)

    meta <- list(
      iso3 = cfg$iso3,
      year = cfg$year,
      cuadrante_codigo = cfg$quadrant_code,
      cuadrante = cfg$quadrant,
      tabla_destino = q$target_table
    )

    procesar_cuadrante_simple(
      cou_path = file.path(data_dir, cfg$file_name),
      sheet_name = cfg$sheet_name,
      cell_range = q$cell_range,
      excl_rows = parse_indices(q$excl_rows),
      excl_cols = parse_indices(q$excl_cols),
      filas_ref = filas_ref,
      columnas_ref = columnas_ref,
      meta = meta
    )
  })

  #-----------------------------
  # Dar significado a filas y columnas
  #-----------------------------

  resultados <- resultados %>%
    left_join(columnas_ref, by = "col_id") %>%
    left_join(filas_ref, by = "row_id")

  resultados
}
