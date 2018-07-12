# En este script se revisan los catálogos. En particular se desea que no haya
# registros duplicados o con ambigüedad.
# Pasos:
# 1. Se lee la lista de catálogos.
# 2. Se revisan los catálogos que lo ameriten

# Cargando archivo de configuración y funciones auxiliares
library("readr")
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo lista de catálogos
################################################################################

lista_catalogos <- readRDS(
  paste0(rutas_salida[1], "/lista_catalogos.RData"))

################################################################################
# 2. Realizando revisiones en catálogos
################################################################################

# Revisando cuántos catálogos tienen cada columna, para ver si hay que renombrar
# o no.

crear_resumen_columnas_df(lista_catalogos) %>%
  select(-nombre_df) %>%
  gather("variable", "valor") %>%
  group_by(variable) %>%
  summarise(
    n = sum(valor)
  ) %>%
  arrange(variable) %>%
  View()
# Parece que todo bien
