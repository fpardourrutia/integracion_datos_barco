# En este script se revisan los catálogos. En particular se desea que no haya
# registros duplicados o con ambigüedad.
# Pasos:
# 1. Se lee la lista de catálogos.
# 2. Se revisa que las columnas de los catálogos estén homologadas.
# 3. Se revisa cada catálogo visualmente, con el fin de detectar a ojo posibles
# errores o catálogos que hay que revisar por otros métodos (por ejemplo, debido
# a su tamaño)
# 4. Revisión específica de cada catálogo.

# Cargando archivo de configuración y funciones auxiliares
library("readr")
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo lista de catálogos
################################################################################

lista_catalogos <- readRDS(paste0(rutas_salida[1], "/lista_catalogos.RData"))
names(lista_catalogos)

################################################################################
# 2. Revisando que las columnas en los catálogos estén homologadas
################################################################################

# Agregando catálogos y ordenando por columnas para revisar más fácilmente
# columnas aproximadamente duplicadas

tabla_revision <- Reduce(rbind.fill, lista_catalogos) %>%
  select(colnames(.) %>%
    sort() %>%
    noquote())

glimpse(tabla_revision)

# Revisando cuántos catálogos tienen cada columna, para ver si hay que renombrar
# o no.

crea_resumen_columnas_df(lista_catalogos) %>%
  select(-nombre_df) %>%
  gather("variable", "valor") %>%
  group_by(variable) %>%
  summarise(
    n = sum(valor)
  ) %>%
  arrange(variable) %>%
  View()
# Parece que todo bien

################################################################################
# 3. Revisión visual de cada catálogo
################################################################################

l_ply(1:length(lista_catalogos), function(i){
  print(names(lista_catalogos)[i])
  View(lista_catalogos[[i]])
  readline(prompt="Presionar [enter] para continuar")
})

################################################################################
# 4. Revisiones específicas de los catálogos que lo necesiten
################################################################################

# Por ahora, se revisarán duplicados en los catálogos que lo ameriten:

relacion_llaves_naturales_catalogo = list(
  "catalogos_muestra_reclutas_info__sustrato" = "codigo",
  "catalogos_registro_bentos__codigo" = "codigo",
  "catalogos_registro_corales__sobrecrecimiento" = "codigo",
  "catalogos_registro_invertebrados__tipo" = "tipo",
  "catalogos_registro_peces__nombre_cientifico_abreviado" = "nombre_cientifico_abreviado"
)

valores_duplicados_catalogos <- encuentra_duplicados(lista_catalogos,
  relacion_llaves_naturales_catalogo)

# Guardando data frames de duplicados en formato csv:
l_ply(1:length(valores_duplicados_catalogos), function(i){
  write_csv(valores_duplicados_catalogos[[i]],
    paste0(rutas_salida[3], "/valores_duplicados_",
      names(valores_duplicados_catalogos)[i], ".csv"))
})


