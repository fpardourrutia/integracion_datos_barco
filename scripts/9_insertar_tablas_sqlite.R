# En este script se integran las tablas, vistas y catálogos creados con anterioridad
# a una base de datos SQLite.

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")
library("RSQLite")

################################################################################
# Leyendo listas de tablas, catálogos y vistas
################################################################################

lista_tablas_bd <- readRDS(paste0(rutas_salida[7], "/lista_tablas_bd.RData"))
lista_catalogos_bd <- readRDS(paste0(rutas_salida[7], "/lista_catalogos_bd.RData"))
lista_vistas_bd <- readRDS(paste0(rutas_salida[8], "/lista_vistas_bd.RData"))

names(lista_tablas_bd)
names(lista_catalogos_bd)
names(lista_vistas_bd)

################################################################################
# Insertando las tablas en la base de datos
################################################################################

# Creando la conexión a la base de datos
base_output <- dbConnect(RSQLite::SQLite(), paste0(rutas_salida[8], "/barco_db.sqlite"))

# Insertando catálogos:
l_ply(1:length(lista_catalogos_bd), function(i){
  dbWriteTable(base_output, names(lista_catalogos_bd)[i], lista_catalogos_bd[[i]])
})

# Insertando_tablas:
l_ply(1:length(lista_tablas_bd), function(i){
  dbWriteTable(base_output, names(lista_tablas_bd)[i], lista_tablas_bd[[i]])
})

# Insertando_vistas:
l_ply(1:length(lista_vistas_bd), function(i){
  dbWriteTable(base_output, names(lista_vistas_bd)[i], lista_vistas_bd[[i]])
})

# Desconectándonos de la base de datos d
dbDisconnect(base_output)