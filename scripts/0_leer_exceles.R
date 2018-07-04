# En este script se leen los datos de los Exceles con la información sobre
# monitoreos de arrecifes. Se tiene cuidado en leerlos apropiadamente y no
# introducir NA's.

# Este script está diseñado para los datos v3, por lo que se complementará con
# las columnas que faltan a partir de las tablas simplificadas
# ("_tablas_individuales")

# También se leerán los catálogos ("_catalogos") para realizar la revisión de los
# registros en los campos correspondientes a éstos, y también integrarlos a la base.

# Cargando archivo de configuración y funciones auxiliares:
source("config.R")
source("funciones_auxiliares.R")

# Poniendo las opciones para las warnings
options(nwarnings=50000)

################################################################################
# Leyendo datos de los exceles
################################################################################

lista_tablas_crudas_exceles_conacyt_greenpeace_2016 <- leer_exceles(ruta_carpeta_conacyt_greenpeace_2016, 2)
saveRDS(lista_tablas_crudas_exceles_conacyt_greenpeace_2016,
  paste0(ruta_salidas_0_leer_exceles, "/lista_tablas_crudas_exceles_conacyt_greenpeace_2016.RData"))

lista_tablas_crudas_exceles_historicos_y_2017 <- leer_exceles(ruta_carpeta_historicos_y_2017, 1)
saveRDS(lista_tablas_crudas_exceles_historicos_y_2017,
  paste0(ruta_salidas_0_leer_exceles, "/lista_tablas_crudas_exceles_historicos_y_2017.RData"))

lista_catalogos <- leer_exceles(ruta_carpeta_catalogos, 1) %>%
  renombra_columnas_minusculas()
saveRDS(lista_catalogos, paste0(ruta_salidas_0_leer_exceles, "/lista_catalogos.RData"))

################################################################################
# Leyendo archivos con datos adicionales
################################################################################

tabla_campos_adicionales_conacyt_greenpeace_2016 <- ruta_carpeta_conacyt_greenpeace_2016 %>%
  paste0(subruta_tabla_campos_adicionales_sitio) %>%
  read_excel() %>%
  mutate(
    nombre_sitio = estandariza_strings(nombre_sitio)
  )
saveRDS(tabla_campos_adicionales_conacyt_greenpeace_2016,
  paste0(ruta_salidas_0_leer_exceles, "/tabla_campos_adicionales_conacyt_greenpeace_2016.RData"))

tabla_campos_adicionales_historicos_y_2017 <- ruta_carpeta_historicos_y_2017 %>%
  paste0(subruta_tabla_campos_adicionales_sitio) %>%
  read_excel() %>%
  mutate(
    nombre_sitio = estandariza_strings(nombre_sitio)
  )
saveRDS(tabla_campos_adicionales_historicos_y_2017,
  paste0(ruta_salidas_0_leer_exceles, "/tabla_campos_adicionales_historicos_y_2017.RData"))

