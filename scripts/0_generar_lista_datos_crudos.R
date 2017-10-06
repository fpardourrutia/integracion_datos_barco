# En este script se leen los datos de los Exceles con la información sobre
# monitoreos de arrecifes. Se tiene cuidado en leerlos apropiadamente y no
# introducir NA's.

# Este script está diseñado para los datos v3, por lo que se complementará con
# las columnas que faltan a partir de las tablas simplificadas ("_tablas_individuales")

# También se leerán los catálogos ("_catalogos") para realizar la revisión de los
# registros en los campos correspondientes a éstos, y también integrarlos a la base.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("readxl")
library("lubridate")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

# Poniendo las opciones para las warnings
options(nwarnings=50000)

# Ruta de la carpeta con los datos V3:
ruta_carpeta_datos_v3 <- "~/Dropbox/carpetas_compartidas/barco_lab/bases_integracion/datos_v3_preeliminar"

##########################################################
# Procesando datos del proyecto CONACyT / GreenPeace 2016
##########################################################

ruta_carpeta_conacyt_greenpeace_2016 <- paste0(ruta_carpeta_datos_v3, "/conacyt_greenpeace_2016")
lista_exceles_conacyt_greenpeace_2016 <- leer_exceles(ruta_carpeta_conacyt_greenpeace_2016, 2)

# Revisando números de registros:
ruta_carpeta_conacyt_greenpeace_2016 %>%
  paste0("/_auxiliares/numero_registros.xlsx") %>%
  read_excel() %>%
  View()
# Perfecto!

# # Revisando que los exceles estén en el formato correcto:
# llply(lista_exceles_conacyt_greenpeace_2016, function(x){
#   nombres_columnas <- colnames(x) %>%
#     sort()
#   return(nombres_columnas)
# })
# # Perfecto! No hay nombres aproximadamente duplicados en las columnas dentro de
# # cada Excel.

# Ahora se procederá a realizar el join de cada una de las tablas en "lista_exceles_conacyt_greenpeace_2016"
# con la tabla de campos adicionales de cada muestreo de sitio, con el fin de
# homologar el formato y reutilizar los scripts.

# Leyendo la tabla de campos adicionales por sitio:
tabla_campos_adicionales_sitio_conacyt_greenpeace_2016 <- ruta_carpeta_conacyt_greenpeace_2016 %>%
  paste0("/_tablas_individuales/campos_adicionales_sitio.xlsx") %>%
  read_excel()

# Revisando los nombres de las columnas de cada data frame en
# "lista_exceles_conacyt_greenpeace_2016" nos interesa el campo de
# "Nombre_del_sitio", pues sobre éste se realizará el join:
# l_ply(lista_exceles_conacyt_greenpeace_2016, glimpse)

# Generando joins con las tablas
lista_datos_crudos_conacyt_greenpeace_2016 <- lista_exceles_conacyt_greenpeace_2016 %>%
  renombra_columna("Nombre_del_Sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_sitio", "nombre_sitio") %>%
  llply(function(df){
    df %>%
      mutate(
        nombre_sitio = estandariza_strings(nombre_sitio)
        )
  }) %>%
  inner_join_lista(
    tabla_campos_adicionales_sitio_conacyt_greenpeace_2016 %>%
      mutate(nombre_sitio = estandariza_strings(nombre_sitio)),
    "nombre_sitio"
  )
# Como no sacó ningún warning el inner_join_lista, los tamaños de todos los data
# frames se quedaron igual (no hubo artefactos).

#saveRDS(lista_datos_crudos_conacyt_greenpeace_2016, "../productos/v3/lista_datos_crudos_conacyt_greenpeace_2016.RData")

#####################################################
# Leyendo catálogos:
#####################################################

# Finalmente se leerán los catálogos (falta que Esme tenga las versiones finales)
ruta_catalogos <- paste0(ruta_carpeta_datos_v3, "/catalogos")
lista_catalogos <- leer_exceles(ruta_catalogos, 1)

#saveRDS(lista_catalogos, "../productos/v3/lista_catalogos.RData")


