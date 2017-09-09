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

ruta_exceles <- "~/Dropbox/carpetas_compartidas/barco_lab/BASES_INTEGRACION/conacyt_greenpeace/respaldos/datos_v3_preeliminar"

nombres_archivos <- list.files(ruta_exceles,
  full.names = TRUE,
  pattern = "^[[:alpha:]].*")

# Leyendo cada archivo de Excel, que contiene información de cada aspecto:
lista_exceles_cruda <- llply(nombres_archivos, function(x){
  # Primero se leerá cada archivo, no importando warnings, con el fin de saber
  # el número de columnas de cada uno y poder especificar que todas sean leídas
  # como texto (y así evitar warnings la segunda vez que se lean)
  aux <- read_excel(x, sheet = 2)
  print(ncol(aux))
  datos <- read_excel(x, sheet = 2, col_types = rep("text", ncol(aux))) %>%
    # Se revisaron los datos, y todos los renglones no vacíos tienen el siguiente campo
    filter(!is.na(Serie)) %>%
    # Agregando el archivo origen a los datos correspondientes
    mutate(
      archivo_origen = (basename(x) %>%
          stri_match_first_regex("(\\w+).xlsx"))[,2]
    )
  return(datos)
})

names(lista_exceles_cruda) <- (basename(nombres_archivos) %>%
    stri_match_first_regex("(\\w+).xlsx"))[,2]

# Escribiendo Warnings para tener el control sobre ellos.
sink("../productos/v3/warnings_lectura_archivos_excel.txt")
warnings()
sink()
# Creo que no hay problemas porque al final todo lo transformo en caracter.

# Revisando que los exceles se hayan leído correctamente:
llply(lista_exceles_cruda, function(x){
  nombres_columnas <- colnames(x) %>%
    sort()
  return(nombres_columnas)
})
# Perfecto! No hay nombres aproximadamente duplicados en las columnas dentro de
# cada Excel.

num_registros <- read_excel(paste0(ruta_exceles, "/_numero_registros_excel_v3.xlsx"))
View(num_registros)
llply(lista_exceles_cruda, nrow)
# Perfecto!

# Ahora se procederá a realizar el join de cada una de las tablas en "lista_exceles_cruda"
# con la tabla de campos adicionales de cada muestreo de sitio, con el fin de
# homologar el formato y reutilizar los scripts.

# Leyendo la tabla de campos adicionales por sitio:
tabla_campos_adicionales_sitio <- read_excel(paste0(ruta_exceles, "/_tablas_individuales/campos_adicionales_sitio.xlsx"))

# Revisando los nombres de las columnas de cada data frame en "lista_exceles_cruda".
# nos interesa el campo de "Nombre_del_sitio", pues sobre éste se realizará el join:
l_ply(lista_exceles_cruda, glimpse)

# Generando joins con las tablas
lista_datos_crudos <- lista_exceles_cruda %>%
  renombra_columna("Nombre_del_Sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_sitio", "nombre_sitio") %>%
  llply(function(x){
    x %>%
      mutate(
        nombre_sitio = estandariza_strings(nombre_sitio)
        )
  }) %>%
  inner_join_lista(
    tabla_campos_adicionales_sitio %>%
      mutate(nombre_sitio = estandariza_strings(nombre_sitio)),
    "nombre_sitio"
  )
# Como no sacó ningún warning el inner_join_lista, los tamaños de todos los data
# frames se quedaron igual (no hubo artefactos).

# Escribiendo los data frames leídos en un objeto
#saveRDS(lista_datos_crudos, "../productos/v3/lista_datos_crudos.RData")

# Finalmente se leerán los catálogos (falta que Esme tenga las versiones finales)
