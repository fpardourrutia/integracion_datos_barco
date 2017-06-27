# En este script se leen los datos de los Exceles con la información sobre
# monitoreos de arrecifes. Se tiene cuidado en leerlos apropiadamente y no
# introducir NA's.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("readxl")
library("lubridate")

# Poniendo las opciones para las warnings
options(nwarnings=50000)

ruta_exceles <- "~/Dropbox/carpetas_compartidas/barco_lab/BASES_INTEGRACION/conacyt_greenpeace/respaldos/datos_v2"

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
sink("../productos/warnings_lectura_archivos_excel.txt")
warnings()
sink()
# Expecting numeric .*
# Coercing text to numeric .*
# Expectig logical .*
# 26408 + 412 + 185 = 27005

# Revisando que los exceles se hayan leído correctamente:
llply(lista_exceles_cruda, function(x){
  nombres_columnas <- colnames(x) %>%
    sort()
  return(nombres_columnas)
})
# Perfecto! No hay nombres aproximadamente duplicados en las columnas dentro de
# cada Excel.

num_registros <- read_excel(paste0(ruta_exceles, "/_numero_registros_excel.xlsx"))
View(num_registros)
# Perfecto!

# Escribiendo los data frames leídos en un objeto
#saveRDS(lista_exceles_cruda, "../productos/lista_exceles_cruda.RData")