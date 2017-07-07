# En este script se generarán las tablas principales que se insertarán en la base
# de datos.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("readr")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

datos_globales <- readRDS("../productos/datos_globales.RData")
glimpse(datos_globales)

# Revisando valores de las columnas de datos_globales:
revision_valores <- revisa_valores(datos_globales)
names(revision_valores)

# Función para consultar el objeto anterior:
# nombre_columna: nombre de la columna a consultar.
# La función regresa la tabla correspondiente a esa columna
# El nombre de esta función es muy rápido para hacer la operación fácilmente
crv <- function(nombre_columna){
  return(revision_valores[[nombre_columna]])
}

datos_globales_llaves_primarias <- datos_globales %>%
  genera_llave("id_proyecto", "nombre_proyecto") %>%
  genera_llave("id_muestreo_sitio", "nombre_sitio", "fecha_hora_muestreo_sitio") %>%
  genera_llave("id_muestreo_transecto", "id_muestreo_sitio", "transecto")
#saveRDS(datos_globales_llaves_primarias, "../productos/datos_globales_llaves_primarias.RData")

##############################
# Generando la tabla "project"
##############################

lista_columnas_project <- list(
  name = "nombre_proyecto",
  description = "titulo",
  purpose = "tema",
  #location falta
  start_year = "anio_inicio_proyecto", # Cambios en esquema!
  end_year = "anio_termino_proyecto",  # Cambios en esquema!
  organization = "institucion",
  suborganization = "suborganizacion",
  person_in_charge = "autor_administrador_proyecto",
  contact = "contacto",
  site_selection_method = "metodo_seleccion_sitios",
  comments = "strings_vacios", # Cambios en el esquema"
  reference = "cita"
  
  # Faltan columnas del cliente de captura
)

project <- genera_tabla(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_proyecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_project
  ) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()

#################################
# Generando la tabla "site_sample"
#################################

lista_columnas_site_sample <- list(
  project_id = "id_proyecto",
  name = "nombre_sitio",
  datetime = "fecha_hora_muestreo_sitio", # Cambios en el esquema!
  # resampling: esta columna se eliminará por dificultad de mantenimiento
  country = "pais",
  healthy_reefs_region = "region_healthy_reefs",
  location = "localidad",
  
  # Falta definir (y depurar) las variables "tipo_arrecide, "zona_arrecifal",
  # "subzona_habitat", "nombre_arrecife".
  
  inside_protected_area = "dentro_anp",
  protected_area_name = "anp",  # Cambios en esquema!
  # Falta "dentro_area_no_pesca" (inside_non_fishing_area)
  latitude = "latitud",
  longitude = "longitud",
  datum = "datum",
  
  methodology = "protocolo_muestreo_sitio",
  # "methodology_details" se elimina, los detalles quedarán a nivel de aspecto
  # "...info" (ver notas)
  # "data_aggregation_level" pasará a las tablas de "...info" (ver notas).
  
  # Esme me calculará la profundidad por sitio
  depth_m = "profundidad_media_m", # Cambios en el esquema!
  temperature_c = "temperatura_c", # Cambios en el esquema!
  comments = "strings_vacios"
  
  # Faltan columnas del cliente de captura
)

site_sample <- genera_tabla(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_muestreo_sitio",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_site_sample
) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()


#####################################
# Generando la tabla "transect_sample"
#####################################
# Supuestos: en cada transecto se vio al menos una observación, sea la que sea:
# bentos: alguna de bentos, corales, reclutas, complejidad (invertebrados si aplica)
# peces: agún pez (o invertebrado), si aplica.

# Es importante tener completa la información de transectos, pero no por sí misma,
# más importante es tener completa la información de muestreos de cada aspecto
# realizados entre sitio (las tablas ...info)
# (información completa de aspectos => información completa de muestreos de transectos)

lista_columnas_transect_sample <- list(
  site_sample_id = "id_muestreo_sitio",
  name = "transecto"
)

transect_sample <- genera_tabla(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_muestreo_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_transect_sample
)

# Supuesto para generar las tablas de aspecto (bentos, peces, corales, reclutas,
# complejidad, invertebrados):
# 1. Cada Excel tiene información correspondiente a un sólo nivel espacial de
# agrupación de datos (transecto / sitio) .
# 2. Cada Excel tiene información correspondiente a un sólo nivel biológico de
# agragación de datos (por observacion, coberturas por especie, etc)...
# Esto es necesario para generar las tablas de "...info" (que tienen información)
# del nivel de agrupación de los datos en el nombre, y del nivel de agrupación
# biológica de los datos en el campo "...info.data_aggregation_level".
