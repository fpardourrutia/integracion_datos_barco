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
  )

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
  # (ver notas)
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
)

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

##########
# Benthos
##########
# Supuestos:
# 1. Cada muestreo de benthos en transecto realizado fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene sólo un muestreo de bentos asociado (por
# ejemplo, no es válido tener PIT y LIT sobre el mismo transecto)

datos_globales_bentos_llaves_primarias <- datos_globales_llaves_primarias %>%
  filter(archivo_origen == "BENTOS_DESAGREGADOS_V2") %>%
  elimina_columnas_vacias() %>%
  # Para que sea un poco más natural la llave primaria generada para
  # "id_punto_muestreo_bentos"
  arrange(serie) %>%
  genera_llave("id_muestreo_bentos_transecto", "id_muestreo_transecto") %>%
  genera_llave("id_punto_muestreo_bentos") # Que sea una simple cuenta en órden

###################################################
# Generando la tabla "Benthos_transect_sample_info
###################################################

lista_columnas_benthos_transect_sample_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  sampling_method = "metodo",
  data_aggregation_level = "nivel_agregacion_datos"
)

#############
# Catálogos
#############
# - Project.purpose (tema)
# - Project.site_selection_method (metodo_seleccion_sitios)
# - Site_sample.country (pais)
# - Site_sample.healthy_reefs_region (region_healthy_reefs)
# - Site_sample.location (localidad)
# - Site_sample.methodology (protocolo_muestreo_sitio)
# - de las variables elegidas entre Site_sample.reef_type (tipo_arrecife),
#   Site_sample.reef_zone (zona_arrecifal), Site_sample.subzone_habitat (subzona_habitat)
# - Site_sample.protected_area (anp)
# - Benthos_transect_sample_info.sampling_method (método en exceles de Bentos)
# - Benthos_transect_sample_info.data_aggregation_level (nivel_agregacion_datos
#   en exceles de Bentos)
# - 

#######################################
# Comentarios (a consultar con Lorenzo)
#######################################
# 1. Tal vez el método de selección de cada sitio se debe dar por sitio, porque
# así puede ser más preciso. Ésto depende si en un mismo proyecto pueden haber
# sitios seleccionados de más de una forma (estratégico, aleatorio, etc).
# Y de la frecuencia con que esto pase.
# 2. Creo que protocolo a nivel de Sitio debe ser muy general, por ejemplo:
# "Otros", "AGRRA_v5", "AGRRA_v5+" (AGRRA_v5 y adicionales). Propongo dejar los
# detalles a nivel muestreo de un aspecto, es decir, a nivel de las tablas "...info".
# Por ello, se elimina el campo "Site_sample.methodology_details".
# 3. Después de mucho pensar, creo que la columna "data_aggregation_level" debe
# estar a nivel de las tablas de "..._info", por ejemplo, "Coral_transect_sample_info",
#  Recruit_subquadrat_sample_from_transect_info", etc, ya que estas tablas son
# las que realmente especifican CADA ASPECTO MUESTREADO EN UN SITIO (independientemente
# de si hubo observaciones o no). Por medio del nombre de la tabla + la variable
# "data_aggregation_level", se puede saber para un sitio/transecto/etc:
#   1. Qué aspectos se muestrearon.
#   2. El nivel biológico de agregación de la información (individuo /
#   agregados por especie, etc).
#   3. El nivel geográfico de agregación de la información: cuadrante, transecto,
#   sitio, etc...
# Para visualizar esta abstracción, recordar que un join es ENTRE TABLAS, entonces,
# por ejemplo, los registros en un join entre "Site_sample", "Transect_sample"
# y "Fish_transect_sample_info", con "data_aggregation_level" = "por especie" en
# esta última tabla, especifican sitios donde la información de PECES (1) está
# agregada a nivel de ESPECIE (2), por transecto (3).
