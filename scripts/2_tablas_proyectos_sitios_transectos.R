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
# Generando la tabla "Project"
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

project <- genera_tabla_2(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_proyecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_project
  ) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()

#################################
# Generando la tabla "Site_sample"
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

site_sample <- genera_tabla_2(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_muestreo_sitio",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_site_sample
) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()


#####################################
# Generando la tabla "Transect_sample"
#####################################
# Supuestos: en cada transecto se vio al menos una observación, sea la que sea:
# bentos: alguna de bentos, corales, reclutas, complejidad (invertebrados si aplica)
# peces: agún pez (o invertebrado), si aplica.

# Es importante tener completa la información de transectos, pero no por sí misma,
# más importante es tener completa la información de muestreos de cada aspecto
# realizados entre sitio (las tablas ...info)
# (información completa de aspectos => información completa de muestreos de transectos)

# Se propone fusionar las tablas de "Subquadrat_samples_from_transect_info" y
# "Transect_sample", además de eliminar la tabla de "Subquadrat_sample_from_transect",
# con el fin de simplificar enormemente el modelo de datos. Esto se hará en este
# momento:

# Generando tabla con la información de "Subquadrat_samples_from_transect_info"
# Ahora es auxiliar pues estará ligada a transecto.
# Supuestos:
# 1. Cada muestreo de cuadrantes realizado fue registrado en los Exceles
# de "Reclutas" (independientemente si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más una definición de muestras por cuadrantes.

lista_columnas_subquadrat_samples_from_transect_info_aux <- list(
  # Campos específicos del "aspecto": toma de datos por cuadrantes. El hecho
  # de que "start/end_depth_m" van a estar en la tabla de "transect_sample",
  # me sugirió que tal vez sería mejor que estén a nivel de transecto (a ver)
  # qué dicen Esme, Nuria y Lorenzo.
  subquadrats_sampled = "verdadero", # Cambios en el esquema!
  subquadrat_transect_start_depth_m = "profundidad_inicial_m", # Cambios en el esquema!
  subquadrat_transect_end_depth_m = "profundidad_final_m", # Cambios en el esquema!
  subquadrat_transect_sampled_length_m = "longitud_transecto_m", # Cambios en el esquema!
  random_selection_centers = "falso", # Cambios en el esquema!
  distance_between_centers_m = "na_numerico", # Cambios en el esquema!
  subquadrat_length_m = "longitud_cuadrante_m", # Cambios en el esquema!
  subquadrat_width_m = "ancho_cuadrante_m", # Cambios en el esquema!
  number_subquadrats_sampled = "cantidad_cuadrantes_realizados" # Cambios en el esquema!
)

subquadrat_samples_from_transect_info_aux <- datos_globales_llaves_primarias %>%
  filter(archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2") %>%
  # elimina_columnas_vacias() %>% # Para que no quite "na_numerico"
  # Calculando cantidad de cuadrantes por muestreo de transecto
  ddply(.(id_muestreo_transecto), function(df){
    cantidad_cuadrantes_realizados <- df %>%
      pull(cuadrante) %>%
      unique() %>%
      length()
    
    resultado <- df %>%
      mutate(
        cantidad_cuadrantes_realizados = cantidad_cuadrantes_realizados
      )
    return(resultado)
  }) %>%
  # para garantizar que sólo haya un registro por "id_muestreo_transecto"
  genera_tabla_2(
    nombre_columna_llave = "id_muestreo_transecto",
    nombre_nuevo_columna_llave ="id_muestreo_transecto",
    lista_columnas_adicionales = lista_columnas_subquadrat_samples_from_transect_info_aux
  )

lista_columnas_transect_sample <- list(
    site_sample_id = "id_muestreo_sitio",
    name = "transecto"
  )

transect_sample <- genera_tabla(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_muestreo_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_transect_sample) %>%
  left_join(subquadrat_samples_from_transect_info_aux,
    by = c("id" = "id_muestreo_transecto")) %>%
  mutate(
    subquadrats_sampled = ifelse(is.na(subquadrats_sampled), FALSE, subquadrats_sampled)
  )

# save(
#   project,
#   site_sample,
#   transect_sample,
#   file = "../productos/tablas_proyectos_sitios_transectos.RData"
# )