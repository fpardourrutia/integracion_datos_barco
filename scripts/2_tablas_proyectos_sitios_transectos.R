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
  start_year = "anio_inicio_proyecto",
  end_year = "anio_termino_proyecto",
  organization = "institucion",
  suborganization = "suborganizacion",
  person_in_charge = "autor_administrador_proyecto",
  contact = "contacto",
  reference = "cita",
  comments = "strings_vacios"
  
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
  datetime = "fecha_hora_muestreo_sitio",
  country = "pais",
  healthy_reefs_region = "region_healthy_reefs",
  location = "localidad",
  
  # Falta definir (y depurar) las variables "tipo_arrecide, "zona_arrecifal",
  # "subzona_habitat", "nombre_arrecife".
  
  inside_protected_area = "dentro_anp",
  protected_area_name = "anp",
  # Falta "dentro_area_no_pesca" (inside_non_fishing_area)
  latitude = "latitud",
  longitude = "longitud",
  datum = "datum",
  
  selection_method = "metodo_seleccion_sitios", # Cambios en el esquema!
  methodology = "protocolo_muestreo_sitio",
  
  # Esme me calculará la profundidad por sitio
  depth_m = "profundidad_media_m",
  temperature_c = "temperatura_c",
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

# Generando tabla con la información de "Subquadrat_samples_from_transect_info"
# Ahora es auxiliar pues estará ligada a transecto.
# Supuestos:
# 1. Cada muestreo de cuadrantes realizado fue registrado en los Exceles
# de "Reclutas" (independientemente si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más una definición de muestras por cuadrantes.

lista_columnas_subquadrat_samples_from_transect_info_aux <- list(
  subquadrats_planned = "verdadero",
  random_selection_subqudrat_centers = "falso",
  subquadrat_length_m = "longitud_cuadrante_m",
  subquadrat_width_m = "ancho_cuadrante_m",
  number_subquadrats_planned = "cantidad_cuadrantes_realizados"
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
  name = "transecto",
  length_m = "Voy a necesitar un campo con la longitud teórica de cada transecto",
  temperature_c = "temperatura_c",
  start_depth_m = "profundidad_inicial_m",
  end_depth_m = "profundidad_final_m",
  comments = "strings_vacios"
  )

transect_sample <- genera_tabla(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_muestreo_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_transect_sample) %>%
  left_join(subquadrat_samples_from_transect_info_aux,
    by = c("id" = "id_muestreo_transecto")) %>%
  mutate(
    subquadrats_planned = ifelse(is.na(subquadrats_planned), FALSE, subquadrats_planned)
  )

# save(
#   project,
#   site_sample,
#   transect_sample,
#   file = "../productos/tablas_proyectos_sitios_transectos.RData"
# )