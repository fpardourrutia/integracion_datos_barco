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
# Generando la tabla "Proyecto"
##############################

lista_columnas_proyecto <- list(
  nombre = "nombre_proyecto",
  descripcion = "titulo",
  proposito = "tema",
  #localizacion falta
  anio_inicio = "anio_inicio_proyecto",
  anio_termino = "anio_termino_proyecto",
  organizacion = "institucion",
  suborganizacion = "suborganizacion",
  encargado = "autor_administrador_proyecto",
  contacto = "contacto",
  referencia = "cita",
  comentarios = "strings_vacios"
  # Faltan columnas del cliente de captura
)

proyecto <- genera_tabla_2(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "id_proyecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_proyecto
  ) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()

####################################
# Generando la tabla "Muestra_sitio"
####################################

lista_columnas_muestra_sitio <- list(
  proyecto_id = "id_proyecto",
  nombre = "nombre_sitio", #
  fecha_hora = "fecha_hora_muestreo_sitio", #
  pais = "pais", # Esme lo corregirá
  region_healthy_reefs = "region_healthy_reefs", # Esme lo corregirá
  localidad = "localidad",
  tipo_arrecife = "tipo_arrecife",
  subtipo_arrecife = "subtipo_arrecife",
  zona_arrecifal = "zona_arrecife",
  dentro_area_natural_protegida = "dentro_anp", #
  nombre_area_natural_protegida = "anp",
  dentro_area_no_pesca = "dentro_area_no_pesca"
  latitud = "latitud",
  longitud = "longitud",
  datum = "datum",
  metodo_seleccion = "metodo_seleccion_sitios", # Esme lo corregirá
  metodologia = "protocolo", # Los que dicen "Sin Protocolo" son los que tienen transectos de invertebrados
  profundidad_m = "profundidad_media_m_sitio",
  temperatura_c = "temperatura_c", # Esta es por transecto para "CONACyT_GREENPEACE"
  comentarios = "strings_vacios"
  
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