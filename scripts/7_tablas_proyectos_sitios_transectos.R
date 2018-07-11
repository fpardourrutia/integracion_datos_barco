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

datos_globales <- readRDS("../productos/v3/datos_globales.RData")
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
  genera_llave("muestreo_id", "nombre_proyecto") %>%
  genera_llave("muestra_sitio_id", "identificador_muestreo_sitio") %>%
  genera_llave("muestra_transecto_id", "muestra_sitio_id", "transecto")
saveRDS(datos_globales_llaves_primarias, "../productos/v3/datos_globales_llaves_primarias.RData")

######################################
# Generando la tabla "Proyecto_anual"
######################################

lista_columnas_muestreo <- list(
  nombre = "nombre_proyecto",
  descripcion = "titulo",
  proposito = "tema",
  area_estudio = "localidad_proyecto",
  organizacion = "institucion",
  suborganizacion = "suborganizacion",
  encargado = "autor_administrador_proyecto",
  contacto = "contacto",
  referencia = "cita",
  comentarios = "strings_vacios",
  nombre_proyecto = "nombre_proyecto_sin_anio",
  anio_inicio_proyecto = "anio_inicio_proyecto",
  anio_termino_proyecto = "anio_termino_proyecto"
  # Faltan columnas del cliente de captura
)

muestreo <- genera_tabla_2(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "muestreo_id",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_muestreo
  ) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()

####################################
# Generando la tabla "Muestra_sitio"
####################################

lista_columnas_muestra_sitio <- list(
  muestreo_id = "muestreo_id",
  nombre = "nombre_sitio",
  fecha = "fecha_muestreo_sitio",
  hora = "hora_muestreo_sitio",
  pais = "pais",
  region_healthy_reefs = "region_healthy_reefs",
  localidad = "localidad",
  tipo_arrecife = "tipo_arrecife",
  subtipo_arrecife = "subtipo_arrecife",
  zona_arrecifal = "zona_arrecife",
  dentro_area_natural_protegida = "dentro_anp",
  nombre_area_natural_protegida = "anp",
  dentro_area_no_pesca = "dentro_area_no_pesca",
  latitud = "latitud",
  longitud = "longitud",
  datum = "datum",
  metodo_seleccion = "metodo_seleccion_sitios",
  metodologia = "protocolo",
  temperatura_c = "na_numerico", # Esta es por transecto para "CONACyT_GREENPEACE"
  profundidad_m = "profundidad_media_m_sitio",
  comentarios = "strings_vacios",
  fuente = "documento"
  
  # Faltan columnas del cliente de captura
)

muestra_sitio <- genera_tabla_2(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "muestra_sitio_id",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_muestra_sitio
) %>%
  # Para los datos CONACyT / GreenPeace no se necesita, pero igual y para los
  # datos históricos sí.
  cambia_na_strings_vacios()

########################################
# Generando la tabla "Muestra_transecto"
########################################
# Supuestos: en cada transecto se vio al menos una observación, sea la que sea.
# En particular, para los datos de CONACyT / GreenPeace:
# bentos: alguna de bentos, corales, reclutas, complejidad (invertebrados si aplica)
# peces: agún pez (o invertebrado), si aplica.

lista_columnas_muestra_transecto <- list(
  muestra_sitio_id = "muestra_sitio_id",
  nombre = "transecto",
  transecto_fijo = "transecto_fijo",
  longitud_m = "longitud_teorica_transecto_m",
  temperatura_c = "temperatura_c",
  profundidad_inicial_m = "profundidad_inicial_m",
  profundidad_final_m = "profundidad_final_m",
  subcuadrantes_planeados = "subcuadrantes_planeados",
  numero_subcuadrantes_planeados = "numero_subcuadrantes_planeados",
  seleccion_aleatoria_centros_subcuadrantes = "seleccion_aleatoria_centros_subcuadrantes",
  longitud_subcuadrante_m = "longitud_subcuadrante_m",
  ancho_subcuadrante_m = "ancho_subcuadrante_m",
  comentarios = "strings_vacios"
)

muestra_transecto <- genera_tabla_2(
  df = datos_globales_llaves_primarias,
  nombre_columna_llave = "muestra_transecto_id",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_muestra_transecto)

# save(
#   muestreo,
#   muestra_sitio,
#   muestra_transecto,
#   file = "../productos/v3/tablas_muestreos_sitios_transectos.RData"
# )