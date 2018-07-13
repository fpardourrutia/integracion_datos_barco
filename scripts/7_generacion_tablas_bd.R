# En este script se usa "datos_globales" para generar las diversas tablas que se
# insertarán en la base de datos.

# La idea es que en "datos_globales" ya estén los datos lo más procesados posible,
# para simplemente "cortar" las tablas usando este script.

# Pasos a seguir:
# 1. Se lee el data frame "datos_globales".
# 2. Se crean las tablas que corresponden a datos que están en todos los archivos
# de Excel, a saber: "Muestreo", y "Muestra_sitio".

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo "datos_globales"
################################################################################

datos_globales <- readRDS(paste0(rutas_salida[6], "/datos_globales.RDS"))
glimpse(datos_globales)

datos_globales %>%
  pull(Auxiliar.archivo_origen) %>%
  unique()

################################################################################
# 2. Generando columnas que especifican qué registros tomar en cuenta para qué
# tablas
################################################################################

# Por ejemplo, todos los registros se toman en cuenta para formar "Muestreo" y
# "Muestra_sitio", pero sólo los de archivos de Excel de corales se toman en
# cuenta para formar "Muestra_transecto_corales_info" y
# "Muestra_transecto_corales_observacion"

datos_globales %>%
  mutate(
    ### Generales ###
    
    Auxiliar.integrar_en_muestreo = TRUE,
    Auxiliar.integrar_en_muestra_sitio = TRUE,
    Auxiliar.integrar_en_Muestra_transecto = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      FALSE, TRUE),
    
    ### Bentos ###
    
    Auxiliar.integrar_en_muestra_sitio_bentos_info = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    Auxiliar.integrar_en_muestra_sitio_bentos_porcentaje = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    Auxiliar.integrar_en_muestra_transecto_bentos_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_bentos_desagregados",
        "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados"
      ), TRUE, FALSE)
  )
  

################################################################################
# 2. Generando llaves primarias
################################################################################

#### Muestreo y Muestra_sitio ###

datos_globales_llaves_muestreo_muestra_sitio <- datos_globales %>%
  arrange(
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo
  ) %>%
  genera_llave("Muestreo.id", "Muestreo.nombre") %>%
  genera_llave("Muestra_sitio.id",
    "Muestreo.id", # Por limpieza se encadena la generación de llaves primarias
    "Muestra_sitio.nombre",
    "Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace",
    "Muestra_sitio.aux_remuestreo_en_mismo_muestreo")

################################################################################

### Muestra_transecto ###

datos_globales_llaves_muestreo_muestra_sitio

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