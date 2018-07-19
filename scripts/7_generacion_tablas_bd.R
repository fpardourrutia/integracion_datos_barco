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

datos_globales_columnas_indicadoras <- datos_globales %>%
  
  mutate(
    
    ### Generales ###
    
    Auxiliar.integrar_en_Muestreo = TRUE,
    
    Auxiliar.integrar_en_Muestra_sitio = TRUE,
    
    Auxiliar.integrar_en_Muestra_transecto = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      FALSE, TRUE),
    
    ### Bentos ###
    
    Auxiliar.integrar_en_Muestra_sitio_bentos_info = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_sitio_bentos_porcentaje = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_bentos_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_bentos_desagregados",
        "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_bentos_porcentaje = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_bentos_punto = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_bentos_desagregados",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados"
      ), TRUE, FALSE),
    
    ### Corales ###
    
    Auxiliar.integrar_en_Muestra_transecto_corales_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_corales_desagregados",
        "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_corales_observacion = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_corales_desagregados",
        "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
      ), TRUE, FALSE),
    
    ### Peces ###
    
    Auxiliar.integrar_en_Muestra_transecto_peces_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_peces_agregados_especie_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
        "historicos_y_2017_transecto_peces_desagregados_especie_talla"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_peces_cuenta = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_peces_agregados_especie_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
        "historicos_y_2017_transecto_peces_desagregados_especie_talla"
      ), TRUE, FALSE),
    
    ### Invertebrados ###
    
    Auxiliar.integrar_en_Muestra_transecto_invertebrados_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_invertebrados_desagregados",
        "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_invertebrados_cuenta = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_invertebrados_desagregados",
        "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
      ), TRUE, FALSE),
    
    ### Reclutas ###
    
    Auxiliar.integrar_en_Muestra_subcuadrante_de_transecto_reclutas_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_reclutas_desagregados",
        "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_cuadrante_reclutas_desagregados"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_subcuadrante_de_transecto_reclutas_cuenta = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_reclutas_desagregados",
        "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_cuadrante_reclutas_desagregados"
      ), TRUE, FALSE),
    
    ### Complejidad ###
    
    Auxiliar.integrar_en_Muestra_transecto_complejidad_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_complejidad",
        "historicos_y_2017_transecto_complejidad_desagregada_cadena"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_subcuadrante_de_transecto_complejidad_info = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve",
      TRUE, FALSE)
  )

################################################################################
# 3. Generando llaves numericas
################################################################################

# Prefijo para localizar las columnas indicadoras de registros correspondientes
# a cada tabla nueva
prefijo_columnas_indicadoras <- "Auxiliar.integrar_en_"

# Sufijo para nombrar las columnas que contendrán las llaves numéricas
sufijo_columnas_llaves_numericas <- ".id"

relacion_tablas_columnas_llaves <- list(
  
  ### Tablas de "Muestreo", "Muestra_sitio" y "Muestra_transecto":
  
  "Muestreo" = c(
    "Muestreo.nombre"
  ),
  
  # No se utiliza "Muestra_sitio.fecha" porque puede dar lugar a muchos errores
  "Muestra_sitio" = c(
    "Muestreo.id", # Recordar que las llaves numéricas se generan iterativamente
    "Muestra_sitio.nombre",
    "Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace",
    "Muestra_sitio.aux_remuestreo_en_mismo_muestreo"
  ),
  
  # Para las siguientes tablas recordar que antes de generar la llave numérica,
  # los registros que no deban ser tomados en cuenta serán filtrados automática-
  # mente de acuerdo a la columna indicadora de la tabla en cuestión.
  
  "Muestra_transecto" = c(
    "Muestra_sitio.id",
    "Muestra_transecto.nombre"
  ),
  
  ### Bentos ###
  
  "Muestra_sitio_bentos_info" = c(
    "Muestra_sitio.id"
  ),
  
  "Muestra_sitio_bentos_porcentaje" = c(
  ),
  
  "Muestra_transecto_bentos_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_transecto_bentos_porcentaje" = c(
  ),
  
  "Muestra_transecto_bentos_punto" = c(
  ),
  
  ### Corales ###
  
  "Muestra_transecto_corales_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_transecto_corales_observacion" = c(
  ),
  
  ### Peces ###
  
  "Muestra_transecto_peces_info" = c(
    "Muestra_transecto.id"
  ),
  
  # Recordar que al crear el data frame homologado ya se le dio forma a esta
  # tabla
  "Muestra_transecto_peces_cuenta" = c(
  ),
  
  ### Invertebrados ###
  
  "Muestra_transecto_invertebrados_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_transecto_invertebrados_cuenta" = c(
  ),
  
  ### Reclutas ###
  
  "Muestra_subcuadrante_de_transecto_reclutas_info" = c(
    "Muestra_transecto.id",
    "Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante"
  ),
  
  "Muestra_subcuadrante_de_transecto_reclutas_cuenta" = c(
  ),
  
  ### Complejidad ###
  
  "Muestra_transecto_complejidad_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_subcuadrante_de_transecto_complejidad_info" = c(
    "Muestra_transecto.id",
    "Muestra_subcuadrante_de_transecto_complejidad_info.numero_cuadrante"
  )
)

datos_globales_llaves_numericas <- genera_llaves_varias_tablas(
  datos_globales_columnas_indicadoras,
  prefijo_columnas_indicadoras,
  sufijo_columnas_llaves_numericas,
  relacion_tablas_columnas_llaves
)

################################################################################

# La siguiente función es un wrapper de "genera_tabla_1()" y genera_tabla_2()",
# que permite, dado un data frame con datos que se dividirán en tablas, y una
# columna por llave numérica correspondiente a tabla nueva, formar las nuevas
# tablas con ciertas columnas especificadas para cada una.

# Parámetros:
# - df: una tabla de datos que se segmentará en distintas tablas. Para cada
# tabla a crear, "df" debe contener una columna de llave numérica, es decir, una
# columna donde cada valor distinto represente un registro distinto.
# - sufijo: los nombres de columnas con las llaves anteriores deberán ser de la
#   forma "nombre_tabla_nueva"sufijo, para ser fácilmente localizables.
# - relacion_tablas_columnas_funciones: una lista que especificará qué columnas
# contendrá cada tabla, además de la función que se utilizará para agregar los
# datos en caso de que se encuentren varias combinaciones por valor de la llave.
# Esta lista será especificada como sigue:
# list("nombre_tabla_nueva#numero_funcion_agregacion" = c(
#   "nombre_nuevo_columna_1" = "nombre_columna_1",
#   "nombre_nuevo_columna_2" = nombre_columna_2",
#   ...), ...)
# Donde "numero_funcion_agregacion" = i, si se quiere utilizar la función
# "genera_tabla_i()".
#
# La función regresa una lista nombrada que contiene las tablas nuevas generadas
# con las especificaciones anteriores. Cabe destacar los siguientes puntos:
# - El nombre en cada tabla nueva de la columna que corresponde a la llave
# numérica es simplemente "id".
# - La función está diseñada para trabajar en conjunto con "genera_llaves_varias_tablas".
# Por este motivo, a la hora de integrar cualquier tabla, se ignoran los registros
# que tienen NA en la llave correspondiente.

genera_tablas <- function(df, sufijo, relacion_tablas_columnas_funciones){
  
  nombres_columnas_df <- colnames(df)
  nombres_tablas_nuevas <- (names(relacion_tablas_columnas_funciones) %>%
    stri_match_first_regex("(.*)#.*"))[,2]
  numeros_funciones_agregacion <- as.numeric((names(relacion_tablas_columnas_funciones) %>%
      stri_match_first_regex(".*#(.*)"))[,2])
  
  # Generando los nombres de las columnas que contienen las llaves numéricas
  # a partir de los nombres de las tablas nuevas:
  nombres_columnas_llaves <- paste0(nombres_tablas_nuevas, ".id")
  
  # Revisando que los nombres de las tablas nuevas y de las funciones de agregación
  # estén correctamente formados.
  
  if(NA %in% nombres_tablas_nuevas){
    stop("Alguna tabla no está correctamente nombrada.")
  }
  
  if(!all(numeros_funciones_agregacion %in% c(1,2))){
    stop("Algún número de función de agregación no es válido")
  }
  
  # Revisando que todas las columnas correpondientes a llaves se encuentren en df
  if(!all(nombres_columnas_llaves %in% nombres_columnas_df)){
    stop(paste0("No se encontraron las columnas de llaves numéricas asociadas ",
      "a algunas tablas nuevas especificadas."))
  }
  
  # Revisando que todas las columnas especificadas como parte de las tablas nuevas
  # se encuentren en "df"
  l_ply(1:length(relacion_tablas_columnas_funciones), function(i){
    if(!all(relacion_tablas_columnas_funciones[[i]] %in% nombres_columnas_df)){
      stop(paste0("Algunas colummas que se desea formen parte de la tabla ",
        nombres_tablas_nuevas[i], " no se encontraron en df"))
    }
  })
  
  # Cortando las tablas nuevas utilizando la especificación correspondiente a
  # cada una.
  
  resultado <- llply(1:length(relacion_tablas_columnas_funciones), function(i){
    # nombre_tabla_nueva <- nombres_tablas_nuevas[i]
    numero_funcion_agregacion <- numeros_funciones_agregacion[i]
    nombre_columna_llave <- nombres_columnas_llaves[i]
    nombre_nuevo_columna_llave <- "id"
    vector_columnas_adicionales <- relacion_tablas_columnas_funciones[[i]]
    
    # Generando la expresión para el filter_:
    expresion_filter_ <- paste0("!is.na(", nombre_columna_llave, ")")
    
    # Generando la i-ésima tabla:
    
    if(numero_funcion_agregacion == 1){
      resultado <- df %>%
        filter_(expresion_filter_) %>%
        genera_tabla_1(df, nombre_columna_llave, nombre_nuevo_columna_llave,
          vector_columnas_adicionales)
    }else{
      resultado <- df %>%
        filter_(expresion_filter_) %>%
        genera_tabla_2(df, nombre_columna_llave, nombre_nuevo_columna_llave,
          vector_columnas_adicionales)
    }
    return(resultado)
  })
  
  names(resultado) <- nombres_tablas_nuevas
  return(resultado)
}

################################################################################



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
  df = datos_globales_llaves_numericas,
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