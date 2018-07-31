# En este script se revisan las listas de data frames con la información leída
# de los archivos de Excel. Las revisiones contenidas en este script son
# basadas en el esquema de datos. Se siguen los siguientes pasos:
# 1. Se leen las listas de tablas y catálogos que tienen nombres de columnas
# homologados.
# 2. Se hacen revisiones de qué tablas contienen qué columnas
# 3. Se revisan los campos ligados a catálogo contra su correspondiente catálogo.
# 4. Se revisan columnas que deben ser numéricas de acuerdo al esquema de datos.
# 5. Se revisan campos que no deben estar vacíos debido a que el esquema de datos
# así lo pide.

# Cargando archivo de configuración y funciones auxiliares
library("readr")
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo cada una de las listas individuales:
################################################################################

# Leyendo listas de exceles
lista_tablas_columnas_homologadas <- readRDS(
  paste0(rutas_salida[2], "/lista_tablas_columnas_homologadas.RData"))
lista_catalogos <- readRDS(
  paste0(rutas_salida[1], "/lista_catalogos.RData"))

################################################################################
# 2. Revisando las relaciones exceles/columnas que contienen
################################################################################

# Generando una tabla preliminar para ayudar a la revisión de los valores en cada
# campo
tabla_revision <- Reduce(rbind.fill, lista_tablas_columnas_homologadas)
#encuentra_columnas(tabla_revision, "altura")

names(lista_tablas_columnas_homologadas)

lista_tablas_columnas_homologadas %>%
  crea_resumen_columnas_df() %>%
  glimpse()

lista_tablas_columnas_homologadas[lista_tablas_columnas_homologadas %>%
    names() %>%
    stri_detect_regex("reclutas")
  ] %>%
  crea_resumen_columnas_df() %>%
  glimpse()

tabla_revision %>%
  filter(stri_detect_regex(archivo_origen, pattern = "reclutas")) %>%
  d_ply(.(archivo_origen), function(df){
    df %>%
      pull(archivo_origen) %>%
      unique() %>%
      print()
    df %>%
      elimina_columnas_vacias() %>%
      glimpse()
  })

################################################################################
# 3. Revision de valores en catálogos:
################################################################################

# En esta sección se revisará que los campos asociados a un catálogo de cada
# data frame en "lista_datos_columnas_homologadas" efectivamente tengan todos sus
# valores tomados de dicho catálogo. En otro caso, se imprimirá la información
# necesaria para corregirlo.

# Generando la relación de columnas en catálogo:
relacion_columnas_catalogo <- c(
  
  # Muestreo y sitio
  ".tema" = "catalogos_muestreo__proposito.categoria", # propósito
  
  ".pais" = "catalogos_muestra_sitio__pais.categoria",
  ".region_healthy_reefs" = "catalogos_muestra_sitio__region_healthy_reefs.categoria",
  ".tipo_arrecife" = "catalogos_muestra_sitio__tipo_arrecife.categoria",
  ".subtipo_arrecife" = "catalogos_muestra_sitio__subtipo_arrecife.categoria",
  ".zona_arrecife" = "catalogos_muestra_sitio__zona_arrecifal.categoria",
  ".anp" = "catalogos_muestra_sitio__nombre_area_natural_protegida.categoria",
  ".metodo_seleccion_sitios" = "catalogos_muestra_sitio__metodo_seleccion.categoria",
  ".protocolo" = "catalogos_muestra_sitio__metodologia.categoria",
  # Falta catálogo de "datum"
  
  # Bentos:
  "conacyt_greenpeace_2016_bentos_desagregados.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  
  "conacyt_greenpeace_2016_bentos_desagregados.metodo" =
    "catalogos_muestra_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.metodo" =
    "catalogos_muestra_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.metodo" =
    "catalogos_muestra_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.metodo" =
    "catalogos_muestra_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.metodo" =
    "catalogos_muestra_bentos_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_bentos_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" =
    "catalogos_muestra_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.nivel_agregacion_datos" =
    "catalogos_muestra_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" =
    "catalogos_muestra_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.nivel_agregacion_datos" =
    "catalogos_muestra_bentos_info__nivel_agregacion_datos.categoria",
  
  # Corales
  "conacyt_greenpeace_2016_corales_desagregados.metodo" =
    "catalogos_muestra_corales_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.metodo" =
    "catalogos_muestra_corales_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_corales_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.nivel_agregacion_datos" =
    "catalogos_muestra_corales_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.depredacion" =
    "catalogos_registro_corales__depredacion.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.depredacion" =
    "catalogos_registro_corales__depredacion.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.lesiones" =
    "catalogos_registro_corales__lesion.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.lesiones" =
    "catalogos_registro_corales__lesion.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.enfermedades" =
    "catalogos_registro_corales__enfermedad.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.enfermedades" =
    "catalogos_registro_corales__enfermedad.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.sobrecrecimiento" =
    "catalogos_registro_corales__sobrecrecimiento.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.sobrecrecimiento" =
    "catalogos_registro_corales__sobrecrecimiento.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.sobrecrecimiento__1" =
    "catalogos_registro_corales__sobrecrecimiento.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.sobrecrecimiento__1" =
    "catalogos_registro_corales__sobrecrecimiento.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.blanqueamiento" =
    "catalogos_registro_corales__tipo_blanqueamiento.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.blanqueamiento" =
    "catalogos_registro_corales__tipo_blanqueamiento.categoria",
  # Falta campo "criterio_seleccion_colonias".
  
  # Invertebrados
  "conacyt_greenpeace_2016_invertebrados_desagregados.metodo" =
    "catalogos_muestra_invertebrados_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.metodo" =
    "catalogos_muestra_invertebrados_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_invertebrados_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.nivel_agregacion_datos" =
    "catalogos_muestra_invertebrados_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.tipo" =
    "catalogos_registro_invertebrados__tipo.tipo",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.tipo" =
    "catalogos_registro_invertebrados__tipo.tipo",
  
  # Peces
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.metodo" =
    "catalogos_muestra_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.metodo" = 
    "catalogos_muestra_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.metodo" =
    "catalogos_muestra_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.metodo" = 
    "catalogos_muestra_peces_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.nivel_agregacion_datos" =
    "catalogos_muestra_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nivel_agregacion_datos" = 
    "catalogos_muestra_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" =
    "catalogos_muestra_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nivel_agregacion_datos" = 
    "catalogos_muestra_peces_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_muestreados" =
    "catalogos_muestra_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_muestreados" = 
    "catalogos_muestra_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_muestreados" =
    "catalogos_muestra_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_muestreados" = 
    "catalogos_muestra_peces_info__peces_muestreados.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.nombre_cientifico_abreviado" =
    "catalogos_registro_peces__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nombre_cientifico_abreviado" = 
    "catalogos_registro_peces__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nombre_cientifico_abreviado" =
    "catalogos_registro_peces__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nombre_cientifico_abreviado" = 
    "catalogos_registro_peces__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  
  # Reclutas
  "conacyt_greenpeace_2016_reclutas_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_reclutas_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" =
    "catalogos_muestra_reclutas_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_cuadrante_reclutas_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_reclutas_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.sustrato" =
    "catalogos_muestra_reclutas_info__sustrato.codigo",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.sustrato" =
    "catalogos_muestra_reclutas_info__sustrato.codigo",
  "historicos_y_2017_cuadrante_reclutas_desagregados.sustrato" =
    "catalogos_muestra_reclutas_info__sustrato.codigo",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.codigo" =
    "catalogos_registro_bentos__codigo.codigo",
  "historicos_y_2017_cuadrante_reclutas_desagregados.codigo" =
    "catalogos_registro_bentos__codigo.codigo"
)

valores_no_presentes_en_catalogo <- revisa_columnas_catalogos(
  lista_tablas_columnas_homologadas, lista_catalogos, relacion_columnas_catalogo) %>%
  group_by(tabla, campo, catalogo, valor) %>%
  tally()

write_csv(valores_no_presentes_en_catalogo,
  paste0(rutas_salida[4], "/valores_no_presentes_en_catalogo.csv"))

################################################################################
# 4. Revision de valores numéricos:
################################################################################

# En esta sección se revisará que las columnas que deben ser numéricas en los
# data frames de "lista_datos_columnas_homologadas" efectivamente lo sean.
# En otro caso, se imprimirá la información necesaria para corregirlo.
# Por facilidad, se revisarán todas las columnas numéricas como si no aceptaran
# 0's.

relacion_columnas_numericas <- c(
  
  # Proyecto y sitio
  ".anio",
  ".anio_inicio_proyecto",
  ".anio_muestreo",
  ".anio_publicacion",
  ".anio_termino_proyecto",
  ".latitud",
  ".longitud",
  ".longitud_transecto_m",
  ".dia",
  ".mes",
  ".minutos",
  ".numero_sitios_agregados",
  ".profundidad_final_m",
  ".profundidad_inicial_m",
  ".profundidad_media_m",
  ".profundidad_media_m_sitio",
  ".temperatura_c",
  
  # Bentos
  "conacyt_greenpeace_2016_bentos_desagregados.altura_algas_cm",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.altura_algas_cm",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.altura_algas_cm",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.altura_algas_cm",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.altura_algas_cm",
  
  "conacyt_greenpeace_2016_bentos_desagregados.cobertura",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.cobertura",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.cobertura",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.cobertura",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.cobertura",
  
  "conacyt_greenpeace_2016_bentos_desagregados.conteo",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.conteo",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.conteo",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.conteo",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.conteo",
  
  "conacyt_greenpeace_2016_bentos_desagregados.identificador_muestreo_sitio",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.identificador_sitio",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.identificador_sitio",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.identificador_sitio",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.identificador_sitio",
  
  "conacyt_greenpeace_2016_bentos_desagregados.longitud_teorica_m_bentos",
  # "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.longitud_teorica_m_bentos",
  # "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.longitud_teorica_m_bentos",
  # "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.longitud_teorica_m_bentos",
  # "historicos_y_2017_transecto_bentos_desagregados_pit_lit.longitud_teorica_m_bentos",
  
  "conacyt_greenpeace_2016_bentos_desagregados.puntos_o_cm_reales_transecto",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.puntos_o_cm_reales_transecto",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.puntos_o_cm_reales_transecto",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.puntos_o_cm_reales_transecto",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.puntos_o_cm_reales_transecto",
  
  # Corales
  "conacyt_greenpeace_2016_corales_desagregados.altura_maxima_cm",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.altura_maxima_cm",
  
  "conacyt_greenpeace_2016_corales_desagregados.ancho_transecto_m",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.ancho_transecto_m",
  
  "conacyt_greenpeace_2016_corales_desagregados.d1_max_diam_cm",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.d1_max_diam_cm",
  
  "conacyt_greenpeace_2016_corales_desagregados.d2_min_diam_cm",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.d2_min_diam_cm",
  
  "conacyt_greenpeace_2016_corales_desagregados.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.identificador_sitio",
  
  "conacyt_greenpeace_2016_corales_desagregados.longitud_teorica_m_corales",
  # "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.longitud_teorica_m_corales",
  
  "conacyt_greenpeace_2016_corales_desagregados.mortalidad_antigua",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_antigua",
  "conacyt_greenpeace_2016_corales_desagregados.mortalidad_reciente",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_reciente",
  "conacyt_greenpeace_2016_corales_desagregados.mortalidad_total",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_total",
  "conacyt_greenpeace_2016_corales_desagregados.mortalidad_transicion",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_transicion",
  
  "conacyt_greenpeace_2016_corales_desagregados.porcentaje",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.porcentaje",
  
  # Invertebrados
  "conacyt_greenpeace_2016_invertebrados_desagregados.ancho_transecto_m",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.ancho_transecto_m",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.conteo",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.conteo",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.identificador_sitio",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.longitud_teorica_m_invertebrados_agrra_v5",
  "conacyt_greenpeace_2016_invertebrados_desagregados.longitud_teorica_m_invertebrados_otros",
  #"historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.longitud_teorica_m_invertebrados"
  
  # Peces
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.abundancia",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.abundancia",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.abundancia",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.abundancia",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.ancho_transecto_m",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.ancho_transecto_m",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.ancho_transecto_m",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.ancho_transecto_m",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.identificador_sitio",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.identificador_sitio",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.identificador_sitio",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.longitud_teorica_m_peces",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.longitud_teorica_m_peces",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.longitud_teorica_m_peces",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.longitud_teorica_m_peces",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_0cm_5cm",                            
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_0cm_5cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_0cm_5cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_0cm_5cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_101cm_110cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_101cm_110cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_101cm_110cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_101cm_110cm",
  # "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_101cm_9999cm",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_101cm_9999cm",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_101cm_9999cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_101cm_9999cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_111cm_120cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_111cm_120cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_111cm_120cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_111cm_120cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_11cm_20cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_191cm_200cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_191cm_200cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_191cm_200cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_191cm_200cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_21cm_30cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_31cm_40cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_41cm_50cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_51cm_60cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_61cm_70cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_6cm_10cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_71cm_80cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_81cm_90cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_tamanio_91cm_100cm",
  
  # Reclutas
  "conacyt_greenpeace_2016_reclutas_desagregados.longitud_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.longitud_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_desagregados.longitud_cuadrante_m",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.ancho_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.ancho_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_desagregados.ancho_cuadrante_m",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.identificador_muestreo_sitio",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.identificador_sitio",
  "historicos_y_2017_cuadrante_reclutas_desagregados.identificador_sitio",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.n",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.n",
  "historicos_y_2017_cuadrante_reclutas_desagregados.n",

  "conacyt_greenpeace_2016_reclutas_desagregados.tamanio_minimo_cm",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.tamanio_minimo_cm",
  "historicos_y_2017_cuadrante_reclutas_desagregados.tamanio_minimo_cm",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.tamanio_maximo_cm",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.tamanio_maximo_cm",
  "historicos_y_2017_cuadrante_reclutas_desagregados.tamanio_maximo_cm",
  
  # Complejidad
  "conacyt_greenpeace_2016_complejidad.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.identificador_sitio",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.identificador_sitio",
  
  "conacyt_greenpeace_2016_complejidad.relieve",
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.relieve",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.relieve",
  
  "conacyt_greenpeace_2016_complejidad.tamanio_cadena_m",
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.tamanio_cadena_m",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.tamanio_cadena_m"
)

valores_esperados_numericos <- revisa_columnas_numericas(lista_tablas_columnas_homologadas, relacion_columnas_numericas,
  ceros_aceptables = FALSE)

write_csv(valores_esperados_numericos,
  paste0(rutas_salida[4], "/valores_esperados_numericos.csv"))

################################################################################
# 5. Revision de valores no vacíos por diseño del esquema de datos:
################################################################################

# En esta sección se revisarán que campos en los exceles que corresponden a
# campos que se definieron como no nulos en el esquema de datos, en realidad
# cumplan dicha restricción.

relacion_columnas_obligatorias <- c(
  
  ###  Muestreo ###
  ".nombre_proyecto" = NA, # Nombre del muestreo
  ".proposito" = NA, # Descripción del muestreo
  ".tema" = NA, # Propósito del muestreo
  ".localidad_proyecto" = NA, # Área de estudio
  ".institucion" = NA, # Organización
  ".autor_administrador_proyecto" = NA, # Encargado
  ".contacto" = NA, # Contacto
  ".cita" = NA,  # Referencia
  ".titulo" = NA, # Nombre del proyecto
  ".anio_inicio_proyecto" = NA,
  
  ### Muestra_sitio ###
  ".nombre_sitio" = NA,
  # ".nombre_original" = NA, # este lo obtendré de nombre_sitio cuando aplique.
  ".anio" = NA,
  ".mes" = NA,
  ".dia" = NA,
  ".pais" = NA,
  ".region_healthy_reefs" = NA,
  ".localidad" = NA,
  ".metodo_seleccion_sitios" = NA,
  ".protocolo" = NA,
  
  ### Muestra_transecto ###
  "conacyt_greenpeace_2016_bentos_desagregados.transecto" = NA,
  "conacyt_greenpeace_2016_complejidad.transecto" = NA,
  "conacyt_greenpeace_2016_corales_desagregados.transecto" = NA,
  "conacyt_greenpeace_2016_invertebrados_desagregados.transecto" = NA,
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.transecto" = NA,
  "conacyt_greenpeace_2016_reclutas_desagregados.transecto" = NA,
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.transecto" = NA,
  "historicos_y_2017_cuadrante_reclutas_desagregados.transecto" = NA,
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.transecto" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.transecto" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.transecto" = NA,
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.transecto" = NA,
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.transecto" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.transecto" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.transecto" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.transecto" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.transecto" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.transecto" = NA,
  
  ### Muestra_sitio_bentos_info ###
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.metodo" = NA,
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" = NA,
  # "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.longitud_muestreada_media_m" = NA # Sugerencia para Esme
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.puntos_o_cm_reales_transecto" = NA, # Númnero de puntos muestreados
  
  ### Muestra_sitio_bentos_porcentaje ###
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.codigo" = NA,
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.cobertura" = NA,
  
  ### Muestra_transecto_bentos_info ###
  "conacyt_greenpeace_2016_bentos_desagregados.metodo" = NA,
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.metodo" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.metodo" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.metodo" = NA,
  
  "conacyt_greenpeace_2016_bentos_desagregados.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.nivel_agregacion_datos" = NA,
  
  "conacyt_greenpeace_2016_bentos_desagregados.longitud_transecto_m" = NA, # longitud muestreada (m)
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.longitud_transecto_m" = NA,
  
  ### Muestra_transecto_bentos_punto ### Muestra_transecto_bentos_linea ### 
  "conacyt_greenpeace_2016_bentos_desagregados.codigo" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.codigo" = NA,
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.codigo" = NA,
  
  ### Muestra_transecto_bentos_porcentaje ###
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.codigo" = NA,
  
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.cobertura" = NA,
  
  ### Muestra_transecto_corales_info ###
  "conacyt_greenpeace_2016_corales_desagregados.metodo" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.metodo" = NA,
  
  "conacyt_greenpeace_2016_corales_desagregados.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.nivel_agregacion_datos" = NA,
  
  "conacyt_greenpeace_2016_corales_desagregados.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.longitud_transecto_m" = NA,
  
  "conacyt_greenpeace_2016_corales_desagregados.ancho_transecto_m" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.ancho_transecto_m" = NA,
  
  ### Muestra_transecto_corales_observacion ###
  "conacyt_greenpeace_2016_corales_desagregados.codigo" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.codigo" = NA,
  
  "conacyt_greenpeace_2016_corales_desagregados.clump" = NA,
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.clump" = NA,
  
  ### Muestra_transecto_peces_info ###
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.metodo" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.metodo" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.metodo" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.metodo" = NA,
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nivel_agregacion_datos" = NA,
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.longitud_transecto_m" = NA,
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.ancho_transecto_m" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.ancho_transecto_m" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.ancho_transecto_m" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.ancho_transecto_m" = NA,
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_muestreados" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_muestreados" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_muestreados" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_muestreados" = NA,
  
  ### Muestra_transecto_peces_cuenta ###
  
  # Cuidado con los siguientes porque puede haber muestras de transectos de peces sin observaciones: 
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.nombre_cientifico_abreviado" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nombre_cientifico_abreviado" = NA,
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nombre_cientifico_abreviado" = NA,
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nombre_cientifico_abreviado" = NA,
  
  ### Muestra_transecto_invertebrados_info ###
  "conacyt_greenpeace_2016_invertebrados_desagregados.metodo" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.metodo" = NA,
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.nivel_agregacion_datos" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.nivel_agregacion_datos" = NA,
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.longitud_transecto_m" = NA,
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.ancho_transecto_m" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.ancho_transecto_m" = NA,
  
  ### Muestra_transecto_invertebrados_cuenta ###
  # Cuidado con los siguientes porque puede haber muestras de transectos de invertebrados sin observaciones:
  "conacyt_greenpeace_2016_invertebrados_desagregados.tipo" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.tipo" = NA,
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.conteo" = NA,
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.conteo" = NA,
  
  ### Muestra_subcuadrante_de_transecto_reclutas_info ###
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = NA,
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = NA,
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = NA,
  
  "conacyt_greenpeace_2016_reclutas_desagregados.nivel_agregacion_datos" = NA,
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" = NA,
  "historicos_y_2017_cuadrante_reclutas_desagregados.nivel_agregacion_datos" = NA,
  
  ### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
  # Cuidado con los siguientes porque puede haber muestras de transectos de invertebrados sin observaciones:
  "conacyt_greenpeace_2016_reclutas_desagregados.codigo" = NA,
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.codigo" = NA,
  "historicos_y_2017_cuadrante_reclutas_desagregados.codigo" = NA,
  
  # Cuidado con los siguientes porque puede haber muestras de transectos de invertebrados sin observaciones:
  "conacyt_greenpeace_2016_reclutas_desagregados.categoria_tamanio" = NA,
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.categoria_tamanio" = NA,
  "historicos_y_2017_cuadrante_reclutas_desagregados.categoria_tamanio" = NA,
  
  # Cuidado con los siguientes porque puede haber muestras de transectos de invertebrados sin observaciones:
  "conacyt_greenpeace_2016_reclutas_desagregados.n" = NA,
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.n" = NA,
  "historicos_y_2017_cuadrante_reclutas_desagregados.n" = NA,
  
  ### Muestra_transecto_complejidad_info ###
  "conacyt_greenpeace_2016_complejidad.longitud_transecto_m" = NA,
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.longitud_transecto_m" = NA,
  
  "conacyt_greenpeace_2016_complejidad.tamanio_cadena_m" = NA,
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.tamanio_cadena_m" = NA,
  
  ### Muestra_subcuadrante_de_transecto_complejidad_info ###
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = NA,
  
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.relieve" = NA
)

columnas_obligatorias_a_revisar <- revisa_columnas_valores(lista_tablas_columnas_homologadas,
  relacion_columnas_obligatorias) %>%
  group_by(tabla, campo, valor) %>%
  tally()

write_csv(columnas_obligatorias_a_revisar,
  paste0(rutas_salida[4], "/columnas_obligatorias_a_revisar.csv"))