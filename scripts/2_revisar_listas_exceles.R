# En este script se revisan las listas de data frames con la información leída
# de los archivos de Excel. Básicamente:
# 1. Se leen las listas de tablas y catálogos que tienen los nombre de columnas
# homologados.
# 2. Se revisan los campos ligados a catálogo contra su correspondiente catálogo.
# 3. Se revisan columnas numéricas.
# 4. Se revistan registros duplicados con respecto a una llave natural

# Cargando archivo de configuración y funciones auxiliares
library("readr")
source("config.R")
source("funciones_auxiliares.R")

###############################################################
# Leyendo cada una de las listas individuales:
###############################################################

# Leyendo listas de exceles
lista_catalogos <- readRDS(
  paste0(ruta_salidas_0_leer_exceles, "/lista_catalogos.RData"))
lista_tablas_columnas_homologadas <- readRDS(
  paste0(ruta_salidas_1_homologar_columnas, "/lista_tablas_columnas_homologadas.RData"))

###############################################################
# Revisiones varias
###############################################################

# Generando una tabla preliminar para ayudar a la revisión de los valores en cada
# campo
tabla_revision <- Reduce(rbind.fill, lista_tablas_columnas_homologadas)
lista_revision <- revisa_valores(tabla_revision)
#encuentra_columnas(tabla_revision, "altura")

# Creando varios resúmenes de qué columnas contiene cada data frame
names(lista_tablas_columnas_homologadas)

lista_tablas_columnas_homologadas %>%
  crear_resumen_columnas_df() %>%
  glimpse()

lista_tablas_columnas_homologadas[lista_tablas_columnas_homologadas %>%
    names() %>%
    stri_detect_regex("bentos")
  ] %>%
  crear_resumen_columnas_df() %>%
  glimpse()

# Revisando cuántos catálogos tienen cada columna, para ver si hay que renombrar
# o no.
crear_resumen_columnas_df(lista_catalogos) %>%
  select(-nombre_df) %>%
  gather("variable", "valor") %>%
  group_by(variable) %>%
  summarise(
    n = sum(valor)
  ) %>%
  arrange(variable) %>%
  View()
# Parece que todo bien

###############################################################
# Revision de valores en catálogos:
###############################################################

# En esta sección se revisará que los campos asociados a un catálogo de cada
# data frame en "lista_datos_columnas_homologadas" efectivamente tengan todos sus
# valores tomados de dicho catálogo. En otro caso, se imprimirá la información
# necesaria para corregirlo.

# Generando la relación de columnas en catálogo:
relacion_columnas_catalogo <- c(
  
  # Proyecto y sitio
  ".tema" = "catalogos_proyecto__proposito.categoria", # propósito
  
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
  "conacyt_greenpeace_2016_bentos_desagregados_v3.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  
  # Corales
  "conacyt_greenpeace_2016_corales_desagregados_v3.metodo" =
    "catalogos_muestra_transecto_corales_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.metodo" =
    "catalogos_muestra_transecto_corales_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_corales_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_corales_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.depredacion" =
    "catalogos_muestra_transecto_corales_observacion__depredacion.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.depredacion" =
    "catalogos_muestra_transecto_corales_observacion__depredacion.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.lesiones" =
    "catalogos_muestra_transecto_corales_observacion__lesion.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.lesiones" =
    "catalogos_muestra_transecto_corales_observacion__lesion.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.enfermedades" =
    "catalogos_muestra_transecto_corales_observacion__enfermedad.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.enfermedades" =
    "catalogos_muestra_transecto_corales_observacion__enfermedad.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.sobrecrecimiento" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.sobrecrecimiento" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.sobrecrecimiento__1" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.sobrecrecimiento__1" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.blanqueamiento" =
    "catalogos_muestra_transecto_corales_observacion__tipo_blanqueamiento.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.blanqueamiento" =
    "catalogos_muestra_transecto_corales_observacion__tipo_blanqueamiento.categoria",
  # Falta campo "criterio_seleccion_colonias".
  
  # Invertebrados
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.metodo" =
    "catalogos_muestra_transecto_invertebrados_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.metodo" =
    "catalogos_muestra_transecto_invertebrados_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_invertebrados_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_invertebrados_info__nivel_agregacion_datos.categoria",
  
  # Peces
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.metodo" =
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.metodo" = 
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.metodo" =
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.metodo" = 
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nivel_agregacion_datos" = 
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nivel_agregacion_datos" = 
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.peces_muestreados" =
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_muestreados" = 
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_muestreados" =
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_muestreados" = 
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.especie" =
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico.especie",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.especie" = 
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico.especie",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.especie" =
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico.especie",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.especie" = 
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico.especie",
  
  # Reclutas
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.sustrato" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__sustrato.codigo",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.sustrato" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__sustrato.codigo",
  
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo" # Falta crear el catálogo de corales
)

valores_no_presentes_en_catalogo <- revisa_columnas_catalogos(
  lista_tablas_columnas_homologadas, lista_catalogos, relacion_columnas_catalogo) %>%
  group_by(tabla, campo, catalogo, valor) %>%
  tally()

# saveRDS(valores_no_presentes_en_catalogo,
#   paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_no_presentes_en_catalogo.RDS"))
# write_csv(valores_no_presentes_en_catalogo,
#   paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_no_presentes_en_catalogo.csv"))

###############################################################
# Revision de valores numéricos:
###############################################################

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
  "conacyt_greenpeace_2016_bentos_desagregados_v3.altura_algas_cm",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.altura_algas_cm",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.altura_algas_cm",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.altura_algas_cm",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.altura_algas_cm",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.cobertura",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.cobertura",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.cobertura",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.cobertura",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.cobertura",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.conteo",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.conteo",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.conteo",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.conteo",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.conteo",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.identificador_muestreo_sitio",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.identificador_sitio",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.identificador_sitio",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.identificador_sitio",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.identificador_sitio",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.longitud_teorica_m_bentos",
  # "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.longitud_teorica_m_bentos",
  # "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.longitud_teorica_m_bentos",
  # "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.longitud_teorica_m_bentos",
  # "historicos_y_2017_transecto_bentos_desagregado_pit_lit.longitud_teorica_m_bentos",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.puntos_o_cm_reales_transecto",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.puntos_o_cm_reales_transecto",
  "historicos_y_2017_sitio_bentos_desagregados_pit_lit_privados.puntos_o_cm_reales_transecto",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.puntos_o_cm_reales_transecto",
  "historicos_y_2017_transecto_bentos_desagregado_pit_lit.puntos_o_cm_reales_transecto",
  
  # Corales
  "conacyt_greenpeace_2016_corales_desagregados_v3.altura_maxima_cm",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.altura_maxima_cm",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.ancho_transecto_m",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.ancho_transecto_m",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.d1_max_diam_cm",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.d1_max_diam_cm",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.d2_min_diam_cm",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.d2_min_diam_cm",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.identificador_sitio",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.longitud_teorica_m_corales",
  # "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.longitud_teorica_m_corales",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.mortalidad_antigua",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_antigua",
  "conacyt_greenpeace_2016_corales_desagregados_v3.mortalidad_reciente",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_reciente",
  "conacyt_greenpeace_2016_corales_desagregados_v3.mortalidad_total",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_total",
  "conacyt_greenpeace_2016_corales_desagregados_v3.mortalidad_transicion",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.mortalidad_transicion",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.porcentaje",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.porcentaje",
  
  # Invertebrados
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.ancho_transecto_m",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.ancho_transecto_m",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.conteo",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.conteo",
  
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.cuadrante",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.identificador_sitio",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.longitud_teorica_m_invertebrados_agrra_v5",
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.longitud_teorica_m_invertebrados_otros",
  #"historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.longitud_teorica_m_invertebrados"
  
  # Peces
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.abundancia",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.abundancia",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.abundancia",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.abundancia",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.ancho_transecto_m",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.ancho_transecto_m",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.ancho_transecto_m",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.ancho_transecto_m",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.identificador_sitio",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.identificador_sitio",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.identificador_sitio",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.longitud_teorica_m_peces",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.longitud_teorica_m_peces",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.longitud_teorica_m_peces",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.longitud_teorica_m_peces",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_0cm_5cm",                            
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_0cm_5cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_0cm_5cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_0cm_5cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_101cm_110cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_101cm_110cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_101cm_110cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_101cm_110cm",
  # "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_101cm_9999cm",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_101cm_9999cm",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_101cm_9999cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_101cm_9999cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_111cm_120cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_111cm_120cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_111cm_120cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_111cm_120cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_11cm_20cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_191cm_200cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_191cm_200cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_191cm_200cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_191cm_200cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_21cm_30cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_31cm_40cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_41cm_50cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_51cm_60cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_61cm_70cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_6cm_10cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_71cm_80cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_81cm_90cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_91cm_100cm",
  
  # Reclutas
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.longitud_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.longitud_cuadrante_m",
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.ancho_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.ancho_cuadrante_m",
  
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.identificador_muestreo_sitio",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.identificador_sitio",
  
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.n",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.n",
  
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.tamanio_cm",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.tamanio_cm",
  
  # Complejidad
  "conacyt_greenpeace_2016_rugosidad_v3.identificador_muestreo_sitio",
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.identificador_sitio",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.identificador_sitio",
  
  "conacyt_greenpeace_2016_rugosidad_v3.relieve",
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.relieve",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.relieve",
  
  "conacyt_greenpeace_2016_rugosidad_v3.tamanio_cadena_m",
  "historicos_y_2017_transecto_complejidad_desagregada_cadena.tamanio_cadena_m",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.tamanio_cadena_m"
)

valores_esperados_numericos <- revisa_columnas_numericas(lista_tablas_columnas_homologadas, relacion_columnas_numericas,
  ceros_aceptables = FALSE)
# saveRDS(valores_esperados_numericos,
#   paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_esperados_numericos.RDS"))
# write_csv(valores_esperados_numericos,
#   paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_esperados_numericos.csv"))

## 2. Creando una tabla con la información de todos los Exceles
# Para construir las tablas especificadas en el esquema de bases de datos,
# primero se unirán los datos de todos los exceles (agregando columnas según se
# necesite), y luego se separarán esos datos en tablas

# Revisiones de la tabla siguiente:
# datos_globales$anio_inicio_proyecto %>% table(useNA = "always")

#!!! = "código posiblemente no generalizable a la hora de integrar más Exceles".

datos_globales <- Reduce(rbind.fill, lista_datos_columnas_homologadas) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  elimina_columnas_vacias() %>%
  
  ## Arreglando valores de las columnas
  
  ################
  # Proyecto
  ################
  
  cambia_valor_columna("titulo",
    paste0("Evaluación de la efectividadde las Áreas Marinas Protegidas en los ",
      "arrecifales del Caribe Mexicano"),
      paste0("Evaluación de la efectividad de las Áreas Marinas Protegidas ",
        "en los sistemas arrecifales del Caribe Mexicano")
  ) %>%
  
  # Arreglando el valor de "nombre_proyecto" == "NA" para los datos de
  # "conacyt_greenpeace". Esto es importante pues "nombre_proyecto" se
  # usará para crear una llave primaria de "proyecto_anual"
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') &
    nombre_proyecto == 'NA'",
    "nombre_proyecto", "CONACYT 247104 2016"
  ) %>%
  
  # Arreglando el valor de "documento" para los datos de "conacyt_greenpeace"
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016')",
    "documento", "datos de campo"
  ) %>%
  
  # Arreglando diversos campos:
  mutate(
    nombre_proyecto_sin_anio = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "CONACYT 247104",
      nombre_proyecto == "Greenpeace 2016" ~ "GreenPeace 2016",
      TRUE ~ nombre_proyecto
    ),
    
    anio_en_curso = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "2016",
      nombre_proyecto == "Greenpeace 2016" ~ "2016",
      TRUE ~ NA_character_
    ),
    
    anio_inicio_proyecto = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "2016",
      nombre_proyecto == "Greenpeace 2016" ~ "2016",
      TRUE ~ anio_inicio_proyecto
    ),
    
    anio_termino_proyecto = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "2017",
      nombre_proyecto == "Greenpeace 2016" ~ "2016",
      TRUE ~ anio_termino_proyecto
    )
    
  ) %>%
  
  cambia_valor_columna("cita",
    paste0("Álvarez-Filip, L. 2016. Evaluación de la efectividadde las Áreas ",
      "Marinas Protegidas en los arrecifales del Caribe Mexicano. ",
      "Proyecto CONACYT 247104."),
      paste0("Álvarez-Filip L., 2016. Evaluación de la efectividad de las Áreas ",
        "Marinas Protegidas en los arrecifales del Caribe Mexicano. ",
        "Proyecto CONACYT 247104.")
    ) %>%
  # las siguientes columnas desaparecerán al hacer los siguientes cambios y eliminar
  # columnas vacías
  cambia_valor_columna("anio_publicacion", "NA", NA) %>%
  cambia_valor_columna("anio_publicacion", "Monitoreo", NA) %>%
  cambia_valor_columna("numero_sitios_agregados", "NA", NA) %>%
  cambia_valor_columna("numero_sitios_agregados", "Especie", NA) %>%
  
  ################
  # Muestra_sitio
  ################

  # Debido a que hay varios typos en los campos de fecha y hora de muestreo de
  # sitio, se utilizará "genera_tabla_2" para crear esta tabla
  
  # Creando los nombres de los sitios: recordar que para los datos CONACyT /
  # GreenPeace tenemos nombres de muestreo de sitio, que son casi nombres de sitio,
  # Sólo Chankanaab y ChankanaabGP son el mismo sitio con distintos nombres.
  # Recordar que podemos seguirlos diferenciando porque ya agregamos la columna
  # "identificador_muestreo_sitio".
  mutate(
    nombre_sitio = estandariza_strings(nombre_sitio),
    nombre_sitio = ifelse(nombre_sitio == "chankanaabgp",
      "chankanaab", nombre_sitio)
  ) %>%
  
  # Creando fecha y hora de muestreo de sitio
  # Arreglando valores (que lo requieran) para formar los campos "fecha" y "hora"
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016')",
    "anio", "2016") %>%
  
  mutate(
    # Arreglando las horas de acuerdo a la regla que me dio Esme para los datos
    # CONACyT / GreenPeace:
    hora = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & hora == "1" ~ "13",
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & hora == "2" ~ "14",
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & hora == "3" ~ "15",
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & hora == "4" ~ "16",
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & hora == "5" ~ "17",
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & hora == "6" ~ "18",
      TRUE ~ hora
    ),
    
    # Agregando un 0 antes de meses y días cuando se requiera:
    mes = ifelse(as.numeric(mes) %in% c(1:9), paste0("0", mes), mes),
    dia = ifelse(as.numeric(dia) %in% c(1:9), paste0("0", dia), dia),
    fecha_muestreo_sitio = paste0(anio, "-", mes, "-", dia),
    
    # Agregando un 0 antes de horas y minutos cuando se requiera:
    hora = ifelse(as.numeric(hora) %in% c(0:9), paste0("0", hora), hora),
    # A veces "minutos" no es entero
    minutos = as.integer(minutos),
    minutos = ifelse(as.numeric(minutos) %in% c(0:9), paste0("0", minutos), minutos),
    hora_muestreo_sitio = ifelse(
      is.na(hora) | is.na(minutos), NA,
      paste0(hora, ":", minutos))
  ) %>%

  cambia_valor_columna("localidad", "Arrecife Alacranes", "Alacranes") %>%
  cambia_valor_columna("localidad", "Arrecife Triangulos", "Triángulos") %>%
  cambia_valor_columna("localidad", "Triangulos", "Triángulos") %>%
  cambia_valor_columna("localidad", "Cancun", "Cancún") %>%
  cambia_valor_columna("localidad", "EL Placer", "El Placer") %>%
  cambia_valor_columna("localidad", "PUERTO MORELOS", "Puerto Morelos") %>%
  cambia_valor_columna("localidad", "Triangulos", "Triángulos") %>%
  
  cambia_valor_columna("anp", "NA", NA) %>%
  
  # Haciendo columna "dentro de ANP, para los datos CONACyT / GreenPeace 2016.
  # Sabemos que si "anp" es NA quiere decir que no.
  # "No se" será un NA en "dentro_anp
  mutate(
    # Haciendo columna "dentro de ANP, para los datos CONACyT / GreenPeace 2016.
    # Sabemos que si "anp" es NA quiere decir que no.
    # "No se" será un NA en "dentro_anp
    dentro_anp = ifelse(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & is.na(anp),
      FALSE, TRUE),
    
    dentro_area_no_pesca = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & area_no_pesca == "si" ~ TRUE,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & area_no_pesca == "no" ~ FALSE,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & is.na(area_no_pesca) ~ NA)
  ) %>%
  
  # Arreglando "protocolo"
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016')",
    "protocolo", "AGRRA V5 modificado"
  ) %>%
  
  ####################
  # Muestra_transecto
  ####################

  # Arreglando nombres de transecto:
  # Esme me dijo que los 0's daban igual, entonces decido a
  # ponerles 0's a los que lo requieran, porque quiero que me los ordene por
  # número, pero son caracteres
  cambia_valor_columna("transecto", "1", "01") %>%
  cambia_valor_columna("transecto", "2", "02") %>%
  cambia_valor_columna("transecto", "3", "03") %>%
  cambia_valor_columna("transecto", "4", "04") %>%
  cambia_valor_columna("transecto", "5", "05") %>%
  cambia_valor_columna("transecto", "6", "06") %>%
  cambia_valor_columna("transecto", "7", "07") %>%
  cambia_valor_columna("transecto", "8", "08") %>%
  cambia_valor_columna("transecto", "9", "09") %>%
  
  cambia_valor_columna_condicion("transecto_fijo == 'no'",
    "transecto_fijo", FALSE) %>%
  
  # Para los transectos de CONACyT / GreenPeace 2016, asociarles la medida teórica
  # (30m los de peces y 10m los de bentos). En esta base es fácil distinguirlos
  # por el nombre (los de peces tienen una P)
  mutate(
    longitud_teorica_transecto_m = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & stri_detect_coll(transecto, 'P') ~ 30,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & !stri_detect_coll(transecto, 'P') ~ 10,
      # Si no, no jala...
      TRUE ~ NA_real_)
  ) %>%
  
  cambia_valor_columna("profundidad_inicial_m", "NA", NA) %>%

  mutate(
    # Para los muestreos de sitio del proyecto CONACyT / GREENPEACE, todos los
    # transectos de nombre 01...06 tienen 5 cuadrantes declarados...
    subcuadrantes_planeados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & transecto %in% paste0("0", 1:6) ~ TRUE,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & !(transecto %in% paste0("0", 1:6)) ~ FALSE,
      TRUE ~ NA),
    
    numero_subcuadrantes_planeados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & transecto %in% paste0("0", 1:6) ~ 5,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & !(transecto %in% paste0("0", 1:6)) ~ NA_real_,
      TRUE ~ NA_real_),
      
    seleccion_aleatoria_centros_subcuadrantes = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & transecto %in% paste0("0", 1:6) ~ FALSE,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & !(transecto %in% paste0("0", 1:6)) ~ NA,
      TRUE ~ NA
    ),
    
    longitud_subcuadrante_m = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & transecto %in% paste0("0", 1:6) ~ 0.25,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & !(transecto %in% paste0("0", 1:6)) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    ancho_subcuadrante_m = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & transecto %in% paste0("0", 1:6) ~ 0.25,
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') & !(transecto %in% paste0("0", 1:6)) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  
  ##################################################
  # Muestra_transecto_bentos_info
  ##################################################

  mutate(
    distancia_entre_puntos_pit_cm = ifelse(archivo_origen == "conacyt_greenpeace_2016_bentos_desagregados_v3", 10, NA),
    
    # En realidad, el siguiente campo sive para varios aspectos
    muestreo_completo = case_when(
      muestreo_completo == "no" ~ FALSE,
      muestreo_completo == "si" ~ TRUE,
      TRUE ~ NA
      )
  ) %>%
  
  ##################################################
  # Muestra_transecto_corales_observacion
  ##################################################

  # Para los datos de CONACYT / GreenPeace, cuando en las variables:
  # - blanqueamiento
  # - mortalidad_antigua
  # - mortalidad_reciente
  # - mortalidad_transicion
  # - mortalidad_total
  # - enfermedades
  # - sobrecrecimiento
  # - depredacion
  # - lesión (si hubiera)
  # Dice NA, en realidad es que no se encontró.

  # Para porcentaje, si blanqueamiento es NO o NA, vale NA. en otro caso, puede
  # valer el porcentaje de blanqueamiento del tipo seleccionado o NA si no se
  # registró el porcentaje.

  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(blanqueamiento)",
    "blanqueamiento",
    "NO"
  ) %>%
  
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(mortalidad_antigua)",
    "mortalidad_antigua",
    "0"
  ) %>%
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(mortalidad_reciente)",
    "mortalidad_reciente",
    "0"
  ) %>%
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(mortalidad_transicion)",
    "mortalidad_transicion",
    "0"
  ) %>%
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(mortalidad_total)",
    "mortalidad_total",
    "0"
  ) %>%
  
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(enfermedades)",
    "enfermedades",
    "NO"
  ) %>%
  
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(sobrecrecimiento)",
    "sobrecrecimiento",
    "NO"
  ) %>%
  
  cambia_valor_columna_condicion(
    "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(depredacion)",
    "depredacion",
    "NO"
  ) %>%
  
  # cambia_valor_columna_condicion(
  #   "archivo_origen == 'conacyt_greenpeace_2016_corales_desagregados_v3' & is.na(lesion)",
  #   "lesion",
  #   "NO"
  # ) %>%
  
  ##################################################
  # Muestra_transecto_invertebrados_info
  ##################################################

  # Generando variables de muestreo de invertebrados:
  mutate(
    
    todos_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ TRUE,
      TRUE ~ NA
    ),
    
    crustaceos_decapodos_carideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_estenopodideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_aquelados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_astacideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_braquiuros_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_anomuros_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_estomatopodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    crustaceos_decapodos_palinuridos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    
    crustaceos_no_decapodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    
    moluscos_gastropodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    moluscos_bivalvos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    moluscos_cefalopodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    otros_moluscos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    
    equinodermos_crinoideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    equinodermos_asteroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    equinodermos_ofiuroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    equinodermos_equinoideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    equinodermos_holothuroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    otros_equinodermos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    
    otros_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    
    detalles_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ "",
      TRUE ~ NA_character_
    )
    
  ) %>%
  
  ##################################################
  # Muestra_transecto_invertebrados_cuenta
  ##################################################

  cambia_valor_columna("especie", "NA", NA) %>%

  mutate(
    es_invertebrado_juvenil = case_when(
      archivo_origen == "conacyt_greenpeace_2016_invertebrados_desagregados_v3" &
      stri_detect_coll(especie, 'juvenil', case_insensitive = TRUE) ~ TRUE,
      archivo_origen == "conacyt_greenpeace_2016_invertebrados_desagregados_v3" &
        stri_detect_coll(especie, 'adulto', case_insensitive = TRUE) ~ FALSE,
      # En otro caso
      TRUE ~ NA
    ),
    
    invertebrados_nombre_cientifico = case_when(
      # Obteniendo el nombre científico de un invertebrado quitando si es juvenil o
      # adulto del nombre:
      archivo_origen == "conacyt_greenpeace_2016_invertebrados_desagregados_v3" &
        !is.na(es_invertebrado_juvenil) & # Si no lo especifico me marca error
        es_invertebrado_juvenil == TRUE ~
        stri_match_first_regex(especie, ".*(?= juvenil)", case_insensitive = TRUE) %>%
        as.character(),
      archivo_origen == "conacyt_greenpeace_2016_invertebrados_desagregados_v3" &
        !is.na(es_invertebrado_juvenil) &
        es_invertebrado_juvenil == FALSE ~
        stri_match_first_regex(especie, ".*(?= adulto)", case_insensitive = TRUE) %>%
        as.character(),
      archivo_origen == "conacyt_greenpeace_2016_invertebrados_desagregados_v3" &
        is.na(es_invertebrado_juvenil) ~
        especie,
      # En otro caso
      TRUE ~ NA_character_
    )
  ) %>%

##################################################
# Muestra_transecto_complejidad_info
##################################################

  mutate(
    rugosidad_longitud_lineal_m = case_when(
      archivo_origen == 'conacyt_greenpeace_2016_rugosidad_v3' ~ 10,
      TRUE ~ NA_real_
    ),
    rugosidad_longitud_lineal_fija = case_when(
      archivo_origen == 'conacyt_greenpeace_2016_rugosidad_v3' ~ TRUE,
      TRUE ~ NA
    )
  ) %>%

####################################################
# Muestra_subcuadrante_de_transecto_reclutas_cuenta
####################################################

  mutate(
    minimo_tamanio_cm = case_when(
      archivo_origen == "conacyt_greenpeace_2016_reclutas_desagregados_v3" &
        !is.na(categoria_tamanio) & categoria_tamanio == "R" ~ 0.01,
      archivo_origen == "conacyt_greenpeace_2016_reclutas_desagregados_v3" &
        !is.na(categoria_tamanio) & categoria_tamanio == "SC" ~ 2.01,
      TRUE ~ NA_real_
    ),
    
    maximo_tamanio_cm = case_when(
      archivo_origen == "conacyt_greenpeace_2016_reclutas_desagregados_v3" &
        !is.na(categoria_tamanio) & categoria_tamanio == "R" ~ 2,
      archivo_origen == "conacyt_greenpeace_2016_reclutas_desagregados_v3" &
        !is.na(categoria_tamanio) & categoria_tamanio == "SC" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  
  ######################################
  # Arreglando detalles finales
  ######################################
  
  mutate_numeric(
    "serie",
    "anio_inicio_proyecto",
    "anio_termino_proyecto",
    "latitud",
    "longitud",
    "anio_muestreo",
    "longitud_transecto_m",
    "puntos_o_cm_reales_transecto",
    "profundidad_inicial_m",
    "profundidad_final_m",
    "profundidad_media_m",
    "temperatura_c",
    "altura_algas_cm",
    "conteo",
    "cobertura",
    "profundidad_media_m_sitio",
    "longitud_teorica_m_bentos",
    "longitud_teorica_m_corales",
    "longitud_teorica_m_peces",
    "longitud_teorica_m_invertebrados_agrra_v5",
    "longitud_teorica_m_invertebrados_otros",
    "identificador_muestreo_sitio",
    "ancho_transecto_m",
    "altura_maxima_cm",
    "d1_max_diam_cm",
    "d2_min_diam_cm",
    "mortalidad_total",
    "mortalidad_reciente",
    "mortalidad_transicion",
    "mortalidad_antigua",
    "fision",
    "s",
    "porcentaje",
    "numero",
    "tamanio_0cm_5cm",
    "tamanio_6cm_10cm",
    "tamanio_11cm_20cm",
    "tamanio_21cm_30cm",
    "tamanio_31cm_40cm",
    "tamanio_41cm_50cm",
    "tamanio_51cm_60cm",
    "tamanio_61cm_70cm",
    "tamanio_71cm_80cm",
    "tamanio_81cm_90cm",
    "tamanio_91cm_100cm",
    "tamanio_111cm_120cm",
    "tamanio_191cm_200cm",
    "abundancia",
    "cuadrante",
    "longitud_cuadrante_m",
    "ancho_cuadrante_m",
    "tamanio_cadena_m",
    "id",
    "anio_en_curso",
    "longitud_teorica_transecto_m",
    "numero_subcuadrantes_planeados",
    "longitud_subcuadrante_m",
    "ancho_subcuadrante_m",
    "distancia_entre_puntos_pit_cm"
  ) %>%
  
  mutate_logical(
    "muestreo_completo",
    "area_no_pesca",
    "transecto_fijo",
    "dentro_anp",
    "dentro_area_no_pesca",
    "subcuadrantes_planeados",
    "seleccion_aleatoria_centros_subcuadrantes",
    "todos_invertebrados_muestreados",
    "crustaceos_decapodos_carideos_muestreados",
    "crustaceos_decapodos_estenopodideos_muestreados",
    "crustaceos_decapodos_aquelados_muestreados",
    "crustaceos_decapodos_astacideos_muestreados",
    "crustaceos_decapodos_braquiuros_muestreados",
    "crustaceos_decapodos_anomuros_muestreados",
    "crustaceos_decapodos_estomatopodos_muestreados",
    "crustaceos_decapodos_palinuridos_muestreados",
    "crustaceos_no_decapodos_muestreados",
    "moluscos_gastropodos_muestreados",
    "moluscos_bivalvos_muestreados",
    "moluscos_cefalopodos_muestreados",
    "otros_moluscos_muestreados",
    "equinodermos_crinoideos_muestreados",
    "equinodermos_asteroideos_muestreados",
    "equinodermos_ofiuroideos_muestreados",
    "equinodermos_equinoideos_muestreados",
    "equinodermos_holothuroideos_muestreados",
    "otros_equinodermos_muestreados",
    "otros_invertebrados_muestreados",
    "es_invertebrado_juvenil"
  ) %>%
  
  mutate(
    # Redondeando apropiadamente columnas numéricas que lo necesitan
    profundidad_inicial_m = round(profundidad_inicial_m, 1),
    profundidad_final_m = round(profundidad_final_m, 1),
    profundidad_media_m = round(profundidad_media_m, 2),
    profundidad_media_m_sitio = round(profundidad_media_m_sitio, 2),
    temperatura_c = round(temperatura_c, 2),
    
    # Creando columnas auxiliares útiles a la hora de generar las tablas
    strings_vacios = "",
    verdadero = TRUE,
    falso = FALSE,
    na_numerico = NA_real_,
    datum = "WGS84"
  ) # %>%
  
  # mutate(
    # Invertebrates_transect_sample_info
    # "numero" siempre es 1 para conacyt_greenpeace_2016_invertebrados_desagregados_v3"
    
    # Recruit_quadrant_sample_transect_count
    # En realidad no son cuentas, sino incidencias
    
    # # arreglando mortalidades si suman más de 100. Ésto se corregirá en v3.
    # suma_mortalidades = mortalidad_antigua +
    #   mortalidad_reciente +
    #   mortalidad_transicion +
    #   mortalidad_total
    # 
    # # Notar que si alguna mortalidad es NA, entonces la suma es NA y todas se
    # # hacen NA (lo cuál no es un problema porque acabamos de castear mortalidades
    # # NA a 0 para "CORALES_DESAGREGADOS_V2")
    # mortalidad_antigua = ifelse(suma_mortalidades > 100, #!!!
    #   (mortalidad_antigua / suma_mortalidades) * 100,
    #   mortalidad_antigua),
    # mortalidad_reciente = ifelse(suma_mortalidades > 100, #!!!
    #   (mortalidad_reciente / suma_mortalidades) * 100 ,
    #   mortalidad_reciente),
    # mortalidad_transicion = ifelse(suma_mortalidades > 100, #!!!
    #   (mortalidad_transicion / suma_mortalidades) * 100 ,
    #   mortalidad_transicion),
    # mortalidad_total = ifelse(suma_mortalidades > 100, #!!!
    #   (mortalidad_total / suma_mortalidades) * 100 ,
    #   mortalidad_total)
    
  # )

################################################################################
# Revisando valores de las columnas de datos_globales:
################################################################################
  
revision_valores <- revisa_valores(datos_globales)
names(revision_valores)

# Función para consultar el objeto anterior:
# nombre_columna: nombre de la columna a consultar.
# La función regresa la tabla correspondiente a esa columna
# El nombre de esta función es muy rápido para hacer la operación fácilmente
crv <- function(nombre_columna){
  return(revision_valores[[nombre_columna]])
}

# encuentra_columnas(datos_globales, "prof") %>%
#   set_names(.) %>%
#   llply(function(x) crv(x))

# saveRDS(datos_globales, "../productos/v3/datos_globales.RData")

# Comentarios Esme y Nuria CONACyT / GreenPeace 2016:
# 1. Falta localidad del proyecto. RESUELTO
# 2. Esme va a revisar los datos con "blanqueamiento" NA, pero con porcentaje y viceversa. RESUELTO
# datos_globales %>% group_by(blanqueamiento, porcentaje) %>% tally() %>% View()
# 3. ¿Por qué los transectos de corales tienen longitudes tan variables?
# Porque a veces no te da tiempo terminar...
# 4. ¿Qué diferencia hay entre "Anio" y "Anio_de_muestreo". Ninguna, usar Anio.
# 5. En "anio_inicio_proyecto" y "anio_termino_proyecto" aún hay datos con formato
# de fecha de Excel (todos los archivos). RESUELTO (ver código arriba)
# 6. Hay algunos sitios sin información de "area_no_pesca". Será que falta actualizar
# la tabla de "datos_anexos?" No, si no está no tenemos info.
# 7. ¿Qué es fisión y S? S: sana. Dejarla porque en algunos proyectos es importante.
# fisión: colonia rota en fragmentos.
# 8. Igual y convendría poner un campo de texto para explicar el "método de selección
# de sitios".
# 9. Falta "criterio de selección de colonias"
# 10. ¿Los datos de "ancho_transecto_m" son el ancho, el semi ancho o combinados?
# 11. FALTA: en cuanto Esme revise los datos de peces para que todas las especies
# coincidan con las del catálogo, hago el join para que la tabla de peces tenga
# el nombre científico (no la especie sólamente).
# 12. FALTA: renombrar "temperatura_en_celsius" a "temperatura_en_celsius_transecto"
# porque se requerirá una nueva columna si hay temperaturas a nivel de sitio.
# 13. FALTA: los códigos en el catálogo de bentos no son únicos...
# La "temperatura_c" copiarla al sitio porque no cambia entre transecto y sitio.

# Revisión para datos históricos:
# 1. Revisar campos de catálogo.
# 2. Revisar campos de tablas adicionales.
# 3. Esme me va a difereciar transectos con el mismo nombre en el mismo muestreo 
#   de sitio, pero que en realidad son distintos, por ejemplo, el transecto 1 de peces
#   y el de bentos.
# 4. Checar qué significa el sustrato "0". Cuadrantes que no se hicieron
# 5. Checar SC (Small Coral) con codigo NA. RESUELTO
# 6. NECESITO que todos los registros de un mismo muestreo de sitio tengan la misma
#    hora, sino, al homologar nombres de sitio (ante remuestreos), va a ser muy difícil
#    distinguir entre remuestreos.
# 7. Lorenzo me confirmó que los peces muestreados en CONACyT / GreenPeace no
#    fueron sólo AGRRA, sino todos.

