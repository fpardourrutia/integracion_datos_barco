# En este script se revisan las listas de data frames con la información leída
# de los archivos de Excel. Básicamente:
# 1. Se leen las listas de tablas y catálogos que tienen los nombres de columnas
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
    stri_detect_regex("peces")
  ] %>%
  crear_resumen_columnas_df() %>%
  glimpse()

tabla_revision %>%
  filter(stri_detect_regex(archivo_origen, pattern = "invertebrados")) %>%
  d_ply(.(archivo_origen), function(df){
    df %>%
      pull(archivo_origen) %>%
      unique() %>%
      print()
    df %>%
      elimina_columnas_vacias() %>%
      glimpse()
    })

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
  "conacyt_greenpeace_2016_bentos_desagregados.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  
  "conacyt_greenpeace_2016_bentos_desagregados.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_bentos_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_bentos_desagregados_pit_lit.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  
  # Corales
  "conacyt_greenpeace_2016_corales_desagregados.metodo" =
    "catalogos_muestra_transecto_corales_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.metodo" =
    "catalogos_muestra_transecto_corales_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_corales_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_corales_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  
  "conacyt_greenpeace_2016_corales_desagregados.depredacion" =
    "catalogos_muestra_transecto_corales_observacion__depredacion.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.depredacion" =
    "catalogos_muestra_transecto_corales_observacion__depredacion.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.lesiones" =
    "catalogos_muestra_transecto_corales_observacion__lesion.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.lesiones" =
    "catalogos_muestra_transecto_corales_observacion__lesion.categoria",
  
  "conacyt_greenpeace_2016_corales_desagregados.enfermedades" =
    "catalogos_muestra_transecto_corales_observacion__enfermedad.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.enfermedades" =
    "catalogos_muestra_transecto_corales_observacion__enfermedad.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.sobrecrecimiento" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.sobrecrecimiento" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.sobrecrecimiento__1" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.sobrecrecimiento__1" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados.blanqueamiento" =
    "catalogos_muestra_transecto_corales_observacion__tipo_blanqueamiento.categoria",
  "historicos_y_2017_transecto_corales_desagregados_colonias_individuales.blanqueamiento" =
    "catalogos_muestra_transecto_corales_observacion__tipo_blanqueamiento.categoria",
  # Falta campo "criterio_seleccion_colonias".
  
  # Invertebrados
  "conacyt_greenpeace_2016_invertebrados_desagregados.metodo" =
    "catalogos_muestra_transecto_invertebrados_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.metodo" =
    "catalogos_muestra_transecto_invertebrados_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_invertebrados_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_invertebrados_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_invertebrados_desagregados.tipo" =
    "catalogos_muestra_transecto_invertebrados__tipo.tipo",
  "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie.tipo" =
    "catalogos_muestra_transecto_invertebrados__tipo.tipo",
  
  # Peces
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.metodo" =
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.metodo" = 
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.metodo" =
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.metodo" = 
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nivel_agregacion_datos" = 
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nivel_agregacion_datos" = 
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.peces_muestreados" =
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.peces_muestreados" = 
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.peces_muestreados" =
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.peces_muestreados" = 
    "catalogos_muestra_transecto_peces_info__peces_muestreados.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.nombre_cientifico_abreviado" =
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.nombre_cientifico_abreviado" = 
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.nombre_cientifico_abreviado" =
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.nombre_cientifico_abreviado" = 
    "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico_abreviado.nombre_cientifico_abreviado",
  
  # Reclutas
  "conacyt_greenpeace_2016_reclutas_desagregados.nivel_agregacion_datos" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__nivel_agregacion_datos.categoria",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.nivel_agregacion_datos" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.sustrato" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__sustrato.codigo",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.sustrato" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__sustrato.codigo",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo" # Falta crear el catálogo de corales
)

valores_no_presentes_en_catalogo <- revisa_columnas_catalogos(
  lista_tablas_columnas_homologadas, lista_catalogos, relacion_columnas_catalogo) %>%
  group_by(tabla, campo, catalogo, valor) %>%
  tally()

saveRDS(valores_no_presentes_en_catalogo,
  paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_no_presentes_en_catalogo.RDS"))
write_csv(valores_no_presentes_en_catalogo,
  paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_no_presentes_en_catalogo.csv"))

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
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_0cm_5cm",                            
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_0cm_5cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_0cm_5cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_0cm_5cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_101cm_110cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_101cm_110cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_101cm_110cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_101cm_110cm",
  # "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_101cm_9999cm",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_101cm_9999cm",
  # "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_101cm_9999cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_101cm_9999cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_111cm_120cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_111cm_120cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_111cm_120cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_111cm_120cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_11cm_20cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_11cm_20cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_191cm_200cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_191cm_200cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_191cm_200cm",
  # "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_191cm_200cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_21cm_30cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_21cm_30cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_31cm_40cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_31cm_40cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_41cm_50cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_41cm_50cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_51cm_60cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_51cm_60cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_61cm_70cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_61cm_70cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_6cm_10cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_6cm_10cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_71cm_80cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_71cm_80cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_81cm_90cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_81cm_90cm",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla.tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados.tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla.tamanio_91cm_100cm",
  "historicos_y_2017_transecto_peces_desagregados_especie_talla.tamanio_91cm_100cm",
  
  # Reclutas
  "conacyt_greenpeace_2016_reclutas_desagregados.longitud_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.longitud_cuadrante_m",
  "conacyt_greenpeace_2016_reclutas_desagregados.ancho_cuadrante_m",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.ancho_cuadrante_m",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.identificador_muestreo_sitio",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.identificador_sitio",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.n",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.n",
  
  "conacyt_greenpeace_2016_reclutas_desagregados.tamanio_cm",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.tamanio_cm",
  
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
saveRDS(valores_esperados_numericos,
  paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_esperados_numericos.RDS"))
write_csv(valores_esperados_numericos,
  paste0(ruta_salidas_2_revisar_listas_exceles, "/valores_esperados_numericos.csv"))