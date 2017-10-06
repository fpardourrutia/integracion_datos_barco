# En este script se genera el DF global de datos limpios, listo para partirse
# en tablas distintas.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("readr")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

###############################################################
# Leyendo cada una de las listas individuales:
###############################################################

# Leyendo listas de exceles
lista_datos_crudos_conacyt_greenpeace_2016 <- readRDS("../productos/v3/lista_datos_crudos_conacyt_greenpeace_2016.RData")
lista_catalogos <- readRDS("../productos/v3/lista_catalogos.RData") %>%
  renombra_columnas_minusculas()

# Generando data frame auxiliar con los nombres de las columnas de cada tabla,
# para ver qué tanto renombrar.
# df_resumen_conacyt_greenpeace_2016 <- crear_resumen_columnas_df(lista_datos_crudos_conacyt_greenpeace_2016)
# glimpse(df_resumen_conacyt_greenpeace_2016)
# encuentra_columnas(df_resumen_conacyt_greenpeace_2016, "Abundancia")

# Revisando cuántos catálogos tienen cada columna, para ver si hay que renombrar
# o no.
# crear_resumen_columnas_df(lista_catalogos) %>%
#   select(-nombre_df) %>%
#   gather("variable", "valor") %>%
#   group_by(variable) %>%
#   summarise(
#     n = sum(valor)
#   ) %>%
#   arrange(variable) %>%
#   View()
# Parece que todo bien

######################################################################################
# Arreglando nombres de columnas que difieren entre data frames (en la lista general)
######################################################################################

lista_datos_columnas_homologadas <- lista_datos_crudos_conacyt_greenpeace_2016 %>%
  renombra_columna("101a110cm", "tamanio_101cm_110cm") %>%
  renombra_columna("111a120cm", "tamanio_111cm_120cm") %>%
  renombra_columna("11a20cm     ll", "tamanio_11cm_20cm") %>%
  renombra_columna("191a200cm", "tamanio_191cm_200cm") %>%
  renombra_columna("21a30cm", "tamanio_21cm_30cm") %>%
  renombra_columna("31a40cm", "tamanio_31cm_40cm") %>%
  renombra_columna("41a50cm", "tamanio_41cm_50cm") %>%
  renombra_columna("51a60cm", "tamanio_51cm_60cm") %>%
  renombra_columna("61a70cm", "tamanio_61cm_70cm") %>%
  renombra_columna("6a10cm", "tamanio_6cm_10cm") %>%
  renombra_columna("71a80cm", "tamanio_71cm_80cm") %>%
  renombra_columna("81a90cm", "tamanio_81cm_90cm") %>%
  renombra_columna("91a100cm", "tamanio_91cm_100cm") %>%
  renombra_columna("Ancho_del_cuadrante_m", "ancho_cuadrante_m") %>%
  renombra_columna("Ancho_del_transecto_metros", "ancho_transecto_m") %>%
  renombra_columna("Anio_de_muestreo", "anio_muestreo") %>%
  renombra_columna("Anio_De_Publicacion", "anio_publicacion") %>%
  renombra_columna("Autor_Administrador_Del_Proyecto", "autor_administrador_proyecto") %>%
  renombra_columna("Blaqueamieto", "blanqueamiento") %>%
  renombra_columna("Depredación", "depredacion") %>%
  renombra_columna("Día", "dia") %>%
  renombra_columna("Dia", "dia") %>%
  renombra_columna("Fecha_de_inicio", "anio_inicio_proyecto") %>%
  renombra_columna("Fecha_de_termino", "anio_termino_proyecto") %>%
  renombra_columna("Longitud_del_cuadrante_m", "longitud_cuadrante_m") %>%
  renombra_columna("Longitud_del_transecto_metro", "longitud_transecto_m") %>%
  renombra_columna("menores_a_5m", "tamanio_0cm_5cm") %>%
  renombra_columna("Metodo_de_Seleccion_de_Sitios", "metodo_seleccion_sitios") %>%
  renombra_columna("Mortalidad_Transición", "mortalidad_transicion") %>%
  renombra_columna("Muestreo_completo", "muestreo_completo") %>%
  renombra_columna("Muestrreo_completo", "muestreo_completo") %>%
  renombra_columna("Nivel_de_agregacion_de_datos", "nivel_agregacion_datos") %>%
  renombra_columna("Nombre_del_sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_Sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_Proyecto", "nombre_proyecto") %>%
  renombra_columna("Numero_de_sitios_agregados", "numero_sitios_agregados") %>%
  renombra_columna("Profundidad_final", "profundidad_final_m") %>%
  renombra_columna("Profundidad_inicial", "profundidad_inicial_m") %>%
  renombra_columna("Profundidad_media_m", "profundidad_media_m") %>%
  renombra_columna("Profundidad_media_metros", "profundidad_media_m") %>%
  renombra_columna("Region_del_arrecife_HR", "region_healthy_reefs") %>%
  renombra_columna("Tamanio_de_cadena_metros", "tamanio_cadena_m") %>%
  renombra_columna("Temperatura_celsius", "temperatura_c") %>% #importante
  renombra_columna("Temperatura_en _Celcius", "temperatura_c") %>% #importante
  renombra_columna("Unidades_de_profundidad", "unidades_profundidad") %>%
  renombra_columnas_minusculas()

# Revisando que no haya duplicados en los nombres de las columnas dentro de cada
# Excel, causados, por ejemplo, por renombrar una columna a un nombre anteriormente
# usado
ldply(lista_datos_columnas_homologadas, function(df){
  numero_columnas_nombres_duplicados <- colnames(df) %>%
    duplicated() %>%
    sum()
  return(numero_columnas_nombres_duplicados)
})
# Perfecto!

###############################################################
# Revision de valores en catálogos:
###############################################################

# En esta sección se revisará que los campos asociados a un catálogo de cada
# data frame en "lista_datos_columnas_homologadas" efectivamente tengan todos sus
# valores tomados de dicho catálogo. En otro caso, se imprimirá la información
# necesaria para corregirlo.

# Revisando las columnas en cada data frame de las listas de tablas y catálogos:
names(lista_datos_columnas_homologadas)
resumen_df <- crear_resumen_columnas_df(lista_datos_columnas_homologadas)
glimpse(resumen_df)
encuentra_columnas(resumen_df, "agregacion")

resumen_catalogos <- crear_resumen_columnas_df(lista_catalogos)
View(resumen_catalogos)

# Generando la relación de columnas en catálogo:
relacion_columnas_catalogo <- c(
  ".tema" = "catalogos_proyecto__proposito.categoria", # propósito
  ".pais" = "catalogos_muestra_sitio__pais.categoria",
  ".region_healthy_reefs" = "catalogos_muestra_sitio__region_healthy_reefs.categoria",
  ".tipo_arrecife" = "catalogos_muestra_sitio__tipo_arrecife.categoria",
  ".tipo_arrecife" = "catalogos_muestra_sitio__tipo_arrecife.categoria",
  ".subtipo_arrecife" = "catalogos_muestra_sitio__subtipo_arrecife.categoria",
  ".zona_arrecife" = "catalogos_muestra_sitio__zona_arrecifal.categoria",
  ".anp" = "catalogos_muestra_sitio__nombre_area_natural_protegida.categoria",
  ".metodo_seleccion_sitios" = "catalogos_muestra_sitio__metodo_seleccion.categoria",
  ".protocolo" = "catalogos_muestra_sitio__metodologia.categoria",
  
  "conacyt_greenpeace_2016_bentos_desagregados_v3.metodo" =
    "catalogos_muestra_transecto_bentos_info__metodo_muestreo.categoria",
  "conacyt_greenpeace_2016_bentos_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_bentos_info__nivel_agregacion_datos.categoria",
  "conacyt_greenpeace_2016_bentos_desagregados_v3.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo",
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.protocolo_utilizado" =
    "catalogos_muestra_transecto_corales_info__metodo_muestreo.categoria",
  "conacyt_greenpeace_2016_corales_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_corales_info__nivel_agregacion_datos.categoria",
  "conacyt_greenpeace_2016_corales_desagregados_v3.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo", # Falta crear el catálogo de corales
  "conacyt_greenpeace_2016_corales_desagregados_v3.depredacion" =
    "catalogos_muestra_transecto_corales_observacion__depredacion.categoria",
  "conacyt_greenpeace_2016_corales_desagregados_v3.lesiones" =
    "catalogos_muestra_transecto_corales_observacion__lesion.categoria",
  "conacyt_greenpeace_2016_corales_desagregados_v3.enfermedades" =
    "catalogos_muestra_transecto_corales_observacion__enfermedades.codigo",
  "conacyt_greenpeace_2016_corales_desagregados_v3.sobrecrecimiento" =
    "catalogos_muestra_transecto_corales_observacion__sobrecrecimiento.codigo",
  "conacyt_greenpeace_2016_corales_desagregados_v3.blanqueamiento" =
    "catalogos_muestra_transecto_corales_observacion__tipo_blanqueamiento.categoria",
  # Falta campo "criterio_seleccion_colonias".
  
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.metodo" =
    "catalogos_muestra_transecto_invertebrados_info__metodo_muestreo.categoria",
  "conacyt_greenpeace_2016_invertebrados_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_invertebrados_info__nivel_agregacion_datos.categoria",
  
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.metodo" =
    "catalogos_muestra_transecto_peces_info__metodo_muestreo.categoria",
  "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3.nivel_agregacion_datos" =
    "catalogos_muestra_transecto_peces_info__nivel_agregacion_datos.categoria",
  # Falta el catálogo de nombres científicos de peces y el de "peces_muestreados"
  
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.nivel_agregacion_datos" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__nivel_agregacion_datos.categoria",
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.sustrato" =
    "catalogos_muestra_subcuadrante_de_transecto_reclutas_info__sustrato.codigo",
  "conacyt_greenpeace_2016_reclutas_desagregados_v3.codigo" =
    "catalogos_muestra_transecto_bentos_observacion__codigo.codigo" # Falta crear el catálogo de corales
)

# Catálogos por agregar:
# 1. Falta catálogo de "datum"
# 2. Falta catálogo de "peces_muestreados

valores_revision_esme_catalogos <- revisa_columnas_catalogos(
  lista_datos_columnas_homologadas, lista_catalogos, relacion_columnas_catalogo) %>%
  group_by(tabla, campo, catalogo, valor) %>%
  tally()
# write_csv(valores_revision_esme_catalogos, "../productos/v3/valores_revision_esme_catalogos.csv")
  
## 2. Creando una tabla con la información de todos los Exceles
# Para crear las tablas, primero se unirán los datos de todos los exceles
# (agregando columnas según se necesite), y luego se separarán esos datos en
# tablas

# Revisiones de la tabla siguiente:
# datos_globales_conacyt_greenpeace$anio_inicio_proyecto %>% table(useNA = "always")

#!!! = precaución con la generalidad a la hora de integrar más Exceles.
datos_globales_conacyt_greenpeace <- Reduce(rbind.fill, lista_datos_columnas_homologadas_conacyt_greenpeace) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  elimina_columnas_vacias() %>%
  ## Arreglando valores de las columnas
  
  # Project
  cambia_valor_columna("titulo",
    paste0("Evaluación de la efectividadde las Áreas Marinas Protegidas en los ",
      "arrecifales del Caribe Mexicano"),
      paste0("Evaluación de la efectividad de las Áreas Marinas Protegidas ",
        "en los sistemas arrecifales del Caribe Mexicano")
  ) %>%
  # Arreglando el valor de "documento" para los datos de "conacyt_greenpeace"
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace')",
    "documento", "datos de campo"
  ) %>%
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace') & anio_inicio_proyecto == '42713'",
    "anio_inicio_proyecto", "2016"
  ) %>%
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace') & anio_termino_proyecto == '42722'",
    "anio_termino_proyecto", "2016"
  ) %>%
  cambia_valor_columna("metodo_seleccion_sitios", "Estrategic", "Estratégico") %>%
  cambia_valor_columna("metodo_seleccion_sitios", "Estrategico", "Estratégico") %>%
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
  
  # Site_sample
  cambia_valor_columna("pais", "Mexico", "México") %>%
  cambia_valor_columna("anp", "NA", NA) %>%
  
  ############## AQUÍ ME QUEDÉ###############
  mutate(
    # Haciendo columna "dentro de ANP, sabemos que si "anp" es NA
    # quiere decir que no. No se será un NA en "dentro_anp
    dentro_anp = ifelse(is.na(anp), FALSE, TRUE) #!!!
  ) %>%
  
  cambia_valor_columna("region_healthy_reefs",
    "Nothern Quintana Roo",
    "Norte de Quintana Roo") %>%
  cambia_valor_columna("region_healthy_reefs",
    "Central Quintana Roo (Sian Ka´an)",
    "Centro de Quintana Roo (Sian Ka'an)") %>%
  cambia_valor_columna("region_healthy_reefs",
    "Southern Quintana Roo",
    "Sur de Quintana Roo") %>%
  cambia_valor_columna("localidad", "Arrecife Alacranes", "Alacranes") %>%
  cambia_valor_columna("localidad", "Arrecife Triangulos", "Triángulos") %>%
  cambia_valor_columna("localidad", "Triangulos", "Triángulos") %>%
  cambia_valor_columna("localidad", "Cancun", "Cancún") %>%
  cambia_valor_columna("localidad", "EL Placer", "El Placer") %>%
  cambia_valor_columna("localidad", "PUERTO MORELOS", "Puerto Morelos") %>%
  cambia_valor_columna("localidad", "Triangulos", "Triángulos") %>%
  
  # Estandarizando nombres de los sitios
  mutate(
    nombre_sitio = estandariza_strings(nombre_sitio)
    ) %>%
  
  cambia_valor_columna("anio", "2017", "2016") %>% #!!!
  cambia_valor_columna("anio", "2017", "2016") %>% #!!!
  cambia_valor_columna("minutos", "0.37", "0") %>% #!!!
  cambia_valor_columna("protocolo", "Evaluación ANPs", "AGRRA_V5") %>%
  cambia_valor_columna("metodo", NA, "BT") %>% #!!! son los de corales
  cambia_valor_columna("metodo", "CADENA", "Cadena") %>% #!!! son los de corales
  # como la columna nivel_agregacion_datos se tiene que recodificar por completo,
  # y con reglas específicas, se usará un mutate %>%
  mutate(
    nivel_agregacion_datos = case_when( #!!! Checarlo bien, aquí estoy suponiendo
      # que los exceles están por nivel de agregación de datos (tanto espacial
      # como biológico), y que los nombres de las tablas "Benthos_transect_sample_info",
      # etc... en la base de datos brindarán la información sobre el nivel de
      # agregación espacial de los datos. Esta info irá a parar a dichas tablas,
      # para brindar información del nivel de agregación biológica de los mismos.
      archivo_origen == "BENTOS_DESAGREGADOS_V2" ~
        "desagregados: puntos/interceptos",
      archivo_origen == "CORALES_DESAGREGADOS_V2" ~
        "desagregados: colonias",
      archivo_origen == "INVERTEBRADOS_DESAGREGADOS_V2" ~
        "desagregados: observaciones",
      archivo_origen == "PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2" ~
        "agregados por especie y categoría de tamaño",
      archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2" ~
        "desagregados: observaciones",
      # Por triste que sea, los reclutas son observaciones, no incidencias.
      archivo_origen == "RUGOSIDAD_DESAGREGADA_V2" ~
        "desagregados: medida por transecto"
    )
  ) %>%
  
  # Transect_sample. Esme me dijo que los 0's daban igual, entonces decido a
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
  
  # Benthos_transect_sample_info
  cambia_valor_columna("profundidad_inicial_m", "NA", NA) %>%
  cambia_valor_columna("profundidad_final_m", "M", NA) %>%
  cambia_valor_columna("profundidad_media_m", "NA", NA) %>%
  
  # Benthos_transect_sample_point
  # Comparar código de bentos con catálogo, para ver si está bien escrito.
  
  # Coral_transect_sample_info
  
  # Coral_transect_sample_observation
  # Comparar "código" de coral con catálogo, para ver si está bien escrito.
  # Comparar "enfermedades" y "sobrecrecimiento" con catálogos tmb.
  mutate(
    sobrecrecimiento = toupper(sobrecrecimiento)
  ) %>%
  cambia_valor_columna("depredacion", "BITES", "BITE") %>%
  
  
  # Invertebrates_transect_sample_info
  # Comparar "especie" con catálogo para eliminar posibles errores ortográficos.
  # "numero" siempre es 1 y sólo está lleno para "INVERTEBRADOS_DESAGREGADOS_V2"
  
  # Fish_transect_sample_info
  # Ya se revisaron sus columnas al revisar las anteriores "transect_sample_info"
  
  # Fish_transect_sample_count
  # Comparar "codigo" y "especie" con catálogos.
  
  # Complexity_transect_sample_info

  # Complexity_transect_sample_mrm
  # No hay datos en la base.

  # Subquadrat_samples_transect_info
  # Ya se revisaron la mayoría de sus datos con anterioridad

  # Recruit_quadrant_sample_transect_info
  # Comparar sustratos con catálogo
  mutate(
    sustrato = toupper(sustrato)
  ) %>%
  
  # Recruit_quadrant_sample_transect_count
  # En realidad no son cuentas, sino incidencias, por lo que se deben introducir
  # en otra tabla.
  cambia_valor_columna("codigo", "NA", NA) %>%
  
  # Eliminando columnas que pudieron haber quedado vacías al hacer cambios en
  # sus valores
  elimina_columnas_vacias() %>%
  
  # Eliminando columnas superfluas
  select(
    -sitio_autor, # No se usa
    -curp_proyecto, # No se usa
    -nombre_remuestreo, # No se usa
    -unidades_profundidad, #!!! Todo es en m.
    -conteo, #!!! Siempre es 1
    -cobertura, #!!! Columna calculable a partir de las anteriores
    -protocolo_utilizado, #!!! Es redundante con "metodo"
    -numero, #!!! Siempre es 1
    -abundancia, #!!! suma de todas las columnas de abundancia por tamaño de peces
    -numero_cuadrantes, #!!! Siempre es 1
    -s, #!!! Coral sano, hay 8 registros
    -fision, #!!! número de roturas en el coral, hay 11 registros
    -tejido_vivo #!!! hay 6 observaciones
  ) %>%
  
  ## Ya que se terminó la revisión de columnas, lo siguiente es transformar en
  ## numéricas y lógicas las que así sean.
  mutate_numeric(
    "serie",
    "anio_inicio_proyecto",
    "anio_termino_proyecto",
    "latitud",
    "longitud",
    "anio_muestreo",
    "longitud_transecto_m",
    "puntos_o_cm_reales_transecto",
    #"dia", # Me conviene tenerlos como string
    #"mes",
    #"anio",
    #"hora",
    #"minutos",
    "profundidad_inicial_m",
    "profundidad_final_m",
    "profundidad_media_m",
    "temperatura_c",
    "altura_algas_cm",
    "ancho_transecto_m",
    "altura_maxima_cm",
    "d1_max_diam_cm",
    "d2_min_diam_cm",
    "mortalidad_total",
    "mortalidad_reciente",
    "mortalidad_transicion",
    "mortalidad_antigua",
    "porcentaje",
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
    "longitud_cuadrante_m",
    "ancho_cuadrante_m",
    "tamanio_cadena_m",
    "id"
  ) %>%
  
  mutate(
    # Redondeando apropiadamente columnas numéricas que lo necesitan
    profundidad_inicial_m = round(profundidad_inicial_m, 1),
    profundidad_final_m = round(profundidad_final_m, 1),
    profundidad_media_m = round(profundidad_media_m, 2),
    temperatura_c = round(temperatura_c, 2),
    
    # Para esta versión, como las horas no tienen am ni pm,
    # usaré la regla de Esme: todo se hace entre las 7 am y las 6 pm
    hora = case_when( #!!!
      hora == "1" ~ "13",
      hora == "2" ~ "14",
      hora == "3" ~ "15",
      hora == "4" ~ "16",
      hora == "5" ~ "17",
      hora == "6" ~ "18",
      TRUE ~ hora
    ),
    
    # Agregando un 0 antes de meses, días, horas y minutos cuando se requiera:
    mes = ifelse(as.numeric(mes) %in% c(1:9), paste0("0", mes), mes),
    dia = ifelse(as.numeric(dia) %in% c(1:9), paste0("0", dia), dia),
    hora = ifelse(as.numeric(hora) %in% c(0:9), paste0("0", hora), hora),
    minutos = ifelse(as.numeric(minutos) %in% c(0:9), paste0("0", minutos), minutos),
    
    # Creando las fecha_horas de muestreo para cada registro
    fecha_hora = ifelse(
      is.na(anio) | is.na(mes) | is.na (dia) | is.na(hora) | is.na(minutos),
      NA,
      paste0(anio, "-", mes, "-", dia, " ", hora, ":", minutos)
    ),

    # Arreglando los transectos: si protocolo == AGRRA_V5, entonces hay dos tipos de
    # transectos:
    # 1. Bentos, Corales, Reclutas, Complejidad, Invertebrados.
    # 2. Peces.
    #
    # Si protocolo == "Sin Protocolo", estos son:
    # 1. Bentos, Corales, Reclutas, Complejidad
    # 2. Peces, Invertebrados
    # 
    # Cabe destacar que el protocolo está por aspecto, entonces sólo los registros
    # provenientes del archivo "INVERTEBRADOS_DESAGREGADOS_V2" lo contienen.
    # Esto lo arreglarán Esme y Nuria en versiones posteriores del monitoreo.
    
    transecto = case_when(
      archivo_origen == "BENTOS_DESAGREGADOS_V2" ~ paste0(transecto, "_bentos"),
      archivo_origen == "CORALES_DESAGREGADOS_V2" ~ paste0(transecto, "_bentos"),
      archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2" ~ paste0(transecto, "_bentos"),
      archivo_origen == "RUGOSIDAD_DESAGREGADA_V2" ~ paste0(transecto, "_bentos"),
      archivo_origen == "PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2" ~ paste0(transecto, "_peces"),
      archivo_origen == "INVERTEBRADOS_DESAGREGADOS_V2" & protocolo == "AGRRA_V5" ~
        paste0(transecto, "_bentos"),
      archivo_origen == "INVERTEBRADOS_DESAGREGADOS_V2" & protocolo == "Sin Protocolo" ~
        paste0(transecto, "_peces")
    ),
    
    # Agregando la columna de "muestreo_completo": todos los transectos están completos
    # menos uno de bentos y reclutas
    muestreo_transecto_completo = case_when(
      nombre_proyecto == "CONACYT 247104 2016" &
        nombre_sitio == "xm04" &
        transecto == "03_bentos" &
        archivo_origen %in% c(
          "BENTOS_DESAGREGADOS_V2",
          "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2"
        ) ~ FALSE,
      # Lo siguiente hace que sea más general esta parte del código:
      archivo_origen %in% c(
        "BENTOS_DESAGREGADOS_V2",
        "CORALES_DESAGREGADOS_V2",
        "INVERTEBRADOS_DESAGREGADOS_V2",
        "PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2",
        "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2",
        "RUGOSIDAD_DESAGREGADA_V2"
      ) ~ TRUE
    ),
    
    ## Cambiando NA's por 0's en mediciones sobre colonias de coral:
    mortalidad_antigua = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(mortalidad_antigua), 0, mortalidad_antigua),
    mortalidad_reciente = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(mortalidad_reciente), 0, mortalidad_reciente),
    mortalidad_transicion = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(mortalidad_transicion), 0, mortalidad_transicion),
    mortalidad_total = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(mortalidad_total), 0, mortalidad_total),
    blanqueamiento = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(blanqueamiento), "ND", blanqueamiento), # No detectado
    enfermedades = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(enfermedades), "ND", enfermedades),
    sobrecrecimiento = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(sobrecrecimiento), "ND", sobrecrecimiento),
    depredacion = ifelse(archivo_origen == "CORALES_DESAGREGADOS_V2" &
        is.na(depredacion), "ND", depredacion),
    
    # # arreglando mortalidades si suman más de 100. Ésto se corregirá en v3.
    # suma_mortalidades = mortalidad_antigua +
    #   mortalidad_reciente +
    #   mortalidad_transicion +
    #   mortalidad_total,
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
    
    # Creando columnas auxiliares útiles a la hora de generar las tablas
    strings_vacios = "",
    verdadero = TRUE,
    falso = FALSE,
    na_numerico = NA_real_,
    datum = "WGS84"
  ) %>%
  # Calculando una sóla hora para cada muestreo de sitio: !!! Esto puede fallar si
  # los sitios ya tienen nombre único.
  ddply(.(nombre_sitio), function(df){
    fecha_hora_muestreo_sitio <- df %>%
      filter(!is.na(fecha_hora)) %>%
      arrange(fecha_hora) %>%
      # Obteniendo el primer renglón
      slice(1) %>%
      pull(fecha_hora)
    
    resultado <- df %>%
      mutate(
        fecha_hora_muestreo_sitio = fecha_hora_muestreo_sitio
      )
  }) %>%
  
  # Para esta versión, cambiar manualmente el "nombre_sitio" ChankanaabGP a
  # Chankanaab, porque es remuestreo. No se podía cambiar antes de arreglar
  # horas de muestreo, pues se perdía la info del remuestreo.
  cambia_valor_columna("nombre_sitio", "chankanaabgp", "chankanaab") %>%
  
  # Generando una columna puramente informativa con el protocolo utilizado a
  # nivel de muestreo de sitio 
  ddply(.(nombre_sitio, fecha_hora_muestreo_sitio), function(df){
    # Para los datos CONACyT / GreenPeace, si se encuentra un "Sin protocolo" para
    # un muestreo de sitio, es un dato de invertebrado que se obtuvo de muestrear
    # invertebrados en el transecto de peces (lo cuál es adicional a AGRRA_V5)
    protocolo_muestreo_sitio = ifelse(
      "Sin Protocolo" %in% df$protocolo, "AGRRA_V5+", "AGRRA_V5")
    
    resultado <- df %>%
      mutate(
        protocolo_muestreo_sitio = protocolo_muestreo_sitio
      )
    
    return(resultado)
  }) %>% #!!! 
  select(
    # Eliminando columnas auxiliares
    -anio,
    -mes,
    -dia,
    -hora,
    -minutos,
    -fecha_hora,
    -anio_muestreo,
    -protocolo
  )
  
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

# saveRDS(datos_globales, "../productos/datos_globales.RData")

# Comentarios Esme y Nuria:
# 1. Falta localidad del proyecto. Esme la va a poner
# 2. Esme va a revisar los datos con "blanqueamiento" NA, pero con porcentaje y viceversa. RESUELTO
# datos_globales %>% group_by(blanqueamiento, porcentaje) %>% tally() %>% View()
# 3. ¿Por qué los transectos de corales tienen longitudes tan variables?
# Porque a veces no te da tiempo terminar...
# 4. ¿Qué diferencia hay entre "Anio" y "Anio_de_muestreo". Ninguna, usar Anio.
# 5. En "anio_inicio_proyecto" y "anio_termino_proyecto" aún hay datos con formato
# de fecha de Excel (todos los archivos)
# 6. En "conacyt_greenpeace_rugosidad_v3" hay unos que tienen "Anio" 2017, está bien? Error.
# 7. Hay algunos sitios sin información de "area_no_pesca". Será que falta actualizar
# la tabla de "datos_anexos?" No, si no está no tenemos info.
# 8. ¿Qué es fisión y S? S: sana. Dejarla porque en algunos proyectos es importante.
# fisión: colonia rota en fragmentos.
# 9. Igual y convendría poner un campo de texto para explicar el "método de selección
# de sitios".

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

