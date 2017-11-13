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
  renombra_columna("Protocolo_utilizado", "metodo") %>% #!!!
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
#encuentra_columnas(resumen_df, "agregacion")

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
  
  "conacyt_greenpeace_2016_corales_desagregados_v3.metodo" =
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
  
  # Arreglando el valor de "documento" para los datos de "conacyt_greenpeace"
  cambia_valor_columna_condicion(
    "stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016')",
    "documento", "datos de campo"
  ) %>%
  
  # Arreglando diversos campos:
  mutate(
    nombre_proyecto_sin_anio = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "CONACYT 247104",
      nombre_proyecto == "GreenPeace 2016" ~ "GreenPeace",
      TRUE ~ nombre_proyecto
    ),
    
    anio_en_curso = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "2016",
      nombre_proyecto == "GreenPeace 2016" ~ "2016",
      TRUE ~ NA_character_
    ),
    
    anio_inicio_proyecto = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "2016",
      nombre_proyecto == "GreenPeace 2016" ~ "2016",
      TRUE ~ anio_inicio_proyecto
    ),
    
    anio_termino_proyecto = case_when(
      nombre_proyecto == "CONACYT 247104 2016" ~ "2017",
      nombre_proyecto == "GreenPeace 2016" ~ "2016",
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
  ) %>%
  
  # mutate(
    # Invertebrates_transect_sample_info
    # "numero" siempre es 1 para conacyt_greenpeace_2016_invertebrados_desagregados_v3"
    
    # Fish_transect_sample_count
    # Falta comparar "codigo" y "especie" con catálogos.
    
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

saveRDS(datos_globales, "../productos/v3/datos_globales.RData")

# Comentarios Esme y Nuria:
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

