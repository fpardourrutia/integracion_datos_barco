# En este script se genera el DF global de datos limpios, listo para partirse
# en tablas distintas.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

# Leyendo lista de exceles
lista_exceles_cruda <- readRDS("../productos/lista_exceles_cruda.RData")

## 1. Arreglando nombres de columnas que difieren entre DF's:
lista_exceles_columnas_homologadas <- lista_exceles_cruda %>%
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
  renombra_columna("Día", "dia") %>% #importante
  renombra_columna("Dia", "dia") %>% #importante
  renombra_columna("Fecha_de_inicio", "anio_inicio_proyecto") %>%
  renombra_columna("Fecha_de_termino", "anio_termino_proyecto") %>%
  renombra_columna("Longitud_del_cuadrante_m", "longitud_cuadrante_m") %>%
  renombra_columna("Longitud_del_transecto_metro", "longitud_transecto_m") %>%
  renombra_columna("menores_a_5m", "tamanio_0cm_5cm") %>%
  renombra_columna("Metodo_de_Seleccion_de_Sitios", "metodo_seleccion_sitios") %>%
  renombra_columna("Mortalidad_Transición", "mortalidad_transicion") %>%
  renombra_columna("Nivel_de_agregacion_de_datos", "nivel_agregacion_datos") %>%
  renombra_columna("Nombre_del_Arrecife", "nombre_arrecife") %>%
  renombra_columna("Nombre_del_sitio", "nombre_sitio") %>% #importante
  renombra_columna("Nombre_del_Sitio", "nombre_sitio") %>% #importante
  renombra_columna("Nombre_del_Proyecto", "nombre_proyecto") %>%
  renombra_columna("Numero_de_sitios_agregados", "numero_sitios_agregados") %>%
  renombra_columna("Profundidad_final", "profundidad_final_m") %>%
  renombra_columna("Profundidad_inicial", "profundidad_inicial_m") %>%
  renombra_columna("Profundidad_media_m", "profundidad_media_m") %>% #importante
  renombra_columna("Profundidad_media_metros", "profundidad_media_m") %>% #importante
  renombra_columna("Region_del_arrecife_HR", "region_healthy_reefs") %>%
  renombra_columna("Tamanio_de_cadena_metros", "tamanio_cadena_m") %>%
  renombra_columna("Temperatura_celsius", "temperatura_c") %>% #importante
  renombra_columna("Temperatura_en _Celcius", "temperatura_c") %>% #importante
  renombra_columna("Tipo_de_Arrecife", "tipo_arrecife") %>%
  renombra_columna("Unidades_de_profundidad", "unidades_profundidad") %>%
  renombra_columnas_minusculas()

# Revisando que no haya duplicados en los nombres de las columnas dentro de cada
# Excel, causados, por ejemplo, por renombrar una columna a un nombre anteriormente
# usado
ldply(lista_exceles_columnas_homologadas, function(df){
  numero_columnas_nombres_duplicados <- colnames(df) %>%
    duplicated() %>%
    sum()
  return(numero_columnas_nombres_duplicados)
})
# Perfecto!

## 2. Creando una tabla con la información de todos los Exceles
# Para crear las tablas, primero se unirán los datos de todos los exceles
# (agregando columnas según se necesite), y luego se separarán esos datos en
# tablas

#!!! = precaución con la generalidad a la hora de integrar más Exceles.
datos_globales <- Reduce(rbind.fill, lista_exceles_columnas_homologadas) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  elimina_columnas_vacias() %>%
  ## Arreglando valores de las columnas
  
  # Project
  cambia_valor_columna("nombre_proyecto", "NA", "CONACYT 247104 2016") %>% #!!!
  cambia_valor_columna("proposito", "Estrategic", "Evaluación ANPs") %>%
  cambia_valor_columna("tema", NA, "Monitoreo") %>% #!!!
  cambia_valor_columna("anio_inicio_proyecto", "42713", "2016") %>%
  cambia_valor_columna("anio_termino_proyecto", "42722", "2016") %>%
  cambia_valor_columna("anio_termino_proyecto", "NA", "2016") %>% #!!!
  cambia_valor_columna("metodo_seleccion_sitios", "Estrategic", "Estratégico") %>%
  cambia_valor_columna("metodo_seleccion_sitios", "NA", "Estratégico") %>% #!!!
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
  cambia_valor_columna("anio", "2017", "2016") %>% #!!!
  cambia_valor_columna("anio", "2017", "2016") %>% #!!!
  cambia_valor_columna("minutos", "0.37", "0") %>% #!!!
  cambia_valor_columna("protocolo", "Evaluación ANPs", "AGRRA_V5") %>%
  cambia_valor_columna("metodo", NA, "BT") %>% #!!! son los de corales
  cambia_valor_columna("metodo", "CADENA", "Cadena") %>% #!!! son los de corales
  # como la columna nivel_agregacion_datos se tiene que recodificar por completo,
  # y con reglas específicas, se usará un mutate %>%
  mutate(
    nivel_agregacion_datos = case_when(
      archivo_origen == "BENTOS_DESAGREGADOS_V2" ~ "Puntos por transecto",
      archivo_origen == "CORALES_DESAGREGADOS_V2" ~ "Colonias por transecto",
      archivo_origen == "INVERTEBRADOS_DESAGREGADOS_V2" ~ "Observaciones por transecto",
      archivo_origen == "PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2" ~ "Especies por transecto",
      archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2" ~ "Incidencias por cuadrante",
      archivo_origen == "RUGOSIDAD_DESAGREGADA_V2" ~ "Medida por transecto"
    )
  ) %>%
  
  # Transect_sample
  cambia_valor_columna("transecto", "01", "1") %>%
  cambia_valor_columna("transecto", "02", "2") %>%
  cambia_valor_columna("transecto", "03", "3") %>%
  cambia_valor_columna("transecto", "04", "4") %>%
  cambia_valor_columna("transecto", "05", "5") %>%
  cambia_valor_columna("transecto", "06", "6") %>%
  
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
    -unidades_profundidad, #!!! Todo es en m.
    -conteo, #!!! Siempre es 1
    -cobertura, #!!! Columna calculable a partir de las anteriores
    -protocolo_utilizado, #!!! Es redundante con "metodo"
    -numero, #!!! Siempre es 1
    -abundancia, #!!! suma de todas las columnas de abundancia por tamaño de peces
    -numero_cuadrantes #!!! Siempre es 1
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
    "dia",
    "mes",
    "anio",
    "hora",
    "minutos",
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
    "fision",
    "s",
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
  
  # Redondeando apropiadamente columnas numéricas que lo necesitan
  mutate(
    profundidad_inicial_m = round(profundidad_inicial_m, 1),
    profundidad_final_m = round(profundidad_final_m, 1),
    profundidad_media_m = round(profundidad_media_m, 2),
    temperatura_c = round(temperatura_c, 2)
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

# Comentarios:
# falta localidad del proyecto
# Revisar la hora y los minutos, hay faltantes tmb!
# No entiendo la diferencia entre "nombre_sitio" y "nombre_remuestreo"
# "tipo_arrecife", "zona_arrecifal" y "subzona_habitat" siguen muy revueltas!
# falta "inside_non_fishing_area"
# No entiendo si "NA" en "anp" es que no está dentro de ANP o no se sabe.
# Por qué en "protocolo" hay algunos "Sin Protocolo" y otros "AGRRA_V5?
# Nivel de agregación de datos == "Organismo/sustrato" es por punto y corresponde
# a Bentos.
# ¿Qué es la columna "Numero" para "INVERTEBRADOS_DESAGREGADOS_V2"?
# No tenemos cuenta del número de reclutas y corales pequeños en
# "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2"
# Redondear "temperatura_c" , "profundidad_inicial/final_m".
# Me falta la variable "profundidad_sitio", pero mientras voy a usar "profundidad_media_m".
# Estoy quitando los 0's de los nombres de transecto, hay que ver si es cierto.
# Los registros que tienen vacía "longitud_transecto_m" son todos del archivo:
# "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2", por lo que tal vez esta información se
# pueda obtener de otros exceles.
# De la misma manera, los registros que tienen vacía la columna "ancho_transecto_m"
# pertenecen a aspectos donde no es necesario este dato.
# Creo que la columna "Sitio_autor" de "BENTOS_DESAGREGADOS_V2" contiene todos los
# sitios en este estudio y se puede usar de referencia.
# falta la columna de "Muestreo_completo"
# Comparar códigos de bentos con catálogo, para saber si están bien escritos
# Sería bueno tener un protocolo para apuntar nombres de observadores.
# Los registros que tienen NA en clump no perteneces a "CORALES_DESAGREGADOS_V2"
# Los registros de "CORALES_DESAGREGADOS_V2" con NA en "d1_max_diam_cm",
# "d2_min_diam_cm" y "altura_maxima_cm" son básicamente los que tienen
# "clump" == "FRAG".
# hay datos con "blanqueamiento" NA pero con porcentaje y viceversa. ¿Es normal?
# datos_globales %>% group_by(blanqueamiento, porcentaje) %>% tally() %>% View()
# Los números no cuadran en la suma de mortalidades, hay algo que o entiendo.
# También creo que "mortalidad_total" es un campo que usan cuando no saben el tipo
# de mortalidad
# datos_globales %>% filter(archivo_origen == "CORALES_DESAGREGADOS_V2") %>%
# group_by(mortalidad_antigua, mortalidad_reciente, mortalidad_transición, mortalidad_total)
# %>% tally() %>% View()
# Falta columna de "muestreo_completo" en "Invertebrados".
# Redondear a dos dígitos "tamanio_cadena_m".
# En "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2" hay transectos sin info de longitud
# ("longitud_transecto_m").
# ¿Por qué hay cuadrante con códigos de especie de reclutas repetidos, bajo la
# misma categoría de tamaño (R/SC). Ejemplo:
# datos_globales %>%
# filter(archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2",
# sitio_autor == "Mex-1047_Álvarez-Filip Lorenzo") %>% elimina_columnas_vacias()
# %>% View()
# Por ahora, para formar la tabla haré un group_by "sitio_autor", "transecto",
# "cuadrante", "codigo", "categoria_tamanio".
# El nombre del cuadrante corresponde a la columna "cuadrante".
# ¿Qué es el 0 en "sustrato?
# Faltan "Maximum_recruit_size" y "Maximum_small_coral_size".
# Hay SC que no se sabe de qué especie son y entonces tienen en "codigo" NA.
# En Corales, ¿qué es "fisión" y "S"?