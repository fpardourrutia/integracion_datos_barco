# En este script se crea un data frame homologado con la información en
# "lista_tablas_columnas_homologadas".

# EL procedimiento seguido es el siguiente:
# 1. Agregar los data frames en "lista_tablas_columnas_homologadas" en un sólo
# data frame.
# 2. Arreglar campos con valores incorrectos

# Se supone que los datos están perfectos a la hora de hacer este data frame, por
# ejemplo, que si el sitio no está dentro de un ANP hay un NA y no una palabra "NA".
# Toda imperfección en la base de datos de Excel la tendrá que arreglar Esme.

# En scripts subsecuentes, este data frame se segmentará apropiadamente en tablas.

# Cargando archivo de configuración y funciones auxiliares
library("readr")
source("config.R")
source("funciones_auxiliares.R")

###############################################################
# Leyendo la lista de tablas con las columnas homologadas:
###############################################################

lista_tablas_columnas_homologadas <- readRDS(
  paste0(ruta_salidas_1_homologar_columnas, "/lista_tablas_columnas_homologadas.RData"))

###############################################################
# Creando la tabla con la información integrada:
###############################################################

datos_globales_crudos <- Reduce(rbind.fill, lista_tablas_columnas_homologadas) %>%
  mutate(
    id = 1:nrow(.)
  )

###############################################################
# Revisando la tabla con la información integrada:
###############################################################

names(lista_tablas_columnas_homologadas)
crear_resumen_columnas_df(lista_tablas_columnas_homologadas) %>%
  glimpse()

revision_valores <- revisa_valores(datos_globales_crudos)
names(revision_valores)

# Definiendo una función para revisar qué archivos contienen determinada columnas
# Inputs:
# datos: data frame que contiene "columna" y otra columna llamada "archivo_origen"
# nombre_columna: nombre de la columna a revisar
# Outputs:
# vector con los nombres de los archivos que la contienen
obtiene_archivos_columna <- function(datos, nombre_columna){
  resultado <- datos %>%
    filter_(paste0("!is.na(", nombre_columna, ")")) %>%
    pull(archivo_origen) %>%
    unique()
  return(resultado)
}

################################################################################

### Muestreo ###
names(revision_valores[["nombre_proyecto"]]) # Nombre del muestreo. No debe haber "NA"
names(revision_valores[["proposito"]]) # Descripción del proyecto
names(revision_valores[["tema"]]) # Propósito del proyecto. Catálogo
names(revision_valores[["localidad_proyecto"]]) # Área de estudio
names(revision_valores[["institucion"]]) # Organización
names(revision_valores[["suborganizacion"]])
names(revision_valores[["autor_administrador_proyecto"]]) # Encargado
names(revision_valores[["contacto"]])
names(revision_valores[["cita"]]) # Referencia
names(revision_valores[["titulo"]]) # Nombre del proyecto
names(revision_valores[["anio_inicio_proyecto"]]) # Numérico
names(revision_valores[["anio_termino_proyecto"]]) # Numérico

### Muestra_sitio ###
names(revision_valores[["identificador_muestreo_sitio"]]) # Sólo presente en datos del CONACyT/GreenPeace
names(revision_valores[["identificador_sitio"]]) # Sólo presente en datos históricos y del 2017
names(revision_valores[["nombre_sitio"]])
names(revision_valores[["nombre_original"]])
# Si este campo está lleno, entonces el nombre original del sitio es éste, sino
# copiarle "nombre_sitio".
names(revision_valores[["anio"]]) # Numérico
names(revision_valores[["mes"]]) # Numérico
names(revision_valores[["dia"]]) # Numérico
names(revision_valores[["hora"]]) # Natural. Esme debe revisar el formato 24h #####
names(revision_valores[["minutos"]]) # Entero #####
names(revision_valores[["pais"]])
names(revision_valores[["region_healthy_reefs"]]) # Catálogo
names(revision_valores[["localidad"]]) # Eliminar "NA"
names(revision_valores[["tipo_arrecife"]]) # Ya se revisó al comparar con catálogos, eliminar "NA" #####
names(revision_valores[["subtipo_arrecife"]]) # Catálogo
names(revision_valores[["zona_arrecife"]]) # Catálogo
names(revision_valores[["anp"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["area_no_pesca"]]) # Booleano
names(revision_valores[["latitud"]]) # Numérico
names(revision_valores[["longitud"]]) # Numérico
names(revision_valores[["metodo_seleccion_sitios"]]) # Catálogo
names(revision_valores[["protocolo"]]) # Catálogo. Metodología
names(revision_valores[["temperatura_c"]]) # Numérico
names(revision_valores[["profundidad_media_m_sitio"]]) # Numérico
names(revision_valores[["notas"]])
names(revision_valores[["observaciones"]])
# Notas son observaciones, sólo que en algunos Exceles se usó un nombre y en otros otro.
# Se agregarán por facilidad a los comentarios de "Muestra_sitio".

### Muestra_sitio_bentos_info ###
names(revision_valores[["metodo"]]) # Catálogo
names(revision_valores[["nivel_agregacion_datos"]]) # Catálogo
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["puntos_o_cm_reales_transecto"]]) # Numérico
names(revision_valores[["muestreo_completo"]]) # Booleano

### Muestra_sitio_bentos_porcentaje ###
names(revision_valores[["codigo"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["cobertura"]]) # No debe haber negativos ni letras. Suman 100.

### Muestra_transecto ###
names(revision_valores[["transecto"]]) # Nombre del transecto
names(revision_valores[["transecto_fijo"]]) # Booleano
names(revision_valores[["profundidad_inicial_m"]]) # Numérico
names(revision_valores[["profundidad_final_m"]]) # Numérico
names(revision_valores[["longitud_cuadrante_m"]]) # Numérico
names(revision_valores[["ancho_cuadrante_m"]]) # Numérico

################################################################################

### Muestra_transecto_bentos_info ###
names(revision_valores[["metodo"]]) # Catálogo
names(revision_valores[["nivel_agregacion_datos"]]) # Catálogo
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["longitud_transecto_m"]]) # Numérico
names(revision_valores[["puntos_o_cm_reales_transecto"]]) # Numérico
names(revision_valores[["muestreo_completo"]]) # Booleano

### Muestra_transecto_bentos_punto ###
names(revision_valores[["codigo"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["altura_algas_cm"]]) # Numérico

### Muestra_transecto_bentos_linea ### (No hay datos)

### Muestra_transecto_bentos_porcentaje ###
names(revision_valores[["codigo"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["cobertura"]]) # No debe haber negativos ni letras. Suman 100.

################################################################################

### Muestra_transecto_corales_info ###
names(revision_valores[["metodo"]]) # Catálogo
names(revision_valores[["nivel_agregacion_datos"]]) # Catálogo
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["longitud_transecto_m"]]) # Numérico
names(revision_valores[["ancho_transecto_m"]]) # Numérico
# Tamaño mínimo de la colonia: siempre 4cm.
names(revision_valores[["muestreo_completo"]]) # Booleano

### Muestra_transecto_corales_observacion ###
names(revision_valores[["codigo"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["clump"]]) # Natural o "C", "F", NA. Suponer que siempre se mide
names(revision_valores[["d1_max_diam_cm"]]) # Numérico
names(revision_valores[["d2_min_diam_cm"]]) # Numérico
names(revision_valores[["altura_maxima_cm"]]) # Numérico
names(revision_valores[["blanqueamiento"]]) # Catálogo. Suponer que siempre se mide
names(revision_valores[["porcentaje"]]) # Numérico. Suponer que siempre se mide
names(revision_valores[["mortalidad_antigua"]]) # Numérico
names(revision_valores[["mortalidad_reciente"]]) # Numérico
names(revision_valores[["mortalidad_total"]]) # Numérico
names(revision_valores[["mortalidad_transicion"]]) # Numérico
# Las mortalidades deben sumár máximo 100. Suponer que siempre se miden
names(revision_valores[["enfermedades"]]) # Catálogo. Suponer que siempre se mide
names(revision_valores[["sobrecrecimiento"]]) # Catálogo
names(revision_valores[["sobrecrecimiento__1"]]) # Catálogo
names(revision_valores[["depredacion"]]) # Catálogo. Suponer que siempre se mide
names(revision_valores[["lesiones"]]) # Catálogo. Suponer que siempre se mide

################################################################################

### Muestra_transecto_peces_info ###
names(revision_valores[["metodo"]]) # Catálogo
names(revision_valores[["nivel_agregacion_datos"]]) # Catálogo
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["longitud_transecto_m"]]) # Numérico
names(revision_valores[["ancho_transecto_m"]]) # Numérico
names(revision_valores[["peces_muestreados"]]) # Catálogo
names(revision_valores[["muestreo_completo"]]) # Booleano

### Muestra_transecto_peces_cuenta ###
names(revision_valores[["especie"]])
# Catálogo. Eliminar de catálogos duplicados menos frecuentes.
# Peces: cantidad de peces por especie (fila) y columna (tamaño).
names(revision_valores[["tamanio_0cm_5cm"]]) # Numérico
names(revision_valores[["tamanio_101cm_110cm"]]) # Numérico
names(revision_valores[["tamanio_101cm_9999cm"]]) # Numérico
names(revision_valores[["tamanio_111cm_120cm"]]) # Numérico
names(revision_valores[["tamanio_11cm_20cm"]]) # Numérico
names(revision_valores[["tamanio_191cm_200cm"]]) # Numérico
names(revision_valores[["tamanio_21cm_30cm"]]) # Numérico
names(revision_valores[["tamanio_31cm_40cm"]]) # Numérico
names(revision_valores[["tamanio_41cm_50cm"]]) # Numérico
names(revision_valores[["tamanio_51cm_60cm"]]) # Numérico
names(revision_valores[["tamanio_61cm_70cm"]]) # Numérico
names(revision_valores[["tamanio_6cm_10cm"]]) # Numérico
names(revision_valores[["tamanio_71cm_80cm"]]) # Numérico
names(revision_valores[["tamanio_81cm_90cm"]]) # Numérico
names(revision_valores[["tamanio_91cm_100cm"]]) # Numérico

################################################################################

### Muestra_transecto_invertebrados_info ###
names(revision_valores[["especie"]]) # Hay que procesarla antes de introducirla
names(revision_valores[["metodo"]]) # Catálogo
names(revision_valores[["nivel_agregacion_datos"]]) # Catálogo
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["longitud_transecto_m"]]) # Numérico
names(revision_valores[["ancho_transecto_m"]]) # Numérico
names(revision_valores[["muestreo_completo"]]) # Booleano

### Muestra_transecto_invertebrados_cuenta ###
names(revision_valores[["especie"]]) # Preguntarle a Esme si están en catálogos
names(revision_valores[["conteo"]]) # Numérico. También las tablas de bentos agregados lo contienen

################################################################################

### Muestra_transecto_complejidad_info ###
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["longitud_transecto_m"]]) # Numérico
names(revision_valores[["tamanio_cadena_m"]]) # Numérico

### Muestra_subcuadrante_de_transecto_complejidad_info ###
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["cuadrante"]]) # Eliminar "NA"
names(revision_valores[["relieve"]]) # Numérico (complejidad por máximo relieve)

################################################################################

### Muestra_subcuadrante_de_transecto_reclutas_info ###
names(revision_valores[["cuadrante"]]) # Eliminar "NA"
names(revision_valores[["nivel_agregacion_datos"]]) # Catálogo
names(revision_valores[["observador"]]) # Homologarlos
names(revision_valores[["sustrato"]]) # Catálogo

### Muestra_subcuadrante_de_transecto_reclutas_observacion ###
names(revision_valores[["codigo"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["tamanio_cm"]]) # Numérico
# ¿Por qué hay valores de esta columna para la tabla
# "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla"?

### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
names(revision_valores[["codigo"]]) # Catálogo. Eliminar "NA"
names(revision_valores[["categoria_tamanio"]]) # Sólo debe haber "No considerada", "R", "SC" y NA #####
names(revision_valores[["n"]]) # Natural. Número de reclutas agregados por código y categoría de tamaño (si es el caso).

################################################################################

### Eliminar ###

# names(revision_valores[["abundancia"]])
# names(revision_valores[["anio_muestreo"]])
# names(revision_valores[["anio_publicacion"]])
# names(revision_valores[["anp_redundante"]])
# names(revision_valores[["curp_proyecto"]])
# names(revision_valores[["longitud_teorica_m_bentos"]])
# names(revision_valores[["longitud_teorica_m_corales"]])
# names(revision_valores[["longitud_teorica_m_invertebrados_agrra_v5"]])
# names(revision_valores[["longitud_teorica_m_invertebrados_otros"]])
# names(revision_valores[["longitud_teorica_m_peces"]])
# names(revision_valores[["necrosis"]])
# names(revision_valores[["neoplasia"]])
# names(revision_valores[["nombre_no_remuestreo"]]
# names(revision_valores[["nombre_original"]]
# names(revision_valores[["nombre_remuestreo"]]
# names(revision_valores[["numero_sitios_agregados"]])
# names(revision_valores[["profundidad_media_m"]])
# names(revision_valores[["profundidad_media_m_sitio_redundante"]])
# names(revision_valores[["proyecto_aux"]])
# names(revision_valores[["region_ask"]])
# names(revision_valores[["region_healthy_reefs_redundante"]])
# names(revision_valores[["region_jordan"]])
# names(revision_valores[["s"]])
# names(revision_valores[["sitio_autor"]])
# names(revision_valores[["subtipo_arrecife_redundante"]])
# names(revision_valores[["superficie_cm2"]])
# names(revision_valores[["unidades_profundidad"]])
# names(revision_valores[["X__1"]])
# names(revision_valores[["X__2"]])
# names(revision_valores[["zona_arrecife_redundante"]])
# names(revision_valores[["tejido_vivo"]])
# names(revision_valores[["tipo_arrecife_redundante"]])+
# names(revision_valores[["subzona_habitat"]])

### Otros ###
# names(revision_valores[["id"]])
# names(revision_valores[["archivo_origen"]])
# names(revision_valores[["serie"]]) 

################################################################################

### Notas acerca de las columnas revisadas ###

# archivo_origen                                    metodo
# 1                          conacyt_greenpeace_2016_bentos_desagregados_v3         PIT
# 2     historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura         CPF
# 3            historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados     PIT
# 4 historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura         LIT
# 5 historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura         PIT
# 6 historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura         Otro
# 7                  historicos_y_2017_transecto_bentos_desagregados_pit_lit        PIT
# Notar que bentos desagregados sólo son PIT, y los bentos agregados por sitio son CPF.

# Hay que homologar "observaciones" y "notas", ya que son columnas presentes en
# todos los Exceles, pero sólo se utiliza una.

# Hay que revisar "sobrecrecimiento" y "sobrecrecimiento__1", ya que a veces
# "sobrecrecimiento__1" está llena mientras "sobrecrecimiento" está vacía.

# Hay que recalcular "documento" --> "fuente", puesto que lo queremos para diferenciar
# datos que provienen de una base de datos (como Healthy Reefs) de los que no

# Por ahora, "rugosidad.longitud_lineal_fija" será marcada como TRUE.

################################################################################

# Revisando "NA"'s en las columnas

datos_globales_crudos %>%
  gather(variable, valor, -archivo_origen) %>%
  filter(valor == "NA") %>%
  unique() %>%
  View()
  
################################################################################

###############################################################
# Creando la tabla con información selecta para la integración
###############################################################

datos_globales_columnas_selectas <- datos_globales_crudos %>%
  
  ## Mutate numeric eliminará por completo cualquier valor no numérico en las
  ## columnas seleccionadas
  mutate_numeric(
    
    ### Muestreo ###
    "anio_inicio_proyecto",
    "anio_termino_proyecto",
    
    ### Muestra_sitio ###
    "anio",
    "mes",
    "dia",
    "hora",
    "minutos",
    "latitud",
    "longitud",
    "temperatura_c",
    "profundidad_media_m_sitio",
    
    ### Muestra_sitio_bentos_info ###
    "puntos_o_cm_reales_transecto",
    
    ### Muestra_sitio_bentos_porcentaje ###
    "cobertura",
    
    ### Muestra_transecto ###
    "profundidad_inicial_m",
    "profundidad_final_m",
    "longitud_cuadrante_m",
    "ancho_cuadrante_m",
    
    ### Muestra_transecto_bentos_info ###
    "longitud_transecto_m",
    # "puntos_o_cm_reales_transecto",
    
    ### Muestra_transecto_bentos_punto ###
    "altura_algas_cm",
    
    ### Muestra_transecto_bentos_porcentaje ###
    # "cobertura",
    
    ### Muestra_transecto_corales_info ###
    # "longitud_transecto_m",
    "ancho_transecto_m",
    
    ### Muestra_transecto_corales_observacion ###
    "d1_max_diam_cm",
    "d2_min_diam_cm",
    "altura_maxima_cm",
    "porcentaje",
    "mortalidad_antigua",
    "mortalidad_reciente",
    "mortalidad_total",
    "mortalidad_transicion",
    
    ### Muestra_transecto_peces_info ###
    # "longitud_transecto_m",
    # "ancho_transecto_m",
    
    ### Muestra_transecto_peces_cuenta ###
    "tamanio_0cm_5cm",
    "tamanio_101cm_110cm",
    "tamanio_101cm_9999cm",
    "tamanio_111cm_120cm",
    "tamanio_11cm_20cm",
    "tamanio_191cm_200cm",
    "tamanio_21cm_30cm",
    "tamanio_31cm_40cm",
    "tamanio_41cm_50cm",
    "tamanio_51cm_60cm",
    "tamanio_61cm_70cm",
    "tamanio_6cm_10cm",
    "tamanio_71cm_80cm",
    "tamanio_81cm_90cm",
    "tamanio_91cm_100cm",
    
    ### Muestra_transecto_invertebrados_info ###
    # "longitud_transecto_m",
    # "ancho_transecto_m",
    
    ### Muestra_transecto_invertebrados_cuenta ###
    "conteo",
    
    ### Muestra_transecto_complejidad_info ###
    # "longitud_transecto_m",
    "tamanio_cadena_m",
    
    ### Muestra_subcuadrante_de_transecto_complejidad_info ###
    "relieve",
    
    ### Muestra_subcuadrante_de_transecto_reclutas_info ###

    ### Muestra_subcuadrante_de_transecto_reclutas_observacion ###
    "tamanio_cm",

    ### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
    "n",
    
    warnings = FALSE
  ) %>%
  
  ## Reestructurando columnas booleanas:
  mutate(
    area_no_pesca = case_when(
      tolower(area_no_pesca) == "si" ~ TRUE,
      tolower(area_no_pesca) == "no" ~ FALSE,
      TRUE ~ NA
    ),
    
    muestreo_completo = case_when(
      tolower(muestreo_completo) == "si" ~ TRUE,
      tolower(muestreo_completo) == "no" ~ FALSE,
      TRUE ~ NA
    ),
    
    transecto_fijo = case_when(
      tolower(transecto_fijo) == "si" ~ TRUE,
      tolower(transecto_fijo) == "no" ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  
  ## Quitando strings no deseados (como "NA")
  mutate(
    categoria_tamanio = case_when(
      tolower(categoria_tamanio) == "no considerada" ~ "No considerada",
      tolower(categoria_tamanio) == "r" ~ "R",
      tolower(categoria_tamanio) == "sc" ~ "SC",
      TRUE ~ NA_character_
    ),
    clump = case_when(
      stri_detect_fixed(tolower(clump), "f") ~ "FRAG",
      stri_detect_fixed(tolower(clump), "c") ~ "CLUMP",
      stri_detect_regex(clump, "[0-9]") ~ clump,
      TRUE ~ NA_character_
    )
  ) %>%
  
  ## Arreglando columnas más complejas:
  mutate(
    # Todas las tablas tienen ambas columnas ("notas" y "observaciones"), pero
    # sólo ocupan una a la vez
    comentarios = ifelse(!is.na(notas), notas, observaciones),
    
    # A veces está lleno el campo "sobrecrecimiento__1" y vacío el de "sobrecrecimiento"
    sobrecrecimiento = ifelse(is.na(sobrecrecimiento), sobrecrecimiento__1, sobrecrecimiento),
    sobrecrecimiento__1 = ifelse(sobrecrecimiento == sobrecrecimiento__1, NA, sobrecrecimiento__1)
  ) %>%
  
  select(
    -notas,
    -observaciones,
    sobrecrecimiento_1 = sobrecrecimiento__1
  ) %>%
  
  ## Eliminando columnas innecesarias:
  select(
    -abundancia,
    -anio_muestreo,
    -anio_publicacion,
    -anp_redundante,
    -curp_proyecto,
    -documento,
    -fuente,
    -longitud_teorica_m_bentos,
    -longitud_teorica_m_corales,
    -longitud_teorica_m_invertebrados_agrra_v5,
    -longitud_teorica_m_invertebrados_otros,
    -longitud_teorica_m_peces,
    -nombre_no_remuestreo,
    -nombre_original,
    -nombre_remuestreo,
    -numero_sitios_agregados,
    -profundidad_media_m,
    -profundidad_media_m_sitio_redundante,
    -proyecto_aux,
    -region_ask,
    -region_healthy_reefs_redundante,
    -region_jordan,
    -s,
    -sitio_autor,
    -subtipo_arrecife_redundante,
    -superficie_cm2,
    -tejido_vivo,
    -tipo_arrecife_redundante,
    -unidades_profundidad,
    -X__1,
    -X__2,
    -zona_arrecife_redundante
  )

###############################################################
# Creando la tabla con la información lista para ser integrada
###############################################################

datos_globales <- datos_globales_columnas_selectas %>%

  ### Muestreo ####

  mutate(
    
    # En la columna "nombre_proyecto", se codificó al final una letra A/B/C para
    # distinguir entre remuestreos del mismo sitio, dentro del mismo proyecto. Esta
    # información la extraeré en una columna nueva.
    # Supuesto: ningún proyecto termina con "A", "B" o "C", a menos que sea indicativo 
    # de un remuestreo de sitio dentro del mismo proyecto.
    Muestreo.nombre_si_A = stri_match_first(nombre_proyecto, regex = "(.*)A$")[,2],
    Muestreo.nombre_si_B = stri_match_first(nombre_proyecto, regex = "(.*)B$")[,2],
    Muestreo.nombre_si_C = stri_match_first(nombre_proyecto, regex = "(.*)C$")[,2],
    Muestreo.nombre = ifelse(!is.na(Muestreo.nombre_si_A), Muestreo.nombre_si_A,
      ifelse(!is.na(Muestreo.nombre_si_B), Muestreo.nombre_si_B,
        ifelse(!is.na(Muestreo.nombre_si_C), Muestreo.nombre_si_C,
          nombre_proyecto
    ))),
    
    # Variable que es A o B si el sitio fue remuestreado en el mismo muestreo,
    # y NA e.o.c
    remuestreo_en_mismo_muestreo = ifelse(!is.na(Muestreo.nombre_si_A), "A",
      ifelse(!is.na(Muestreo.nombre_si_B), "B",
        ifelse(!is.na(Muestreo.nombre_si_C), "C", NA
    )))
  ) %>%
  
  select(
    -nombre_proyecto,
    -Muestreo.nombre_si_A,
    -Muestreo.nombre_si_B,
    -Muestreo.nombre_si_C
  ) %>%
  
  rename(
    Muestreo.descripcion = proposito,
    Muestreo.proposito = tema,
    Muestreo.area_estudio = localidad_proyecto,
    Muestreo.organizacion = institucion,
    Muestreo.suborganizacion = suborganizacion,
    Muestreo.encargado = autor_administrador_proyecto,
    Muestreo.contacto = contacto,
    Muestreo.referencia = cita
  ) %>%
  
  mutate(
    Muestreo.comentarios = NA_character_
  ) %>%
  
  rename(
    Muestreo.nombre_proyecto = titulo,
    Muestreo.anio_inicio_proyecto = anio_inicio_proyecto,
    Muestreo.anio_termino_proyecto = anio_termino_proyecto
  ) %>%
  
  # mutate(
  #   Muestreo.etapa_revision,
  #   Muestreo.compatibilidad_cliente
  # ) %>%
  
  ### Muestra_sitio ###
  
  rename(
    Muestra_sitio.nombre = nombre_sitio
  ) %>%
  mutate(
    Muestra_sitio.nombre_original = NA_character_
  ) %>%
  mutate(
    # Agregando un 0 antes de meses y días cuando se requiera:
    mes = ifelse(as.numeric(mes) %in% c(1:9), paste0("0", mes), mes),
    dia = ifelse(as.numeric(dia) %in% c(1:9), paste0("0", dia), dia),
    Muestra_sitio.fecha = paste0(anio, "-", mes, "-", dia)
  ) %>%
  select(
    -anio,
    -mes,
    -dia
  ) %>%
  mutate(
    # Agregando un 0 antes de horas y minutos cuando se requiera:
    hora = ifelse(as.numeric(hora) %in% c(0:9), paste0("0", hora), hora),
    minutos = ifelse(as.numeric(minutos) %in% c(0:9), paste0("0", minutos), minutos),
    Muestra_sitio.hora = ifelse(
      is.na(hora) | is.na(minutos), NA,
      paste0(hora, ":", minutos))
  ) %>%
  select(
    -hora,
    -minutos
  ) %>%
  rename(
    Muestra_sitio.pais = pais,
    Muestra_sitio.region_healthy_reefs = region_healthy_reefs,
    Muestra_sitio.localidad = localidad,
    Muestra_sitio.tipo_arrecife = tipo_arrecife,
    Muestra_sitio.subtipo_arrecife = subtipo_arrecife,
    Muestra_sitio.zona_arrecifal = zona_arrecife
  ) %>%
  mutate(
    # Estoy suponiendo que Esme ya identificó todos los muestreos de sitio que
    # pertenecen a un ANP
    Muestra_sitio.dentro_area_natural_protegida = ifelse(!is.na(anp), TRUE, FALSE)
  ) %>%
  rename(
    Muestra_sitio.nombre_area_natural_protegida = anp,
    Muestra_sitio.dentro_area_no_pesca = area_no_pesca,
    Muestra_sitio.latitud = latitud,
    Muestra_sitio.longitud = longitud
  ) %>%
  mutate(
    Muestra_sitio.datum = "WGS84"
  ) %>%
  rename(
    Muestra_sitio.metodo_seleccion = metodo_seleccion_sitios,
    Muestra_sitio.metodologia = protocolo,
    # Muestra_sitio.temperatura_c = temperatura_c,
    Muestra_sitio.profundidad_m = profundidad_media_m_sitio,
    Muestra_sitio.comentarios = comentarios
  ) %>%
  mutate(
    Muestra_sitio.excluir = FALSE,
    Muestra_sitio.datos_abiertos = ifelse(stri_detect_fixed(archivo_origen, "privados"), FALSE, TRUE),
    #Muestra_sitio.etapa_revision
    #Muestra_sitio.compatibilidad_cliente
    Muestra_sitio.fuente = ifelse(stri_detect_fixed(Muestreo.nombre, "Base de datos HRI"), "Base de datos", "Otro")
  ) %>%
  
  ### Muestra_transecto ###
  
  rename(
    Muestra_transecto.nombre = transecto,
    Muestra_transecto.transecto_fijo = transecto_fijo,
    Muestra_transecto.temperatura_c = temperatura_c,
    Muestra_transecto.profundidad_inicial_m = profundidad_inicial_m,
    Muestra_transecto.profundidad_final_m = profundidad_final_m
  ) %>%
  
  # Calculando "Muestra_transecto.subcuadrantes_planeados":
  # Para cada muestra de transecto:
  group_by(Muestreo.nombre, Muestra_sitio.nombre, remuestreo_en_mismo_muestreo, Muestra_transecto.nombre) %>%
  mutate(
    # Con que un registro de dicha muestra de transecto tenga información de cuadrante,
    # quiere decir que el transecto tiene subcuadrantes planeados
    Muestra_transecto.subcuadrantes_planeados = sum(!is.na(cuadrante)) %>%
      as.logical()
  ) %>%
  ungroup() %>%
  rename(
    # Muestra_transecto.seleccion_aleatoria_centros_subcuadrantes,
    # Muestra_transecto.distancia_centros_subcuadrantes_m
  ) %>%
  
  # Calculando "Muestra_transecto.longitud_subcuadrante_m" y
  # "Muestra_transecto.ancho_subcuadrante_m" para cada muestra de
  # transecto. Si tiene subcuadrantes planeados, se pone el primer valor de la
  # columna "longitud_cuadrante_m", al ordenarla por ella misma (los NA's) quedan
  # al final, en otro caso se pone NA
  group_by(Muestreo.nombre, Muestra_sitio.nombre, remuestreo_en_mismo_muestreo, Muestra_transecto.nombre) %>%
  mutate(
    Muestra_transecto.longitud_subcuadrante_m = ifelse(Muestra_transecto.subcuadrantes_planeados,
      first(longitud_cuadrante_m, order_by = longitud_cuadrante_m), NA),
    Muestra_transecto.ancho_subcuadrante_m = ifelse(Muestra_transecto.subcuadrantes_planeados,
      first(ancho_cuadrante_m, order_by = ancho_cuadrante_m), NA)
  ) %>%
  ungroup() %>%
  mutate(
    Muestra_transecto.comentarios = NA_character_
  ) %>%
  select(
    -longitud_cuadrante_m,
    -ancho_cuadrante_m
  ) %>%
  
  ### Muestra_*_info ###
  
  # Para cualquier *:
  # - Muestra_*_info.metodo_muestreo = metodo 
  # - Muestra_*_info.nivel_agregacion_datos = nivel_agregacion_datos
  # - Muestra_*_info.observador = observador
  # - Muestra_*_info.longitud_muestreada_m = longitud_transecto_m
  # - Muestra_*_info.ancho_muestreado_m = ancho_transecto_m
  # - Muestra_*_info.muestreo_completado = muestreo_completo
  
  rename(
    .Muestra_info.metodo_muestreo = metodo,
    .Muestra_info.nivel_agregacion_datos = nivel_agregacion_datos,
    .Muestra_info.observador = observador,
    .Muestra_info.longitud_muestreada_m = longitud_transecto_m,
    .Muestra_info.ancho_muestreado_m = ancho_transecto_m,
    .Muestra_info.muestreo_completado = muestreo_completo
  ) %>%
  
  ### Muestra_transecto_bentos_info ###
  
  mutate(
    Muestra_transecto_bentos_info.numero_puntos_muestreados = case_when(
      .Muestra_info.metodo_muestreo == "PIT" ~ puntos_o_cm_reales_transecto,
      .Muestra_info.metodo_muestreo == "CPF" ~ puntos_o_cm_reales_transecto,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    -puntos_o_cm_reales_transecto
  ) %>%
  mutate(
    Muestra_transecto_bentos_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_corales_info ###
  
  mutate(
    # Muestra_transecto_corales_info.criterio_seleccion_colonias
    # Muestra_transecto_corales_info.tamanio_minimo_colonia_cm
    Muestra_transecto_corales_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_peces_info ###
  
  rename(
    Muestra_transecto_peces_info.peces_muestreados = peces_muestreados
  ) %>%
  mutate(
    Muestra_transecto_peces_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_invertebrados_info ###
  
  mutate(
    Muestra_transecto_invertebrados_info.todos_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ TRUE,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_carideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_estenopodideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_aquelados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_astacideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_braquiuros_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_anomuros_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_estomatopodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_palinuridos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_no_decapodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.moluscos_gastropodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.moluscos_bivalvos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.moluscos_cefalopodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.otros_moluscos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_crinoideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_asteroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_ofiuroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_equinoideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_holothuroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.otros_equinodermos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.otros_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.detalles_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ "",
      TRUE ~ NA_character_
    ),
    Muestra_transecto_invertebrados_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_complejidad_info ###
  
  rename(
    Muestra_transecto_complejidad_info.rugosidad_longitud_contorno_m = tamanio_cadena_m
  ) %>%
  mutate(
    Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_m = .Muestra_info.longitud_muestreada_m,
    # Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_fija,
    Muestra_transecto_complejidad_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_subcuadrante_de_transecto_reclutas_info ###
  
  mutate(
    Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante = cuadrante
  ) %>%
  rename(
    Muestra_subcuadrante_de_transecto_reclutas_info.sustrato = sustrato
  ) %>%
  mutate(
    Muestra_subcuadrante_de_transecto_reclutas_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_subcuadrante_de_transecto_complejidad_info ###
  
  mutate(
    Muestra_subcuadrante_de_transecto_complejidad_info.numero_cuadrante = cuadrante
  ) %>%
  select(
    -cuadrante
  ) %>%
  rename(
    Muestra_subcuadrante_de_transecto_complejidad_info.maximo_relieve_m = relieve
  ) %>%
  mutate(
    Muestra_subcuadrante_de_transecto_complejidad_info.comentarios = NA_character_
  ) %>%
  
  ### Tabla_observaciones ###
  
  # Las tablas de observaciones corresponden a las tablas que contienen los datos
  # correspondientes a un muestreo particular de determinado aspecto. Por ejemplo,
  # "Muestra_transecto_bentos_punto" o "Muestra_transecto_corales_observacion".
    
  # Para todas las tablas de observaciones:
  # - Tabla_observaciones.codigo = codigo
  # - La columna "Tabla_observaciones.numero_observacion" corresponde a la cuenta
  #   de observaciones para cada muestreo de determinado aspecto en determinado
  #   muestreo de transecto.
  
  # Calculando la cuenta de observaciones para cada muestreo de determinado aspecto
  # en determinado muestreo de transecto.
  # Supuesto: para cada muestreo de transecto, en cada "archivo_origen" sólo
  # aparecen observaciones de la misma naturaleza (por lo que tiene sentido
  # enlistarlas)
  ddply(
    .(Muestreo.nombre, Muestra_sitio.nombre, remuestreo_en_mismo_muestreo, Muestra_transecto.nombre, archivo_origen),
    function(df){
      resultado <- df %>%
        mutate(
          .Tabla_observaciones.numero_observacion = 1:nrow(df)
        )
      return(resultado)
  }) %>%
  
  ### Muestra_transecto_bentos_punto (PIT desagregado)
  # Las columnas se corresponden intuitivamente

  ### Muestra_transecto_bentos_linea ###
  # Aún no se calcula porque no se han encontrado datos de LIT desagregados.
  
  ### Muestra_transecto_corales_observacion ###
  # Las columnas se corresponden intuitivamente.
    
  ### Muestra_transecto_peces_cuenta ###

  # Por facilidad se hará el "gather" de peces a la hora de integrar los datos.
  # Además, se agregarán los peces en "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  # (que vienen desglosados en observaciones) en el mismo script.
  
  ### Muestra_transecto_invertebrados_cuenta ###
  # Por facilidad, se agregarán los datos de "conacyt_greenpeace_2016_invertebrados_desagregados_v3"
  # a la hora de integrar las tablas.
  
  ### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
  # Ambas tablas: "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla"
  # y "conacyt_greenpeace_2016_reclutas_desagregados_v3" pertenecen a este tipo
  # porque no tienen información sobre reclutas individuales.
  
  
  ###############################################################
  # Transformaciones finales por "archivo_origen"
  ###############################################################
  
  ### Muestra_transecto_corales_observacion ###
  
  # Archivos de Excel relacionados:
  # - "conacyt_greenpeace_2016_corales_desagregados"
  # - "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
  
  # - Se revisará que todos los transectos estén declarados, independientemente
  # de si tuvieron o no observaciones.
  
  ### Muestra_transecto_peces_cuenta ###
  
  # Archivos de Excel relacionados:
  # - "conacyt_greenpeace_2016_peces_agregados_especie_talla"
  # - "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados"
  # - "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla"
  # - "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  
  # - Se cambiará el campo "especie" por "nombre_cientifico". Revisar que no haya
  #   registros duplicados al hacer el join con el catálogo, pues "especie" no es
  #   una llave natural de "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico".
  # - Se agregarán registros con los mismos valores en: "Muestreo.nombre", "Muestra_sitio.nombre",
  #   "remuestreo_en_mismo_muestreo", "Muestra_transecto.nombre", "nombre_cientifico",
  #   por consistencia con la tabla de interés.
  # - Se revisará que todos los transectos estén incluídos, independiente si
  #   tienen o no observaciones.
  # - Se transformarán los datos para darles la estructura apropiada.
  
  ### Revisión de Esme con respecto a los puntos anteriores:
revision_numero_transectos_muestreo_sitio <- datos_globales %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_bentos_desagregados",
    "conacyt_greenpeace_2016_corales_desagregados",
    "conacyt_greenpeace_2016_invertebrados_desagregados",
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales",
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
    
    # Los transectos
  )) %>%
  select(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    remuestreo_en_mismo_muestreo,
    Muestra_transecto.nombre
  ) %>%
  distinct() %>%
  group_by(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    remuestreo_en_mismo_muestreo) %>%
  tally()

revision_numero_cuadrantes_muestreo_transecto <- datos_globales %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_bentos_desagregados",
    "conacyt_greenpeace_2016_corales_desagregados",
    "conacyt_greenpeace_2016_invertebrados_desagregados",
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales",
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  )) %>%
  select(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    remuestreo_en_mismo_muestreo,
    Muestra_transecto.nombre
  ) %>%
  distinct() %>%
  group_by(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    remuestreo_en_mismo_muestreo) %>%
  tally()


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# Esperando a Esme:
# 1. Ver si puedo cambiar el nombre de ""historicos_y_2017_transecto_invertebrados_agregados_conteos_especie""
# a "historicos_y_2017_cuadrante_invertebrados_agregados_conteos_especie"
  

#!!! = "código posiblemente no generalizable a la hora de integrar más Exceles".

datos_globales <- Reduce(rbind.fill, lista_tablas_columnas_homologadas) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  elimina_columnas_vacias() %>%
  
  ## Editando las columnas para formar las tablas en el "esquema_v5" apropiadamente
  
  # En la columna "nombre_proyecto", se codificó al final una letra A/B/C para
  # distinguir entre remuestreos del mismo sitio, dentro del mismo proyecto. Esta
  # información la extraeré en una columna nueva.
  # Supuesto: ningún proyecto termina con "A", "B" o "C", a menos que sea indicativo 
  # de un remuestreo de sitio dentro del mismo proyecto.
  
  ##########################
  # Muestreo, Muestra_sitio
  ##########################

  # Debido a que hay varios typos en los campos de fecha y hora de muestreo de
  # sitio, se utilizará "genera_tabla_2" para crear la tabla "Muestra_sitio"

  mutate(
    
    nombre_muestreo_si_A = stri_match_first(nombre_proyecto, regex = "(.*)A$")[,2],
    nombre_muestreo_si_B = stri_match_first(nombre_proyecto, regex = "(.*)B$")[,2],
    nombre_muestreo_si_C = stri_match_first(nombre_proyecto, regex = "(.*)C$")[,2],
    
    nombre_muestreo = ifelse(!is.na(nombre_muestreo_si_A), nombre_muestreo_si_A,
      ifelse(!is.na(nombre_muestreo_si_B), nombre_muestreo_si_B,
        ifelse(!is.na(nombre_muestreo_si_C), nombre_muestreo_si_C,
          nombre_proyecto
    ))),
    
    # Variable que es A o B si el sitio fue remuestreado en el mismo muestreo,
    # y NA e.o.c
    remuestreo_mismo_muestreo = ifelse(!is.na(nombre_muestreo_si_A), "A",
      ifelse(!is.na(nombre_muestreo_si_B), "B",
        ifelse(!is.na(nombre_muestreo_si_C), "C", NA
    )))
  ) %>%
  select(
    -nombre_muestreo_si_A,
    -nombre_muestreo_si_B,
    -nombre_muestreo_si_C
  ) %>%

  # Creando los nombres de los sitios: recordar que para los datos CONACyT /
  # GreenPeace tenemos nombres de muestreo de sitio, que son casi nombres de sitio,
  # Sólo Chankanaab y ChankanaabGP son el mismo sitio con distintos nombres.
  # Recordar que podemos seguirlos diferenciando porque ya agregamos la columna
  # "identificador_muestreo_sitio".
  mutate(
    nombre_sitio = estandariza_strings(nombre_sitio),
    
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

  # Haciendo columna "dentro de ANP". Sabemos que si "anp" es NA quiere decir que no.
  # "No se" será un NA en "dentro_anp
  mutate(
    dentro_anp = ifelse(is.na(anp), FALSE, TRUE),
    dentro_area_no_pesca = case_when(
      area_no_pesca == "si" ~ TRUE,
      area_no_pesca == "no" ~ FALSE,
      is.na(area_no_pesca) ~ NA)
  ) %>%
  
  ####################
  # Muestra_transecto
  ####################

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

l_ply(1:length(revision_valores), function(i){
  nombre_columna <- names(revision_valores)[i]
  valores_columna <- names(revision_valores[[i]])
  
  write(nombre_columna, "~/Desktop/prueba.txt", append = TRUE)
  write(valores_columna, "~/Desktop/prueba.txt", append = TRUE)
})

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