# En este script se toma "lista_tablas_columnas_homologadas· y se crea un data
# frame listo para ser separado en las tablas descritas en el esquema v5".

# La idea básica es la siguiente:
# 1. Se agregan los data frames en "lista_tablas_columnas_homologadas" en uno sólo.
# 2. Del data frame anterior, se seleccionan las columnas con información de
# interés para el esquema v5 de la base de datos
# 3. Se realizan diversas transformaciones en el data frame anterior con el fin
# de formar un nuevo data frame que se pueda dividir fácilmente en las tablas y
# columnas especificadas en el esquema v5.
# 4. Se realiza una revisión rápida del data frame formado en el paso 3. 

# Se supone que los datos están lo más limpios posible a la hora de formar el
# data frame, anterior. Toda imperfección deberá ser arreglada en los archivos
# de Excel.

# En scripts subsecuentes, el data frame producido por este script se segmentará
# apropiadamente en tablas.

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")

library("readr")
library("doMC") # procesamiento en paralelo

#Revisar número de nucleos en la terminal (Mac): sysctl hw.ncpu
registerDoMC(4)

################################################################################
# 1. Creando data frame con la información integrada:
################################################################################

# Leyendo lista de tablas con columnas homologadas
lista_tablas_columnas_homologadas <- readRDS(
  paste0(rutas_salida[2], "/lista_tablas_columnas_homologadas.RData"))

# Agregando dichas tablas en un solo data frame
datos_globales_crudos <- Reduce(rbind.fill, lista_tablas_columnas_homologadas) %>%
  mutate(
    id = 1:nrow(.)
  )

################################################################################
# 2. Creando una tabla con las columnas de importancia la base de datos
################################################################################

# Cabe destacar que se limpiaron, reestructuraron y cambió el tipo de datos de
# algunas de estas columnas.

datos_globales_columnas_selectas <- datos_globales_crudos %>%
  
  ## Mutate numeric_integer eliminará por completo cualquier valor no numérico en las
  ## columnas seleccionadas
  mutate_numeric_integer(c(
    
    ### Auxiliar ###
    "serie" = "integer",
    
    ### Muestreo ###
    "anio_inicio_proyecto" = "integer",
    "anio_termino_proyecto" = "integer",
    
    ### Muestra_sitio ###
    "anio" = "integer",
    "mes" = "integer",
    "dia" = "integer",
    "hora" = "integer",
    "minutos" = "integer",
    "latitud" = "numeric",
    "longitud" = "numeric",
    "temperatura_c" = "numeric",
    "profundidad_media_m_sitio" = "numeric",
    
    ### Muestra_transecto ###
    "profundidad_inicial_m" = "numeric",
    "profundidad_final_m" = "numeric",
    "longitud_cuadrante_m" = "numeric",
    "ancho_cuadrante_m" = "numeric",
    
    ### Muestra_sitio_bentos_info ###
    "puntos_o_cm_reales_transecto" = "integer",
    
    ### Muestra_sitio_bentos_porcentaje ###
    "cobertura" = "numeric",
    
    ### Muestra_transecto_bentos_info ###
    "longitud_transecto_m" = "numeric",
    # "puntos_o_cm_reales_transecto",
    
    ### Muestra_transecto_bentos_punto ###
    "altura_algas_cm" = "numeric",
    
    ### Muestra_transecto_bentos_porcentaje ###
    # "cobertura",
    
    ### Muestra_transecto_corales_info ###
    # "longitud_transecto_m",
    "ancho_transecto_m" = "numeric",
    
    ### Muestra_transecto_corales_observacion ###
    "d1_max_diam_cm" = "numeric",
    "d2_min_diam_cm" = "numeric",
    "altura_maxima_cm" = "numeric",
    "porcentaje" = "integer",
    "mortalidad_antigua" = "integer",
    "mortalidad_reciente" = "integer",
    "mortalidad_total" = "integer",
    "mortalidad_transicion" = "integer",
    
    ### Muestra_transecto_peces_info ###
    # "longitud_transecto_m",
    # "ancho_transecto_m",
    
    ### Muestra_transecto_peces_cuenta ###
    "peces_tamanio_0cm_5cm" = "integer",
    "peces_tamanio_101cm_110cm" = "integer",
    "peces_tamanio_101cm_9999cm" = "integer",
    "peces_tamanio_111cm_120cm" = "integer",
    "peces_tamanio_11cm_20cm" = "integer",
    "peces_tamanio_191cm_200cm" = "integer",
    "peces_tamanio_21cm_30cm" = "integer",
    "peces_tamanio_31cm_40cm" = "integer",
    "peces_tamanio_41cm_50cm" = "integer",
    "peces_tamanio_51cm_60cm" = "integer",
    "peces_tamanio_61cm_70cm" = "integer",
    "peces_tamanio_6cm_10cm" = "integer",
    "peces_tamanio_71cm_80cm" = "integer",
    "peces_tamanio_81cm_90cm" = "integer",
    "peces_tamanio_91cm_100cm" = "integer",
    
    ### Muestra_transecto_invertebrados_info ###
    # "longitud_transecto_m",
    # "ancho_transecto_m",
    
    ### Muestra_transecto_invertebrados_cuenta ###
    "conteo" = "integer",
    
    ### Muestra_subcuadrante_de_transecto_reclutas_info ###
    # "cuadrante",
    
    ### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
    "tamanio_minimo_cm" = "numeric",
    "tamanio_maximo_cm" = "numeric",
    "n" = "integer",
    
    ### Muestra_transecto_complejidad_info ###
    # "longitud_transecto_m",
    "tamanio_cadena_m" = "numeric",
    
    ### Muestra_subcuadrante_de_transecto_complejidad_info ###
    # "cuadrante",
    "relieve" = "numeric"
  ), warnings = FALSE) %>%
  
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
    -fision,
    -fuente,
    -longitud_teorica_m_bentos,
    -longitud_teorica_m_corales,
    -longitud_teorica_m_invertebrados_agrra_v5,
    -longitud_teorica_m_invertebrados_otros,
    -longitud_teorica_m_peces,
    -necrosis,
    -neoplasia,
    -nombre_no_remuestreo,
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
    -subzona_habitat,
    -superficie_cm2,
    -tejido_vivo,
    -tipo_arrecife_redundante,
    -unidades_profundidad,
    -X__1,
    -X__2,
    -x__1,
    -zona_arrecife_redundante
  ) %>%
  
  # Ordenando las columnas en orden alfabético
  select(noquote(sort(colnames(.))))

glimpse(datos_globales_columnas_selectas)

################################################################################
# 3. Creando la tabla con la información lista para ser integrada
################################################################################

datos_globales <- datos_globales_columnas_selectas %>%

  ### Muestreo ####

  mutate(
    
    # En la columna "nombre_proyecto", se codificó al final una letra A/B/C para
    # distinguir entre remuestreos del mismo sitio, dentro del mismo proyecto. Esta
    # información la extraeré en una columna nueva. Esto no se hizo para los datos
    # de CONACyT / GreenPeace (en los que se tiene la columna "identificador_muestreo_sitio")
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
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo = ifelse(!is.na(Muestreo.nombre_si_A), "A",
      ifelse(!is.na(Muestreo.nombre_si_B), "B",
        ifelse(!is.na(Muestreo.nombre_si_C), "C", NA
    )))
  ) %>%
  rename(
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace = identificador_muestreo_sitio
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
    nombre_original = estandariza_strings(nombre_original),
    Muestra_sitio.nombre_original = ifelse(!is.na(nombre_original),
      nombre_original, Muestra_sitio.nombre)
  ) %>%
  select(
    -nombre_original
  ) %>%
  mutate(
    # Agregando un 0 antes de meses y días cuando se requiera:
    mes = ifelse(mes %in% c(1:9), paste0("0", mes), mes),
    dia = ifelse(dia %in% c(1:9), paste0("0", dia), dia),
    Muestra_sitio.fecha = paste0(anio, "-", mes, "-", dia)
  ) %>%
  select(
    -anio,
    -mes,
    -dia
  ) %>%
  mutate(
    # Agregando un 0 antes de horas y minutos cuando se requiera:
    hora = ifelse(hora %in% c(0:9), paste0("0", hora), hora),
    minutos = ifelse(minutos %in% c(0:9), paste0("0", minutos), minutos),
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
    Muestra_sitio.temperatura_c = temperatura_c,
    Muestra_sitio.profundidad_m = profundidad_media_m_sitio,
    Muestra_sitio.comentarios = comentarios
  ) %>%
  mutate(
    Muestra_sitio.excluir = FALSE,
    Muestra_sitio.datos_abiertos = ifelse(stri_detect_fixed(archivo_origen, "privados"), FALSE, TRUE),
    # Muestra_sitio.etapa_revision
    # Muestra_sitio.compatibilidad_cliente
    Muestra_sitio.fuente = ifelse(stri_detect_fixed(Muestreo.nombre, "Base de datos HRI"), "Base de datos", "Otro")
  ) %>%
  
  ### Muestra_transecto ###
  
  rename(
    Muestra_transecto.nombre = transecto,
    Muestra_transecto.transecto_fijo = transecto_fijo,
    Muestra_transecto.profundidad_inicial_m = profundidad_inicial_m,
    Muestra_transecto.profundidad_final_m = profundidad_final_m
  ) %>%
  
  # Calculando "Muestra_transecto.subcuadrantes_planeados":
  # Para cada muestra de transecto:
  group_by(
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
    Muestra_transecto.nombre) %>%
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
  group_by(
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
    Muestra_transecto.nombre) %>%
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
    Muestra_._info.metodo_muestreo = metodo,
    Muestra_._info.nivel_agregacion_datos = nivel_agregacion_datos,
    Muestra_._info.observador = observador,
    Muestra_._info.longitud_muestreada_m = longitud_transecto_m,
    Muestra_._info.ancho_muestreado_m = ancho_transecto_m,
    Muestra_._info.muestreo_completado = muestreo_completo
  ) %>%
  
  ### Muestra_sitio/transecto_bentos_info ###
  
  mutate(
    # Muestra_sitio_bentos_info.longitud_muestreada_media_m,
    
    Muestra_._bentos_info.numero_puntos_muestreados = case_when(
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "PIT" ~
        as.integer(puntos_o_cm_reales_transecto),
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "CPF" ~
        as.integer(puntos_o_cm_reales_transecto),
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "CPV" ~
        as.integer(puntos_o_cm_reales_transecto),
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "Otro" ~ 
        as.integer(puntos_o_cm_reales_transecto),
      TRUE ~ NA_integer_
    ),
    Muestra_transecto_bentos_info.longitud_contorno_transecto_si_lit_m = case_when(
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "LIT" ~
        round(puntos_o_cm_reales_transecto / 100, 2),
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    -puntos_o_cm_reales_transecto
  ) %>%
  
  mutate(
    Muestra_._bentos_info.comentarios = NA_character_
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
    Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_m = Muestra_._info.longitud_muestreada_m,
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
  
  ### Tablas de registros de observaciones ###
  
  # Las tablas de registros de observaciones corresponden a las tablas que
  # contienen los datos correspondientes a un muestreo particular de determinado
  # aspecto. Por ejemplo, "Muestra_transecto_bentos_punto" o
  # "Muestra_transecto_corales_observacion".
  
  # Cabe destacar que al operar sobre estas tablas, se debe tener cuidado de no
  # perder los registros de muestreos realizados pero donde no se obtuvieron
  # observaciones. Esto en particular afecta a las tablas de peces, reclutas e
  # invertebrados.
    
  # Para todas las tablas de datos:
  # - Registro_observacion.codigo = codigo
  # - La columna "Registro_observacion.numero_observacion" corresponde a la
  #   enumeración de observaciones para cada muestreo de determinado aspecto en
  #   determinado muestreo de transecto.
  
  # Calculando la cuenta de observaciones para cada muestreo de determinado aspecto
  # en determinado muestreo de transecto.
  # Supuestos:
  # - Para cada muestreo de transecto, el orden de aparición de las observaciones
  #   en el archivo corresponde al orden de observación.
  # - Para cada muestreo de transecto, en cada "archivo_origen" sólo
  #   aparecen observaciones de la misma naturaleza (por lo que tiene sentido
  #   enlistarlas).

  # Notar que este número no tiene significado para muchas tablas en el esquema
  # (las que contienen datos de complejidad, de toma de datos en cuadrantes, o
  # datos agregados). Sin embargo, para no agregar complejidad al código, se
  # decidió calcularla para todos los datos. Simplemente hacer caso omiso de
  # esta columna para las tablas en que no tiene sentido.
  
  ddply(
    .(Muestreo.nombre,
      Muestra_sitio.nombre,
      Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
      Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
      Muestra_transecto.nombre,
      archivo_origen),
    function(df){
      resultado <- df %>%
        mutate(
            Registro_observacion.numero_observacion = 1:nrow(df)
          )
      return(resultado)
  }, .parallel = TRUE) %>%
  
  rename(
    Registro_observacion.codigo = codigo
    # Para los archivos de peces se tiene la columna "nombre_cientifico_abreviado"
    # que es más útil, y para invertebrados se tiene únicamente "tipo". Por ello,
    # "Registro_observacion.codigo" sólamente es útil para bentos, corales y reclutas
  ) %>%
  
  ### Muestra_sitio/transecto_bentos_porcentaje
  
  rename(
    Muestra_._bentos_porcentaje.porcentaje_cobertura = cobertura
    # Esta columna hay que revisarla, pues contiene números negativos y otros que
    # no suman el 100% (ver archivo auxiliar). Por lo pronto se copiará tal cual.
  ) %>%
  
  # Agregando los datos por muestra de sitio/transecto y código de especie.
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)
    
    if(stri_detect_fixed(archivo_origen, "bentos_agregados")){
      resultado <- df %>%
        group_by(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          # Si son bentos agregados a nivel de sitio, Muestra_transecto.nombre == NA
          # y se agregará por sitio.
          Muestra_transecto.nombre, 
          # en el ddply ya se separó por "archivo_origen"
          Registro_observacion.codigo
        ) %>%
        mutate(
          Muestra_._bentos_porcentaje.porcentaje_cobertura = sum(
            Muestra_._bentos_porcentaje.porcentaje_cobertura, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
        # hacer el mutate(). No se puede usar summarise() porque se pierden las
        # otras columnas
        distinct(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Registro_observacion.codigo,
          .keep_all = TRUE
        )
    } else{
      resultado <- df
    }
    return(resultado)
  }) %>% 
  
  ### Muestra_transecto_bentos_punto (PIT desagregado)
  rename(
    Muestra_transecto_bentos_punto.altura_si_alga_cm = altura_algas_cm
  ) %>%

  ### Muestra_transecto_bentos_linea ###
  # Aún no se calcula porque no se han encontrado datos de LIT desagregados.
  
  ### Muestra_transecto_corales_observacion ###
  rename(
    Muestra_transecto_corales_observacion.tipo_observacion = clump,
    Muestra_transecto_corales_observacion.d1_max_diam_cm = d1_max_diam_cm,
    Muestra_transecto_corales_observacion.d2_min_diam_cm = d2_min_diam_cm,
    Muestra_transecto_corales_observacion.altura_maxima_cm = altura_maxima_cm
  ) %>%
  
  # Para todas las siguientes variables, se supone que siempre se midieron, y que
  # un NA significa que no se observó el efecto buscado.
  mutate(
    Muestra_transecto_corales_observacion.tipo_blanqueamiento = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(blanqueamiento),
      "NO",
      blanqueamiento
    ),
    Muestra_transecto_corales_observacion.porcentaje_blanqueamiento = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & Muestra_transecto_corales_observacion.tipo_blanqueamiento == "NO",
      as.integer(0),
      porcentaje
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_total = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_total),
      0,
      mortalidad_total
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_reciente = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_reciente),
      0,
      mortalidad_reciente
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_transicion = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_transicion),
      0,
      mortalidad_transicion
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_antigua = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_antigua),
      0,
      mortalidad_antigua
    ),
    Muestra_transecto_corales_observacion.enfermedad = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(enfermedades),
      "NO",
      enfermedades
    ),
    Muestra_transecto_corales_observacion.sobrecrecimiento = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(sobrecrecimiento),
      "NO",
      sobrecrecimiento
    ),
    # No podemos suponer que siempre se anotaron sobrecrecimientos adicionales
    # (aún bajo el supuesto de que en todas las colonias se revisaron sobrecrecimientos)
    # Por ello, un NA en sobrecrecimiento adicional puede ser que existe y no
    # se anotó o que simplemente no existe. Es decir, hay que dejar los NA
    # como "no se".
    Muestra_transecto_corales_observacion.sobrecrecimiento_adicional = sobrecrecimiento_1,
    
    Muestra_transecto_corales_observacion.depredacion = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(depredacion),
      "NO",
      depredacion
    ),
    Muestra_transecto_corales_observacion.lesion = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(lesiones),
      "NO",
      lesiones
    ),
    Muestra_transecto_corales_observacion.comentarios = NA_character_
  ) %>%
  select(
    -blanqueamiento,
    -porcentaje,
    -mortalidad_total,
    -mortalidad_reciente,
    -mortalidad_transicion,
    -mortalidad_antigua,
    -enfermedades,
    -sobrecrecimiento,
    -sobrecrecimiento_1,
    -depredacion,
    -lesiones
  ) %>%

  ### Muestra_transecto_peces_cuenta ###
  # Se tiene cuidado de no perder los muestreos realizados y que no tuvieron
  # observaciones
  
  # Haciendo el "gather" de las columnas de "peces_tamanio_._.", para los
  # archivos de peces. Se agregarán los datos por muestra de transecto, nombre
  # científico abreviado, edad y categoría de talla.
  
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)

    if(stri_detect_fixed(archivo_origen, "peces")){
      # Generando columnas de "minimo_tamanio_cm", "maximo_tamanio_cm" y "cuenta"
      resultado  <- df %>%
        gather(key = peces_tamanio_min_max, value = cuenta, dplyr::contains("peces_tamanio")) %>%
        # Filtrando tamaños no encontrados en un mismo conteo de especie en transecto.
        # Notar que si "nombre_cientifico_abreviado" es NA, se preserva el registro
        # pues puede corresponder a transectos sin observaciones. Estos se tendrán
        # que filtrar a la hora de crear la tabla: "Muestra_transecto_peces_cuenta".
        filter(!is.na(cuenta) | is.na(nombre_cientifico_abreviado)) %>%
        # Generando campos de "tamanio_minimo_cm" y "tamanio_maximo_cm"
        separate(col = peces_tamanio_min_max, into = c("peces", "tamanio",
          "peces_tamanio_minimo_cm", "peces_tamanio_maximo_cm")) %>%
        select(
          -peces,
          -tamanio
        ) %>%
        mutate(
          # Quitándoles la etiqueta de "cm" a "peces_tamanio_minimo_cm" y "peces_tamanio_maximo_cm"
          peces_tamanio_minimo_cm = stri_sub(peces_tamanio_minimo_cm, from = 1,
            to = (stri_length(peces_tamanio_minimo_cm)-2)) %>%
            as.integer(),
          peces_tamanio_maximo_cm = stri_sub(peces_tamanio_maximo_cm, from = 1,
            to = (stri_length(peces_tamanio_maximo_cm)-2)) %>%
            as.integer(),
          es_juvenil = NA # lógico
        ) %>%
        rename(
          Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado = nombre_cientifico_abreviado,
          Muestra_transecto_peces_cuenta.tamanio_minimo_cm = peces_tamanio_minimo_cm,
          Muestra_transecto_peces_cuenta.tamanio_maximo_cm = peces_tamanio_maximo_cm,
          Muestra_transecto_peces_cuenta.cuenta = cuenta,
          Muestra_transecto_peces_cuenta.es_juvenil = es_juvenil
        ) %>%
        # Agregando los datos por muestra de transecto, nombre científico
        # abreviado, edad y categoría de talla.
        group_by(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "archivo_origen"
          Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado,
          Muestra_transecto_peces_cuenta.tamanio_minimo_cm,
          Muestra_transecto_peces_cuenta.tamanio_maximo_cm,
          Muestra_transecto_peces_cuenta.es_juvenil
        ) %>%
        mutate(
          Muestra_transecto_peces_cuenta.cuenta = sum(Muestra_transecto_peces_cuenta.cuenta, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
        # hacer el mutate(). No se puede usar summarise() porque se pierden las
        # otras columnas
        distinct(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado,
          Muestra_transecto_peces_cuenta.tamanio_minimo_cm,
          Muestra_transecto_peces_cuenta.tamanio_maximo_cm,
          Muestra_transecto_peces_cuenta.es_juvenil,
          .keep_all = TRUE
        )
    } else{
      # Si "archivo_origen" no es de peces, se eliminan las columnas de
      # "peces_tamanio" y se agregan columnas dummy para que se pueda hacer la
      # fusión al final del ddply().
      resultado <- df %>%
        select(
          -dplyr::contains("peces_tamanio_")
        ) %>%
        rename(
          Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado = nombre_cientifico_abreviado
        ) %>%
        mutate(
          Muestra_transecto_peces_cuenta.tamanio_minimo_cm = NA_integer_,
          Muestra_transecto_peces_cuenta.tamanio_maximo_cm = NA_integer_,
          Muestra_transecto_peces_cuenta.cuenta = NA_integer_,
          Muestra_transecto_peces_cuenta.es_juvenil = NA # lógico
        )
    }
    return(resultado)
  }, .parallel = TRUE) %>%

  ### Muestra_transecto_invertebrados_cuenta ###
  # Se tiene cuidado de no perder los muestreos realizados y que no tuvieron
  # observaciones
  
  rename(
    Muestra_transecto_invertebrados_cuenta.tipo = tipo,
    Muestra_transecto_invertebrados_cuenta.cuenta = conteo
  ) %>%
  
  # Agregando los datos por muestra de transecto y tipo de invertebrado.
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)
    
    if(stri_detect_fixed(archivo_origen, "invertebrados")){
      resultado <- df %>%
        group_by(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "archivo_origen"
          Muestra_transecto_invertebrados_cuenta.tipo
        ) %>%
        mutate(
          Muestra_transecto_invertebrados_cuenta.cuenta = sum(Muestra_transecto_invertebrados_cuenta.cuenta, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
        # hacer el mutate(). No se puede usar summarise() porque se pierden las
        # otras columnas
        distinct(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_transecto_invertebrados_cuenta.tipo,
          .keep_all = TRUE
        )
    } else{
      resultado <- df
    }
    return(resultado)
  }, .parallel = TRUE) %>%
  
   
  rename(
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio = categoria_tamanio,
    
    # Si los siguientes valores son iguales, entonces se tiene un recluta que
    # fue medido exactamente.
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm = tamanio_minimo_cm,
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm = tamanio_maximo_cm
  ) %>%
  
  # Para calcular "cuenta", los registros se agregan por muestra de cuadrante,
  # código, categoría de tamaño y tamaños mínimo y máximo:
  
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)
    
    if(stri_detect_fixed(archivo_origen, "reclutas")){
      resultado <- df %>%
        group_by(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante,
          Registro_observacion.codigo,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm
        ) %>%
        mutate(
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.cuenta = sum(n, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
        # hacer el mutate(). No se puede usar summarise() porque se pierden las
        # otras columnas.
        distinct(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante,
          Registro_observacion.codigo,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm,
          .keep_all = TRUE
        ) %>%
        select(-n)
    } else{
      resultado <- df %>%
        rename(
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.cuenta = n
        )
    }
    return(resultado)
  }, .parallel = TRUE) %>%
  
  #### Arreglos finales ###
  
  # Renombrando columnas que faltan
  rename(
    Auxiliar.archivo_origen = archivo_origen,
    Auxiliar.id = id,
    Auxiliar.identificador_sitio = identificador_sitio,
    Auxiliar.serie = serie
  ) %>%

  # Redondeando apropiadamente
  mutate(
    Muestra_._bentos_porcentaje.porcentaje_cobertura =
      round(Muestra_._bentos_porcentaje.porcentaje_cobertura, 4),
    
    Muestra_._info.ancho_muestreado_m = round(Muestra_._info.ancho_muestreado_m, 2),
    
    Muestra_._info.longitud_muestreada_m = round(Muestra_._info.longitud_muestreada_m, 2),
    
    Muestra_sitio.profundidad_m = round(Muestra_sitio.profundidad_m, 2),
    
    Muestra_sitio.temperatura_c = round(Muestra_sitio.temperatura_c, 2),
    
    Muestra_subcuadrante_de_transecto_complejidad_info.maximo_relieve_m =
      round(Muestra_subcuadrante_de_transecto_complejidad_info.maximo_relieve_m, 2),
    
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm =
      round(Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm, 1),
    
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm =
      round(Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm, 1),
    
    Muestra_transecto_bentos_info.longitud_contorno_transecto_si_lit_m =
      round(Muestra_transecto_bentos_info.longitud_contorno_transecto_si_lit_m, 2),
    
    Muestra_transecto_bentos_punto.altura_si_alga_cm =
      round(Muestra_transecto_bentos_punto.altura_si_alga_cm, 1),
    
    Muestra_transecto_complejidad_info.rugosidad_longitud_contorno_m =
      round(Muestra_transecto_complejidad_info.rugosidad_longitud_contorno_m, 2),
    
    Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_m =
      round(Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_m, 2),
    
    Muestra_transecto_corales_observacion.altura_maxima_cm =
      round(Muestra_transecto_corales_observacion.altura_maxima_cm, 1),
    
    Muestra_transecto_corales_observacion.d1_max_diam_cm =
      round(Muestra_transecto_corales_observacion.d1_max_diam_cm, 1),
    
    Muestra_transecto_corales_observacion.d2_min_diam_cm =
      round(Muestra_transecto_corales_observacion.d2_min_diam_cm, 1),
    
    Muestra_transecto.ancho_subcuadrante_m =
      round(Muestra_transecto.ancho_subcuadrante_m, 2),
    
    Muestra_transecto.longitud_subcuadrante_m =
      round(Muestra_transecto.longitud_subcuadrante_m, 2),
    
    Muestra_transecto.profundidad_final_m =
      round(Muestra_transecto.profundidad_final_m, 2),
    
    Muestra_transecto.profundidad_inicial_m =
      round(Muestra_transecto.profundidad_inicial_m, 2)
  ) %>%
  
  # Cambiando los NA's a "" en columnas tipo caracter:
  cambia_na_strings_vacios() %>%
  
  # Ordenando las columnas en orden alfabético
  select(noquote(sort(colnames(.))))
  
saveRDS(datos_globales, paste0(rutas_salida[6], "/datos_globales.RDS"))

################################################################################
# 4. revisión rápida del data frame anterior
################################################################################

glimpse(datos_globales)

lista_revision <- revisa_valores(datos_globales)

l_ply(1:length(lista_revision), function(i){
  print(names(lista_revision)[i])
  print(class(datos_globales[[names(lista_revision)[i]]]))
  View(lista_revision[[i]])
  readline(prompt="Presionar [enter] para continuar")
})

################################################################################

# Revisando columnas de tipo "double" no "integer" para ver si hay que redondear

datos_globales %>%
  select_if(is_double) %>%
  glimpse()
  
### Me quedé generando las tablas, después crearé varios resúmenes para
### revisarlas y las integraré en la base.

# Para porcentaje, si blanqueamiento es NO o NA, vale NA. en otro caso, puede
# valer el porcentaje de blanqueamiento del tipo seleccionado o NA si no se
# registró el porcentaje.

  #   # Creando columnas auxiliares útiles a la hora de generar las tablas
  #   strings_vacios = "",
  #   verdadero = TRUE,
  #   falso = FALSE,
  #   na_numerico = NA_real_,
  #   datum = "WGS84"
  # ) # %>%

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