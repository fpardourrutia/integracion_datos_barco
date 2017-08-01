# En este script se generarán las tablas de muestreos de aspectos a nivel de transecto
# Es decir, todas las llamadas "...transect_sample_info" y asociadas a ellas.

# Supuesto para generar las tablas de aspecto (bentos, peces, corales, reclutas,
# complejidad, invertebrados):
# 1. Cada Excel tiene información correspondiente a un sólo nivel espacial de
# agrupación de datos (transecto / sitio) .
# 2. Cada Excel tiene información correspondiente a un sólo nivel biológico de
# agregación de datos (por observacion, coberturas por especie, etc)...
# Esto es necesario para generar las tablas de "...info" (que tienen información)
# del nivel de agrupación de los datos en el nombre, y del nivel de agrupación
# biológica de los datos en el campo "...info.data_aggregation_level".

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("readr")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

# Leyendo datos globales con llaves primarias de proyecto, muestreo de sitio y
# muestreo de transecto
datos_globales_llaves_primarias <- readRDS("../productos/datos_globales_llaves_primarias.RData")

# Generando un subconjunto de la tabla anterior con información sólo a nivel transecto,
# para fácil manipulación de la misma:
datos_globales_transecto_llave_primaria <- datos_globales_llaves_primarias %>%
  select(
    -fuente,
    -autor_administrador_proyecto,
    -contacto,
    -titulo,
    -documento,
    -cita,
    -institucion,
    -suborganizacion,
    -nombre_proyecto,
    -metodo_seleccion_sitios,
    -tema,
    -proposito,
    -anio_inicio_proyecto,
    -anio_termino_proyecto,
    -pais,
    -anp,
    -region_healthy_reefs,
    -localidad,
    #-nombre_sitio, # Sirve para revisiones
    #-fecha_hora_muestreo_sitio, # Sirve para revisiones
    -tipo_arrecife,
    -zona_arrecifal,
    -subzona_habitat,
    -latitud,
    -longitud,
    -profundidad_media_m,
    -dentro_anp,
    -datum,
    -id_proyecto,
    -id_muestreo_sitio,
    -protocolo_muestreo_sitio
  )

# Revisando valores de las columnas de datos_globales:
revision_valores <- revisa_valores(datos_globales_transecto_llave_primaria)
names(revision_valores)

# Función para consultar el objeto anterior:
# nombre_columna: nombre de la columna a consultar.
# La función regresa la tabla correspondiente a esa columna
# El nombre de esta función es muy rápido para hacer la operación fácilmente
crv <- function(nombre_columna){
  return(revision_valores[[nombre_columna]])
}

##############
# Benthos
##############
###################################################
# Generando la tabla "Benthos_transect_sample_info"
###################################################
# Supuestos:
# 1. Cada muestreo de benthos en transecto realizado fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más un muestreo de bentos asociado (por
# ejemplo, no es válido tener PIT y LIT sobre el mismo transecto)
# Nota: posteriormente, para crear todas las tablas asociadas a
# "Benthos_transect_sample_info" se tendrá que segmentar aún más la tabla creada
# en esta sección.

muestreo_bentos_transecto_llave_primaria <- datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "BENTOS_DESAGREGADOS_V2") %>%
  elimina_columnas_vacias() %>%
  # Segundo supuesto:
  genera_llave("id_muestreo_bentos_transecto", "id_muestreo_transecto") %>%
  # Para que sea más natural la llave "id_punto_muestreo_bentos"
  arrange(id_muestreo_bentos_transecto, serie) %>%
  genera_llave("id_punto_muestreo_bentos") # Que sea una simple cuenta en órden

lista_columnas_benthos_transect_sample_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  sampling_method = "metodo",
  data_aggregation_level = "nivel_agregacion_datos", # Cambios en el esquema!
  surveyor = "observador",
  start_depth_m = "profundidad_inicial_m", # Cambios en el esquema!
  end_depth_m = "profundidad_final_m", # Cambios en el esquema!
  sampled_length_m = "longitud_transecto_m", # Cambios en el esquema!
  puntos_o_cm_reales_transecto = "puntos_o_cm_reales_transecto",
  sampling_completed = "muestreo_transecto_completo",
  comments = "strings_vacios"
)

benthos_transect_sample_info <- genera_tabla_2(
  df = muestreo_bentos_transecto_llave_primaria,
  nombre_columna_llave = "id_muestreo_bentos_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_benthos_transect_sample_info
) %>%
  mutate(
    # Primero recalculo y luego renombro para que la columna se quede en el
    # mismo lugar
    puntos_o_cm_reales_transecto = (sampled_length_m / puntos_o_cm_reales_transecto) * 100
  ) %>%
  rename(
    distance_between_points_if_pit_cm = puntos_o_cm_reales_transecto # Cambios en el esquema!
  ) %>%
  cambia_na_strings_vacios()

####################################################
# Generando la tabla "Benthos_transect_sample_point"
####################################################

lista_columnas_benthos_transect_sample_point <- list(
  benthos_transect_sample_info_id = "id_muestreo_bentos_transecto",
  # Para generar el número de punto y preservar el órden:
  serie = "serie",
  species_code = "codigo",
  height_if_algae_cm  = "altura_algas_cm" # Cambios en el esquema!
)

benthos_transect_sample_point <- genera_tabla(
  df = muestreo_bentos_transecto_llave_primaria,
  nombre_columna_llave = "id_punto_muestreo_bentos",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_benthos_transect_sample_point
) %>%
  # generando el campo "point_no", con ayuda del órden en "serie".
  ddply(.(benthos_transect_sample_info_id), function(df){
    resultado <- df %>%
      arrange(serie) %>%
      mutate(
        serie = 1:nrow(.)
      ) %>%
      # Para que no cambie de lugar las columnas primero mutate y luego rename
      rename(
        point_no = serie
      )
    
    return(resultado)
  })# %>%
  # Creo que en esta tabla no debe haber strings vacíos.
  # cambia_na_strings_vacios()

##############
# Corales
##############
###################################################
# Generando la tabla "Coral_transect_sample_info"
###################################################
# Supuestos:
# 1. Cada muestreo de corales en transecto realizado fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más un muestreo de corales asociado
# 3. Por ahora, todos los transectos tienen observaciones
# Nota: posteriormente, para crear todas las tablas asociadas a
# "Coral_transect_sample_info" se tendrá que segmentar aún más la tabla creada
# en esta sección.


muestreo_corales_transecto_llave_primaria <- datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "CORALES_DESAGREGADOS_V2") %>%
  elimina_columnas_vacias() %>%
  # Segundo supuesto:
  genera_llave("id_muestreo_corales_transecto", "id_muestreo_transecto") %>%
  # Para que sea más natural la llave "id_observacion_coral"
  arrange(id_muestreo_corales_transecto, serie) %>%
  genera_llave("id_observacion_coral") # Que sea una simple cuenta en órden

lista_columnas_coral_transect_sample_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  sampling_method = "metodo", # Cambios en el esquema!
  data_aggregation_level = "nivel_agregacion_datos", # Cambios en el esquema!
  surveyor = "observador",
  start_depth_m = "profundidad_inicial_m", # Cambios en el esquema!
  end_depth_m = "profundidad_final_m", # Cambios en el esquema!
  # Es la longitud real muestreada, no la teórica, la que nos interesa
  sampled_length_m = "longitud_transecto_m", # Cambios en el esquema!
  sampled_width_m = "ancho_transecto_m", # Cambios en el esquema!
  # Igual y "sampled_area" y "counted_area" se eliminan.
  sampling_completed = "muestreo_transecto_completo",
  comments = "strings_vacios"
)

coral_transect_sample_info <- genera_tabla_2(
  df = muestreo_corales_transecto_llave_primaria,
  nombre_columna_llave = "id_muestreo_corales_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_coral_transect_sample_info
  ) %>%
  cambia_na_strings_vacios()

####################################################
# Generando la tabla "Coral_transect_sample_observation"
####################################################

lista_columnas_coral_transect_sample_observation <- list(
  coral_transect_sample_info_id = "id_muestreo_corales_transecto",
  # Para generar el número de punto y preservar el órden:
  serie = "serie",
  species_code = "codigo",
  ifc = "clump",
  # Posiblemente haya que cambiar el nombre a los dos siguientes campos, para que
  # quede estandarizado cuál es la medida más grande y cuál la más chica.
  length_cm = "d1_max_diam_cm", # Cambios en el esquema!
  width_cm = "d2_min_diam_cm", # Cambios en el esquema!
  height_cm = "altura_maxima_cm", # Cambios en el esquema!
  bleaching_type = "blanqueamiento",
  bleaching_percentage = "porcentaje",
  recent_mortality_percentage = "mortalidad_reciente",
  trans_mortality_percentage = "mortalidad_transicion",
  old_mortality_percentage = "mortalidad_antigua",
  total_mortality_percentage = "mortalidad_total", # Cambios en el esquema!
  diseases = "enfermedades", # Cambios en el esquema!
  overgrowth = "sobrecrecimiento", # Cambios en el esquema"
  predation = "depredacion",
  # Falta injury
  comments = "strings_vacios"
)

coral_transect_sample_observation <- genera_tabla(
  df = muestreo_corales_transecto_llave_primaria,
  nombre_columna_llave = "id_observacion_coral",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_coral_transect_sample_observation
) %>%
  # generando el campo "point_no", con ayuda del órden en "serie".
  ddply(.(coral_transect_sample_info_id), function(df){
    resultado <- df %>%
      arrange(serie) %>%
      mutate(
        serie = 1:nrow(.)
      ) %>%
      # Para que no cambie de lugar las columnas primero mutate y luego rename
      rename(
        observation_no = serie
      )
    
    return(resultado)
  }) %>%
  cambia_na_strings_vacios()

################
# Invertebrados
################
########################################################
# Generando la tabla "Invertebrate_transect_sample_info"
########################################################
# Supuestos:
# 1. Cada muestreo de invertebrados realizado fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más un muestreo de invertebrados asociado
# 3. Este script ya acepta muestreos de invertebrados en transectos sin observaciones

muestreo_invertebrados_transecto_llave_primaria <- datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "INVERTEBRADOS_DESAGREGADOS_V2") %>%
  elimina_columnas_vacias() %>%
  # Segundo supuesto:
  genera_llave("id_muestreo_invertebrados_transecto", "id_muestreo_transecto")

lista_columnas_invertebrate_transect_sample_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  sampling_method = "metodo", # Cambios en el esquema!
  data_aggregation_level = "nivel_agregacion_datos", # Cambios en el esquema!
  surveyor = "observador",
  start_depth_m = "profundidad_inicial_m", # Cambios en el esquema!
  end_depth_m = "profundidad_final_m", # Cambios en el esquema!
  # Es la longitud real muestreada, no la teórica, la que nos interesa
  sampled_length_m = "longitud_transecto_m", # Cambios en el esquema!
  sampled_width_m = "ancho_transecto_m", # Cambios en el esquema!
  # Creo que el campo "invertebrates_sampled" se debe hacer más específico, porque
  # es muy general. Además, pueden ser varios campos como:
  # invertebrates_sampled_aggra = TRUE, lion_fish_sampled = TRUE,
  # lobsters_sampled_not_agrra = FALSE, etc. 
  # Para el proyecto CONACyT / GreenPeace, siempre se muestreó
  # el pez león pues es metodología AGRRA.
  # El transecto lo queremos para ver si se los invertebrados muestreados fueron
  # los de AGRRA, o además, otros.
  transecto = "transecto", #!!!
  # Creo que "lion_fish_count" debe pasarse a
  # "Invertebrate_transect_sample_count"
  sampling_completed = "muestreo_transecto_completo",
  comments = "notas" # !!! Aquí sí hay comentarios"
)

invertebrate_transect_sample_info <- genera_tabla_2(
  df = muestreo_invertebrados_transecto_llave_primaria,
  nombre_columna_llave = "id_muestreo_invertebrados_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_invertebrate_transect_sample_info
  ) %>%
  # generando propuestas para modificar los campos de "invertebrates_sampled"
  # "lion_fish_sampled", etc.
  mutate(
    tipo_transecto = ifelse(stri_detect_fixed(transecto, "bentos"), "bentos", "peces"),
    agrra_invertebrates_sampled = "TRUE",
    other_invertebrates_sampled = ifelse(tipo_transecto == "bentos", FALSE, TRUE),
    lion_fish_sampled = ifelse(tipo_transecto == "bentos", TRUE, FALSE)
  ) %>% #!!!
  select(
    id,
    transect_sample_id,
    sampling_method,
    data_aggregation_level,
    surveyor,
    start_depth_m,
    end_depth_m,
    sampled_length_m,
    sampled_width_m,
    agrra_invertebrates_sampled,
    other_invertebrates_sampled,
    lion_fish_sampled,
    sampling_completed,
    comments
  ) %>%
  cambia_na_strings_vacios()


################################################################
# Generando la tabla "Invertebrate_transect_sample_observation"
################################################################

# Para que sea más natural la llave "id_observacion_invertebrados"
# Cambios en el esquema! Se agrega una nueva tabla (a discutir con Lorenzo)
observacion_invertebrados_transecto_llave_primaria <-
  muestreo_invertebrados_transecto_llave_primaria %>%
  # Filtrando información de transectos sin observaciones
  filter(especie != "NA") %>%
  arrange(id_muestreo_invertebrados_transecto, serie) %>%
  genera_llave("id_observacion_invertebrado") # Que sea una simple cuenta en órden

lista_columnas_invertebrate_transect_sample_observation <- list(
  invertebrate_transect_sample_info_id = "id_muestreo_invertebrados_transecto",
  # Para generar el número de punto y preservar el órden:
  serie = "serie",
  scientific_name = "especie"
)

invertebrate_transect_sample_observation <- genera_tabla(
  df = observacion_invertebrados_transecto_llave_primaria,
  nombre_columna_llave = "id_observacion_invertebrado",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_invertebrate_transect_sample_observation
  ) %>%
  # generando el campo "point_no", con ayuda del órden en "serie".
  ddply(.(invertebrate_transect_sample_info_id), function(df){
    resultado <- df %>%
      arrange(serie) %>%
      mutate(
        serie = 1:nrow(.)
      ) %>%
      # Para que no cambie de lugar las columnas primero mutate y luego rename
      rename(
        observation_no = serie
      )
    
    return(resultado)
  }) %>%
  cambia_na_strings_vacios()

################
# Peces
################
########################################################
# Generando la tabla "Fish_transect_sample_info"
########################################################
# Supuestos:
# 1. Cada muestreo de peces realizado fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más un muestreo de peces asociado
# 3. Por ahora, todos los transectos tienen observaciones

muestreo_peces_transecto_llave_primaria <- datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2") %>%
  elimina_columnas_vacias() %>%
  # Segundo supuesto:
  genera_llave("id_muestreo_peces_transecto", "id_muestreo_transecto")

lista_columnas_fish_transect_sample_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  sampling_method = "metodo", # Cambios en el esquema!
  data_aggregation_level = "nivel_agregacion_datos", # Cambios en el esquema!
  surveyor = "observador",
  # Los siguientes campos no se tienen para el proyecto CONACyT / GreenPeace.
  #start_depth_m = "profundidad_inicial_m", # Cambios en el esquema!
  #end_depth_m = "profundidad_final_m", # Cambios en el esquema!
  # Es la longitud real muestreada, no la teórica, la que nos interesa
  sampled_length_m = "longitud_transecto_m", # Cambios en el esquema!
  sampled_width_m = "ancho_transecto_m", # Cambios en el esquema!
  sampling_completed = "muestreo_transecto_completo",
  comments = "strings_vacios"
)

fish_transect_sample_info <- genera_tabla_2(
  df = muestreo_peces_transecto_llave_primaria,
  nombre_columna_llave = "id_muestreo_peces_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_fish_transect_sample_info
  ) %>%
  # generando una propuesta del campo "sampled_fish"
  mutate(
    sampled_fish = "AGRRA_V5"
  ) %>% #!!!
  select(
    id,
    transect_sample_id,
    sampling_method,
    data_aggregation_level,
    surveyor,
    sampled_length_m,
    sampled_width_m,
    sampled_fish,
    sampling_completed,
    comments
  ) %>%
  cambia_na_strings_vacios()

################################################################
# Generando la tabla "Fish_transect_sample_count"
################################################################

conteo_peces_transecto_llave_primaria <-
  muestreo_peces_transecto_llave_primaria %>%
  select(
    id,
    serie,
    codigo,
    especie,
    dplyr::contains("tamanio"),
    id_muestreo_peces_transecto
  ) %>%
  
  # Supuesto 3: no hay muestreos de peces en transectos sin observaciones, para
  # CONACyT / GreenPeace
  # Filtrando información de transectos sin observaciones.
  #filter(especie != "NA") %>%
  
  ## Generando columnas de "min_size", "max_size" y "count".
  gather(key = categoria_tamanio, value = cuenta, dplyr::contains("tamanio")) %>%
  # Filtrando tamaños no encontrados en un mismo conteo de especie en transecto.
  filter(!is.na(cuenta)) %>%
  # Generando campos de "tamanio_minimo_cm" y "tamanio_maximo_cm" # Cambios en el esquema!
  separate(col = categoria_tamanio, into = c("etiqueta", "tamanio_minimo_cm", "tamanio_maximo_cm")) %>%
  mutate(
    # Quitándoles la etiqueta de "cm" a "tamanio_minimo_cm" y "tamanio_maximo_cm"
    tamanio_minimo_cm = stri_sub(tamanio_minimo_cm, from = 1, to = (stri_length(tamanio_minimo_cm)-2)) %>%
      as.numeric(),
    tamanio_maximo_cm = stri_sub(tamanio_maximo_cm, from = 1, to = (stri_length(tamanio_maximo_cm)-2)) %>%
      as.numeric(),
    es_juvenil = NA #lógico
  ) %>%
  # Suponemos que para cada "muestreo_peces_transecto", sólo debe haber un registro
  # por código de especie y talla. Si hay más, se toma el primero (provisionalmente,
  # Esme y Nuria lo tienen que checar.)
  genera_llave(
    "id_conteo_especie_talla",
    "id_muestreo_peces_transecto",
    "codigo",
    "es_juvenil",
    "tamanio_minimo_cm",
    "tamanio_maximo_cm"
    )

lista_columnas_fish_transect_sample_count <- list(
  fish_transect_sample_info_id = "id_muestreo_peces_transecto",
  species_code = "codigo",
  is_juvenile = "es_juvenil",
  min_size_cm = "tamanio_minimo_cm",
  max_size_cm = "tamanio_maximo_cm",
  count = "cuenta"
  )

fish_transect_sample_count <- genera_tabla(
  df = conteo_peces_transecto_llave_primaria,
  nombre_columna_llave = "id_conteo_especie_talla",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_fish_transect_sample_count
  )

################
# Complejidad
################
########################################################
# Generando la tabla "Complexity_transect_sample_info"
########################################################
# Supuestos:
# 1. Cada muestreo de complejidad realizado fue registrado en los Exceles
# correspondientes.
# 2. Cada muestreo de transecto tiene a lo más un muestreo de complejidad asociado

muestreo_complejidad_transecto_llave_primaria <- datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "RUGOSIDAD_DESAGREGADA_V2") %>%
  elimina_columnas_vacias() %>%
  # Segundo supuesto:
  genera_llave("id_muestreo_complejidad_transecto", "id_muestreo_transecto")

lista_columnas_complexity_transect_sample_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  
  # El siguiente campo no es efectivo en este caso, pues la complejidad se
  # puede tomar de varias formas en un mismo transecto.
  # sampling_method = "metodo", # Cambios en el esquema!
  
  # El siguiente campo no es necesario para complejidad por transecto, pero sí
  # para complejidad por cuadrantes, en el caso de que, por ejemplo, se reporte
  # la media y sd de rugosidad. En lugar de los valores de rugosidad de cuadrantes
  # individuales
  # data_aggregation_level = "nivel_agregacion_datos", # Cambios en el esquema!
  
  surveyor = "observador",
  start_depth_m = "profundidad_inicial_m", # Cambios en el esquema!
  end_depth_m = "profundidad_final_m", # Cambios en el esquema!
  # Es la longitud real muestreada, no la teórica, la que nos interesa
  sampled_length_m = "longitud_transecto_m", # Cambios en el esquema!
  
  # El siguiente campo sólo cobra sentido si existen submuestreos (no cuadrantes
  # que ya van asociados a "Transect_sample") donde se midió complejidad.
  # Sampling_completed = "muestreo_transecto_completo",
  
  rugosity_contour_length_m = "tamanio_cadena_m", # Cambios en el esquema!
  rugosity_linear_length_m = "longitud_transecto_m", #!!!
  
  comments = "strings_vacios"
)

complexity_transect_sample_info <- genera_tabla(
  df = muestreo_complejidad_transecto_llave_primaria,
  nombre_columna_llave = "id_muestreo_complejidad_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_complexity_transect_sample_info
  ) %>%
  cambia_na_strings_vacios()

############
# Reclutas
############
####################################################################
# Generando la tabla "Recruit_subquadrat_sample_from_transect_info"
####################################################################
# Cambios en el esquema! Esta tabla va ligada ahora a "Transect_sample", y contiene
# un registro por cuadrante.

# Supuestos:
# 1. Cada muestreo realizado de reclutas en cuadrante fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada cuadrante tiene a lo más un muestreo de reclutas asociado (esto es obvio)
# 3. Este script ya acepta muestreos de reclutas en cuadrantes sin observaciones

muestreo_reclutas_cuadrante_llave_primaria <-
  datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2") %>%
  #elimina_columnas_vacias() %>% # Se elimina para no quitar "na_numerico"
  # Segundo supuesto:
  genera_llave("id_muestreo_reclutas_cuadrante", "id_muestreo_transecto", "cuadrante")

lista_columnas_recruit_subquadrat_sample_from_transect_info <- list(
  transect_sample_id = "id_muestreo_transecto",
  quadrat_no = "cuadrante",
  # sampling_method y data_aggregation_level creo que son innecesarios para reclutas
  # tomados en un cuadrante, ya que no hay muchas opciones: siempre están agregados
  # por especie (pues no hay orden), y siempre se cuentan.
  surveyor = "observador",
  maximum_recruit_size_cm = "na_numerico",
  maximum_small_coral_size_cm = "na_numerico",
  substratum = "sustrato",
  comments = "strings_vacios"
)

recruit_subquadrat_sample_from_transect_info <- genera_tabla(
  df = muestreo_reclutas_cuadrante_llave_primaria,
  nombre_columna_llave = "id_muestreo_reclutas_cuadrante",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_recruit_subquadrat_sample_from_transect_info
)

####################################################################
# Generando la tabla "Recruit_subquadrat_sample_from_transect_count"
####################################################################

recruit_subquadrat_sample_from_transect_count <- muestreo_reclutas_cuadrante_llave_primaria %>%
  elimina_columnas_vacias() %>%
  # Filtrando los cuadrantes sin observaciones:
  filter(!is.na(codigo)) %>%
  # Agregando los datos por cuadrante, especie y categoria_tamanio
  group_by(id_muestreo_reclutas_cuadrante, codigo, categoria_tamanio) %>%
    summarise(
      cuenta = n()
    ) %>%
  ungroup() %>%
  arrange(id_muestreo_reclutas_cuadrante, codigo, categoria_tamanio) %>%
  genera_llave("id") %>%
  select(
    id,
    recruit_subquadrat_sample_from_transect_info_id = id_muestreo_reclutas_cuadrante,
    species_code = codigo,
    size_category = categoria_tamanio,
    count = cuenta
  )

# save(
#   benthos_transect_sample_info,
#   benthos_transect_sample_point,
#   coral_transect_sample_info,
#   coral_transect_sample_observation,
#   invertebrate_transect_sample_info,
#   invertebrate_transect_sample_observation,
#   fish_transect_sample_info,
#   fish_transect_sample_count,
#   complexity_transect_sample_info,
#   recruit_subquadrat_sample_from_transect_info,
#   recruit_subquadrat_sample_from_transect_count,
#   file = "../productos/tablas_muestreo_aspectos_transecto.RData"
# )
  
#############
# Catálogos
#############
# - Project.purpose (tema)
# - Project.site_selection_method (metodo_seleccion_sitios)
# - Site_sample.country (pais)
# - Site_sample.healthy_reefs_region (region_healthy_reefs)
# - Site_sample.location (localidad)
# - Site_sample.methodology (protocolo_muestreo_sitio)
# - de las variables elegidas entre Site_sample.reef_type (tipo_arrecife),
#   Site_sample.reef_zone (zona_arrecifal), Site_sample.subzone_habitat (subzona_habitat)
# - Site_sample.protected_area (anp)
# - Benthos_transect_sample_info.sampling_method (método en exceles de bentos)
# - Benthos_transect_sample_info.data_aggregation_level (nivel_agregacion_datos
#   en exceles de bentos)
# - Benthos_transect_sample_point.species_code (codigo)
# - Coral_transect_sample_info.sampling_method (método en exceles de corales)
# - Coral_transect_sample_info.data_aggregation_level (nivel_agregacion_datos
#   en exceles de corales).
# - Coral_transect_sample_observation.species code (código de especie en exceles
#   de corales)
# - Coral_transect_sample_observation.bleaching_type (blanqueamiento)
# - Coral_transect_sample_observation.diseases (enfermedades)
# - Coral_transect_sample_observation.overgrowth (sobrecrecimiento)
# - Coral_transect_sample_observation.predation (depredación)
# - Invertebrate_transect_sample_info.sampling_method (método en exceles de invertebrados)
# - Invertebrate_transect_sample_info.data_aggregation_level (nivel_agregacion_datos
#   en exceles de invertebrados)
# - Definir columnas booleanas de Invertebrate_transect_sample_info.tipo_invertebrados_muestreados.
# - Invertebrate_transect_sample_observation.scientific_name
# (catálogo de nombres científicos de invertebrados)
# - Fish_transect_sample_info.sampling_method (método en exceles de peces)
# - Fish_transect_sample_info.data_aggregation_level (nivel de agregación de datos
#   en exceles de peces)
# - Fish_transect_sample_info.sampled_fish (peces muestreados: AGRRA, ReefBudget, todos)
# - Fish_transect_sample_count.species_code (codigo)
# - Fish_transect_sample_count.species (especie)
# - Recruit_subquadrat_sample_from_transect_info.substratum (sustrato)
# - Recruit_subquadrat_sample_from_transect_count.species_code (codigo)
# - Recruit_subquadrat_sample_from_transect_count.size_category (categoria_tamanio)

#######################################
# Comentarios (a consultar con Lorenzo)
#######################################

# 1. Tal vez el método de selección de cada sitio se debe dar por sitio, porque
# así puede ser más preciso. Ésto depende si en un mismo proyecto pueden haber
# sitios seleccionados de más de una forma (estratégico, aleatorio, etc).
# Y de la frecuencia con que esto pase. Lorenzo dice que por sitio está bien.
# Se queda el criterio de la primera vez que se definió un sitio en remuestreos.

# 2. Creo que protocolo a nivel de Sitio debe ser muy general, por ejemplo:
# “Otros”, “AGRRA_v5", “AGRRA_v5+” (AGRRA_v5 y adicionales). Propongo dejar los
# detalles a nivel muestreo de un aspecto, es decir, a nivel de las tablas “...info”.
# Por ello, se elimina el campo “Site_sample.methodology_details” (así se realizó para v2).
# Perfecto: Esme y Nuria proponen una y Lorenzo la revisa.

# 3. Después de mucho pensar, creo que la columna “data_aggregation_level” debe
# estar a nivel de las tablas de “..._info”, por ejemplo, “Coral_transect_sample_info”,
# Recruit_subquadrat_sample_from_transect_info”, etc, ya que estas tablas son
# las que realmente especifican cada aspecto muestreado en un sitio (independientemente
# de si hubo observaciones o no). Por medio del nombre de la tabla + la variable
# “data_aggregation_level”, se puede saber para un sitio/transecto/etc:
#   1. Qué aspectos se muestrearon.
#   2. El nivel biológico de agregación de la información (individuo /
#     agregados por especie, etc).
#   3. El nivel geográfico de agregación de la información: cuadrante, transecto,
#   sitio, etc...
# Nota: Benthos agregados por porcentajes para homologar distintos tipos de
# datos. Promedios, desviación estándar, número de transectos.

# Para visualizar esta abstracción, recordar que un join es ENTRE TABLAS, entonces,
# por ejemplo, los registros en un join entre “Site_sample”, “Transect_sample”
# y “Fish_transect_sample_info”, con “data_aggregation_level” = “por especie” en
# esta última tabla, especifican sitios donde la información de PECES (1) está
# agregada a nivel de ESPECIE (2), por transecto (3) (así se realizó para v2).

# 4. Revisar lo del pez león en invertebrados, ya que está muy raro. Agregarlo
# al catálogp de invertebrados con el nombre de pez león AGRRA.

# 5. Es necesario que el campo de “invertebrates_sampled” sea más específico,
# por ejemplo, dividirlo en “agrra_invertebrates_sampled”, “lion_fish_sampled,
# “other_invertebrates_sampled” (que posiblemente se tenga que dividir, etc.).
# (se puso un ejemplo de esto en v2). Lorenzo lo checa con Esme y Nuri, pero
# está de acuerdo con categorías muy generales y una categoría de otros que se
# pongan a mano.

# 6. Los datos de invertebrados muestreados para el proyecto CONACyT / GreenPeace
# están por observaciones, si hay muchos así, conviene hacer una tabla para
# invertebrados por observaciones (no por agregados de especies, o ambas)
# (así se realizó para v2). Lorenzo dice que no es necesario, agregarlos por
# especies.

# 7. Para la tabla “Fish_transect_sample_info”, es necesario especificar qué especies
# se muestrearon. Para ello, necesito un catálogo de “sampled_fish”
# (AGRRA, ReefBudget, etc). Así se realizó para v2. Se creará el catálogo.

# 8. Consultar con Lorenzo si él cree que puede haber varias medidas de complejidad
# implementadas en el mismo transecto, en este caso, habría que desechar la idea
# de meter el campo de “sampling_method” (así se realizó para v2). Quitarlo.

# 9. Checar con L/E/N si “start_depth_m” y “end_depth_m” pueden ser declaradas
# a nivel de transecto, así como la longitud teórica del mismo. Checar si
# “temperature_c” está bien a nivel de muestreo de sitio. Esme ya me confirmó
# que los 3 campos deben estar a nivel de transecto.

# 10. Cambios masivos para simplificar cuadrantes en transectos: la tabla
# “Subquadrat_samples_from_transect_info” desaparece, y se une a “Transect_sample”
# Es una relación uno - uno (o cero). La tabla “Subquadrat_sample_from_transect”
# desaparece, ya que el nombre de cada subcuadrante es autogenerado y no tiene
# mucho sentido, entonces la info de reclutas queda asociada al transecto, con el
# nombre (autogenerado) de cada cuadrante en un nuevo campo, y la cuenta de reclutas
# queda asociada a su info correspondiente (así se realizó para v2).
# Si Esme me contesta que es muy improbable que más de una persona haga un mismo
# cuadrante, y además maximum_relief siempre es por cuadrantes, entonces la tabla
# de “Complexity_transect_maximum_relief_measurement” se une directo a transecto.
# Esme me contestó que es muy improbable que varias personas realicen el mismo
# cuadrante, entonces se realizará como lo anterior.
# Lorenzo: lo único que importa es la longitud del transecto y cuántos cuadrantes hubo
# Quitar el campo "Transect_sample.distance_between_centers".

# 11. Pensar si se necesitan los campos “Transect_sample.random_selection_centers”
# y “Transect_sample.distance_between_centers”. Ver punto 11.

# 12. Igual y en “Transect_sample” se puede incluir el campo que especifique que
# el transecto es fijo entre remuestreos. Sí, perfecto.

# 13. Necesitamos mejorar el campo “Project.site_selection_method”, ya que es crucial
# para la correcta aplicación de la teoría de muestreo al análisis de esta base de
# datos. Una solución que se me ocurre es poner una columna "project.sample_stratum",
# y cada estrato del mismo proyecto (sea espacial o temporal) va a tener su propio
# registro en la tabla. Dentro de cada estrato, los sitios deben tomarse exáctamente
# de la misma forma: muestreo aleatorio simple, estratégico para muestra representativa,
# estratégico por ser sitio de importancia para la biodiversidad, etc (a definir menú).
# Cambiar nombre de la tabla de Project a Project_stratum, para dar al estrato la importancia
# que se merece. Mejor "site_selection_method" será por sitio.

# 14. Para el sesgo por observadores, igual y conviene tener un catálogo de
# observadores (independiente del login del cliente), y que el observador se seleccione
# de ahí. Mejor dar una regla tipo AGRRA: las dos primeras letras del nombre y
# las dos primeras del apellido.

# 15. Igual y convendría poner un campo de "método de selección de transectos".
# Se pueden pensar 2 categorías: definidos en sitio, al azar (previamente).

# 16. Igual y convendría quitar la profundidad media por sitio si ésta se tiene
# a nivel transecto. Sin embargo, este campo sería útil en el caso de sitios muestreados
# sin transecto (como los de Mau), así como datos tomados en transecto pero agregados
# a nivel de sitio. Conviene dejarlo. Dejarla.