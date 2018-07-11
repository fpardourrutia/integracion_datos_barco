# En este script se generarán las tablas de muestreos de aspectos a nivel de transecto
# Es decir, todas las llamadas "Muestreo_transecto_..._info" y asociadas a ellas.

# Supuesto para generar las tablas de aspecto (bentos, peces, corales, reclutas,
# complejidad, invertebrados):
# 1. Cada Excel tiene información correspondiente a un sólo nivel espacial de
# agrupación de datos (transecto / sitio) .
# 2. Cada Excel tiene información correspondiente a un sólo nivel biológico de
# agregación de datos (por observacion, coberturas por especie, etc)...
# Esto es necesario para generar las tablas de "..._info" (que tienen información)
# del nivel de agrupación geográfica de los datos en el nombre, y del nivel de
# agrupación biológica de los datos en el campo "...info.data_aggregation_level".

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
datos_globales_llaves_primarias <- readRDS("../productos/v3/datos_globales_llaves_primarias.RData")

# Revisando valores de las columnas de datos_globales:
revision_valores <- revisa_valores(datos_globales_llaves_primarias)
names(revision_valores)

# Función para consultar el objeto anterior:
# nombre_columna: nombre de la columna a consultar.
# La función regresa la tabla correspondiente a esa columna
# El nombre de esta función es muy rápido para hacer la operación fácilmente
crv <- function(nombre_columna){
  return(revision_valores[[nombre_columna]])
}

##############
# Bentos
##############
####################################################
# Generando la tabla "Muestra_transecto_bentos_info"
####################################################
# Supuestos:
# 1. Cada muestreo de benthos en transecto realizado fue registrado en los Exceles
# correspondientes (independientemente de si tuvo observaciones o no).
# 2. Cada muestreo de transecto tiene a lo más un muestreo de bentos asociado (por
# ejemplo, no es válido tener PIT y LIT sobre el mismo transecto)
# Nota: posteriormente, para crear todas las tablas asociadas a
# "Muestra_transecto_bentos_info" se tendrá que segmentar aún más la tabla creada
# en esta sección.

datos_muestra_transecto_bentos_llaves_primarias <- datos_globales_llaves_primarias %>%
  filter(archivo_origen %in% c(
    # Por flexibilidad
    "conacyt_greenpeace_2016_bentos_desagregados_v3"
  )) %>%
  # Segundo supuesto:
  genera_llave("muestra_transecto_bentos_info_id", "muestra_transecto_id") %>%
  # Para que sea más natural la llave "muestra_transecto_bentos_punto_id"
  arrange(muestra_transecto_bentos_info_id, serie) %>%
  genera_llave("muestra_transecto_bentos_punto_id") # Que sea una simple cuenta en órden

lista_columnas_muestra_transecto_bentos_info <- list(
  muestra_transecto_id = "muestra_transecto_id",
  metodo_muestreo = "metodo",
  nivel_agregacion_datos = "nivel_agregacion_datos",
  observador = "observador",
  longitud_muestreada_m = "longitud_transecto_m",
  distancia_entre_puntos_pit_cm = "distancia_entre_puntos_pit_cm",
  muestreo_completado = "muestreo_completo",
  comentarios = "strings_vacios"
)

muestra_transecto_bentos_info <- genera_tabla_2(
  df = datos_muestra_transecto_bentos_llaves_primarias,
  nombre_columna_llave = "muestra_transecto_bentos_info_id",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_muestra_transecto_bentos_info
  ) %>%
  cambia_na_strings_vacios()

######################################################
# Generando la tabla "Muestra_transecto_bentos_punto"
######################################################

lista_columnas_muestra_transecto_bentos_punto <- list(
  muestra_transecto_bentos_info_id = "muestra_transecto_bentos_info_id",
  # Para generar el número de punto y preservar el órden:
  serie = "serie",
  codigo = "codigo",
  altura_si_alga_cm  = "altura_algas_cm"
)

muestra_transecto_bentos_punto <- genera_tabla(
  df = datos_muestra_transecto_bentos_llaves_primarias,
  nombre_columna_llave = "muestra_transecto_bentos_punto_id",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_muestra_transecto_bentos_punto
) %>%
  
  # Generando el número de punto, utilizando el orden de la serie, y suponiendo
  # que sólo hay un muestreo de bentos en cada transecto. Esto se hace en este
  # momento por facilidad, ya que ya tenemos definido el campo:
  # "muestra_transecto_bentos_info_id"
  
  ddply(.(muestra_transecto_bentos_info_id), function(df){
    resultado <- df %>%
      arrange(serie) %>%
      # Para que no cambie de lugar las columnas primero mutate y luego rename
      mutate(
        serie = 1:nrow(.)
      ) %>%
      rename(
        numero_punto = serie
      )
    
    return(resultado)
  })# %>%
  # Creo que en esta tabla no debe haber strings vacíos.
  # cambia_na_strings_vacios()

### AQUÍ ME QUEDÉ
##############
# Corales
##############
#####################################################
# Generando la tabla "Muestra_transecto_corales_info"
#####################################################
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
  sampling_method = "metodo",
  data_aggregation_level = "nivel_agregacion_datos",
  surveyor = "observador",
  sampled_length_m = "longitud_transecto_m",
  sampled_width_m = "ancho_transecto_m",
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
  length_cm = "d1_max_diam_cm",
  width_cm = "d2_min_diam_cm",
  height_cm = "altura_maxima_cm",
  bleaching_type = "blanqueamiento",
  bleaching_percentage = "porcentaje",
  recent_mortality_percentage = "mortalidad_reciente",
  trans_mortality_percentage = "mortalidad_transicion",
  old_mortality_percentage = "mortalidad_antigua",
  total_mortality_percentage = "mortalidad_total",
  diseases = "enfermedades",
  overgrowth = "sobrecrecimiento",
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
  sampling_method = "metodo",
  data_aggregation_level = "nivel_agregacion_datos",
  surveyor = "observador",
  sampled_length_m = "longitud_transecto_m",
  sampled_width_m = "ancho_transecto_m",
  # Creo que el campo "invertebrates_sampled" se debe hacer más específico, porque
  # es muy general. Además, pueden ser varios campos como:
  # invertebrates_sampled_aggra = TRUE, lion_fish_sampled = TRUE,
  # lobsters_sampled_not_agrra = FALSE, etc. 
  # Para el proyecto CONACyT / GreenPeace, siempre se muestreó
  # el pez león pues es metodología AGRRA.
  # El transecto lo queremos para ver si se los invertebrados muestreados fueron
  # los de AGRRA, o además, otros.
  transecto = "transecto", #!!!
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
  sampling_method = "metodo",
  data_aggregation_level = "nivel_agregacion_datos",
  surveyor = "observador",
  sampled_length_m = "longitud_transecto_m",
  sampled_width_m = "ancho_transecto_m",
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
  
  # El siguiente campo no es necesario para complejidad por transecto, pero sí
  # para complejidad por cuadrantes, en el caso de que, por ejemplo, se reporte
  # la media y sd de rugosidad. En lugar de los valores de rugosidad de cuadrantes
  # individuales
  # data_aggregation_level = "nivel_agregacion_datos", # Cambios en el esquema!
  
  surveyor = "observador",
  sampled_length_m = "longitud_transecto_m",
  
  # El siguiente campo sólo cobra sentido si existen submuestreos (no cuadrantes
  # que ya van asociados a "Transect_sample") donde se midió complejidad.
  # Sampling_completed = "muestreo_transecto_completo",
  
  rugosity_contour_length_m = "tamanio_cadena_m",
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

save(
  muestra_transecto_bentos_info,
  muestra_transecto_bentos_punto,
  coral_transect_sample_info,
  coral_transect_sample_observation,
  invertebrate_transect_sample_info,
  invertebrate_transect_sample_observation,
  fish_transect_sample_info,
  fish_transect_sample_count,
  complexity_transect_sample_info,
  recruit_subquadrat_sample_from_transect_info,
  recruit_subquadrat_sample_from_transect_count,
  file = "../productos/v3/prueba_tablas_muestra_transecto_aspectos.RData"
)