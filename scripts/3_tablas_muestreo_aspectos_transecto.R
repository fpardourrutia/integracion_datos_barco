# En este script se generarán las tablas de muestreos de aspectos a nivel de transecto
# Es decir, todas las llamadas "...transect_sample_info" y asociadas a ellas

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

benthos_transect_sample_info <- genera_tabla(
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

coral_transect_sample_info <- genera_tabla(
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

invertebrate_transect_sample_info <- genera_tabla(
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

fish_transect_sample_info <- genera_tabla(
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
    contains("tamanio"),
    id_muestreo_peces_transecto
  ) %>%
  
  # Supuesto 3: no hay muestreos de peces en transectos sin observaciones, para
  # CONACyT / GreenPeace
  # Filtrando información de transectos sin observaciones.
  #filter(especie != "NA") %>%
  
  ## Generando columnas de "min_size", "max_size" y "count".
  gather(key = categoria_tamanio, value = cuenta, contains("tamanio")) %>%
  # Filtrando tamaños no encontrados en un mismo conteo de especie en transecto.
  filter(!is.na(cuenta)) %>%
  # Generando campos de "min_size_cm" y "max_size_cm" # Cambios en el esquema!
  separate(col = categoria_tamanio, into = c("etiqueta", "min_size_cm", "max_size_cm")) %>%
  mutate(
    # Quitándoles la etiqueta de "cm" a "min_size" y "max_size"
    min_size_cm = stri_sub(min_size_cm, from = 1, to = (stri_length(min_size_cm)-2)) %>%
      as.numeric(),
    max_size_cm = stri_sub(max_size_cm, from = 1, to = (stri_length(max_size_cm)-2)) %>%
      as.numeric()
  ) %>%
  # Suponemos que para cada "muestreo_peces_transecto", sólo debe haber un registro
  # por código de especie y talla. Si hay más, se toma el primero (provisionalmente,
  # Esme y Nuria lo tienen que checar.)
  genera_llave(
    "id_conteo_especie_talla",
    "id_muestreo_peces_transecto",
    "codigo",
    "min_size_cm",
    "max_size_cm" #!!! no se ha generado por "is_juvenile"
    )

lista_columnas_fish_transect_sample_count <- list(
  fish_transect_sample_info_id = "id_muestreo_peces_transecto",
  species_code = "codigo",
  min_size_cm = "min_size_cm",
  max_size_cm = "max_size_cm",
  count = "cuenta"
  )

fish_transect_sample_count <- genera_tabla(
  df = conteo_peces_transecto_llave_primaria,
  nombre_columna_llave = "id_conteo_especie_talla",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_fish_transect_sample_count
  ) %>%
  mutate(
    is_juvenile = as.logical(NA)
  ) %>%
  select(
    id,
    fish_transect_sample_info_id,
    species_code,
    is_juvenile,
    min_size_cm,
    max_size_cm,
    count
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
  
  # El siguiente campo sólo cobra sentido si existen cuadrantes donde se midió
  # complejidad.
  # sampling_completed = "muestreo_transecto_completo",
  
  rugosity_contour_length_m = "tamanio_cadena_m", # Cambios en el esquema!
  rugosity_linear_length_m = "longitud_transecto_m", #!!!
  
  comments = "strings_vacios"
)

rugosity_transect_sample_info <- genera_tabla(
  df = muestreo_complejidad_transecto_llave_primaria,
  nombre_columna_llave = "id_muestreo_complejidad_transecto",
  nombre_nuevo_columna_llave = "id",
  lista_columnas_adicionales = lista_columnas_complexity_transect_sample_info
  ) %>%
  cambia_na_strings_vacios()

############
# Reclutas
############
############################################################
# Generando la tabla "Subquadrat_samples_from_transect_info"
############################################################

# Supuestos:
# 1.La información de subcuadrantes 
# 2. Cada muestreo de transecto da lugar a, a lo más, un registro de información
# de subcuadrantes.

informacion_muestras_subcuadrantes_transecto_llave_primaria <- datos_globales_transecto_llave_primaria %>%
  filter(archivo_origen == "RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2") %>%
  elimina_columnas_vacias() %>%
  # Segundo supuesto:
  genera_llave("id_muestreo_peces_transecto", "id_muestreo_transecto")





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
# - Fish_transect_sample_count.species_code (codigo)
# - Fish_transect_sample_count.species (especie)

#######################################
# Comentarios (a consultar con Lorenzo)
#######################################
# 1. Tal vez el método de selección de cada sitio se debe dar por sitio, porque
# así puede ser más preciso. Ésto depende si en un mismo proyecto pueden haber
# sitios seleccionados de más de una forma (estratégico, aleatorio, etc).
# Y de la frecuencia con que esto pase.
# 2. Creo que protocolo a nivel de Sitio debe ser muy general, por ejemplo:
# "Otros", "AGRRA_v5", "AGRRA_v5+" (AGRRA_v5 y adicionales). Propongo dejar los
# detalles a nivel muestreo de un aspecto, es decir, a nivel de las tablas "...info".
# Por ello, se elimina el campo "Site_sample.methodology_details".
# 3. Después de mucho pensar, creo que la columna "data_aggregation_level" debe
# estar a nivel de las tablas de "..._info", por ejemplo, "Coral_transect_sample_info",
#  Recruit_subquadrat_sample_from_transect_info", etc, ya que estas tablas son
# las que realmente especifican CADA ASPECTO MUESTREADO EN UN SITIO (independientemente
# de si hubo observaciones o no). Por medio del nombre de la tabla + la variable
# "data_aggregation_level", se puede saber para un sitio/transecto/etc:
#   1. Qué aspectos se muestrearon.
#   2. El nivel biológico de agregación de la información (individuo /
#   agregados por especie, etc).
#   3. El nivel geográfico de agregación de la información: cuadrante, transecto,
#   sitio, etc...
# Para visualizar esta abstracción, recordar que un join es ENTRE TABLAS, entonces,
# por ejemplo, los registros en un join entre "Site_sample", "Transect_sample"
# y "Fish_transect_sample_info", con "data_aggregation_level" = "por especie" en
# esta última tabla, especifican sitios donde la información de PECES (1) está
# agregada a nivel de ESPECIE (2), por transecto (3).
# 4. Revisar lo del pez león en invertebrados, ya que está muy raro.
# 5. Es necesario que el campo de "invertebrates_sampled" sea más específico,
# por ejemplo, dividirlo en "agrra_invertebrates_sampled", "lion_fish_sampled,
# "other_invertebrates_sampled" (que posiblemente se tenga que dividir, etc.)
# 6. Los datos de invertebrados muestreados para el proyecto CONACyT / GreenPeace
# están por observaciones, si hay muchos así, conviene hacer una tabla para
# invertebrados por observaciones (no por agregados de especies)
# 7. Para la tabla "Fish_transect_sample_info", es necesario especificar qué especies
# se muestrearon. Para ello, necesito un catálogo de "sampled_fish".
# 8. Consultar con Lorenzo si él cree que puede haber varias medidas de complejidad
# implementadas en el mismo transecto, en este caso, habría que desechar la idea
# de meter el campo de "sampling_method".
