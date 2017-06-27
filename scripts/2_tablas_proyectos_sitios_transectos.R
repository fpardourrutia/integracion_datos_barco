# En este script se generarán las tablas de "Project", "Site_sample" y "Transect_sample".

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

datos_globales_final <- readRDS("../productos/datos_globales_final.RData")
glimpse(datos_globales_final)

# Revisando valores de las columnas de datos_globales:
revision_valores <- revisa_valores(datos_globales_final)

# Función para consultar el objeto anterior:
# nombre_columna: nombre de la columna a consultar.
# La función regresa la tabla correspondiente a esa columna
# El nombre de esta función es muy rápido para hacer la operación fácilmente
crv <- function(nombre_columna){
  return(revision_valores[[nombre_columna]])
}

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
# EN Corales, ¿qué es "fisión" y "S"?

# Renombrando columnas apropiadamente para generar las tablas de Project

## 2. Creando la información de proyecto en dicha tabla
Project <- datos_globales_final %>%
  mutate(
    Project.name_aux = Nombre_del_Proyecto,
    Project.description_aux = Proposito,
    Project.purpose_aux = Tema,
    # Project.location_aux
    Project.start_year_aux = Fecha_de_inicio,
    Project.end_year_aux = Fecha_de_termino,
    Project.organization_aux = Institucion,
    Project.suborganization_aux = Suborganizacion,
    Project.person_in_charge_aux = Autor_Administrador_Del_Proyecto,
    Project.contact_aux = Contacto,
    Project.site_selection_method_aux = Metodo_de_Seleccion_de_Sitios,
    # comment_aux
    Project.reference_aux = Cita
    ) %>%
  # Generando los datos finales de la tabla "Project"
  mutate(
    Project.name = ifelse(stri_detect_coll(Project.name_aux, "247104"),
      "CONACYT 247104",
      "Greenpeace 2016"),
    Project.id = ifelse(Project.name == "CONACYT 247104", 1, 2)
  ) %>%
  group_by(Project.id) %>%
  mutate(
    Project.description = first(Project.description_aux),
    Project.purpose = first(Project.purpose_aux),
    # Definiendo las fecha de incio y término del proyecto a mano. Lorenzo dijo
    # que con el año es suficiente.
    Project.start_year = "2016",
    Project.end_year = ifelse(Project.name == "CONACYT 247104",
      NA, "2016"),
    Project.organization = first(Project.organization_aux),
    Project.suborganization = first(Project.suborganization_aux),
    Project.person_in_charge = first(Project.person_in_charge_aux),
    Project.contact = first(Project.contact_aux),
    Project.site_selection_method = "Estratégico",
    Project.reference = first(Project.reference_aux)
  ) %>%
  ungroup() %>%
  select(
    id,
    archivo_origen,
    everything()
  )

# Construyendo la tabla "project"
project <- datos_globales_proyecto %>%
  select(
    id = Project.id,
    name = Project.name,
    description = Project.description,
    purpose = Project.purpose,
    start_year = Project.start_year,
    end_year = Project.end_year,
    organization = Project.organization,
    suborganization = Project.suborganization,
    person_in_charge = Project.person_in_charge,
    contact = Project.contact,
    site_selection_method = Project.site_selection_method,
    reference = Project.reference
  ) %>%
  unique()

## 3. Agregando ahora la información de sitio a "datos_globales_proyecto"

datos_globales_sitio <- datos_globales_proyecto %>%
  mutate(
    # Auxiliar para generar la llave primaria de la tabla en cuestión, debe ser
    # una llave natural en una sola columna de tipo factor (formada posiblemente
    # concatenando varias columnas), que posteriormente se convertirá a numérico.
    Site_sample.id_aux = Nombre_del_Arrecife %>%
      # quitando mayúsculas
      tolower() %>%
      # quitando espacios
      stri_trim_both,
    
    Site_sample.name_aux = Nombre_del_Arrecife,
    Site_sample.country_aux = PAIS,
    
    # Healthy_reefs_region pueden ser dos columnas distintas, pero por flexibilidad
    # del código, se quedará como una única.
    Site_sample.healthy_reefs_region_aux = Region_del_arrecife,
    Site_sample.location_aux = Localidad,
    Site_sample.reef_name_aux = "",
    
    # Revisar diferencias entre estas 3, parece que son lo mismo
    Site_sample.reef_type_aux = Tipo_de_Arrecife,
    Site_sample.reef_zone_aux = Zona_arrecifal,
    Site_sample.subzone_habitat_aux = Subzona_de_habitat,
    Site_sample.inside_protected_area_aux = ifelse(ANP == "NA", FALSE, TRUE),
    Site_sample.protected_area_aux = ifelse(ANP == "NA", NA, ANP),
    Site_sample.inside_non_fishing_area_aux = NA,
    Site_sample.latitude_aux = `Latitud `,
    Site_sample.longitude_aux = Longitud,
    Site_sample.datum_aux = "WGS84",
    Site_sample.date_aux = ifelse(Fecha == "NA", "", Fecha %>%
        as.numeric() %>%
        as.Date(origin = "1899-12-30") %>%
        as.character()
    ),
    Site_sample.time_aux = ifelse(Hora == "NA" | is.na(Hora), NA,
      ifelse(stri_detect_coll(Hora, ":"), Hora,
        (parse_date_time("00:00:00", orders = "HMS") +
          as.numeric(Hora) * 24 * 60 * 60) %>%
          as.character() %>%
          stri_extract_first_regex(., "\\d\\d:\\d\\d")
      )
    ),
    Site_sample.methodology_aux = Protocolo,
    Site_sample.methodology_details_aux = Metodo,
    Site_sample.data_aggregation_level_aux = Nivel_de_agregacion_de_datos,
    Site_sample.temperature_aux = Temperatura,
    Site_sample.depth_aux = Profundidad_media_m
  ) %>%
  # generando Site_sample.id, la llave primaria
  mutate(
    Site_sample.id = Site_sample.id_aux %>%
      as.factor() %>%
      as.numeric()
  ) %>%
  group_by(
    # Agrupando por la llave primaria
    Site_sample.id
  ) %>%
  mutate(
    Site_sample.name = first(Site_sample.name_aux),
    Site_sample.country = first(Site_sample.country_aux),
    Site_sample.healthy_reefs_region = first(Site_sample.healthy_reefs_region_aux),
    Site_sample.location = first(Site_sample.location_aux),
    Site_sample.reef_name = first(Site_sample.reef_name_aux),
    Site_sample.reef_type = first(Site_sample.reef_type_aux),
    Site_sample.reef_zone = first(Site_sample.reef_zone_aux),
    Site_sample.subzone_habitat = first(Site_sample.subzone_habitat_aux),
    Site_sample.inside_protected_area = first(Site_sample.inside_protected_area_aux),
    Site_sample.protected_area = first(Site_sample.protected_area_aux),
    Site_sample.inside_non_fishing_area = first(Site_sample.inside_non_fishing_area_aux),
    Site_sample.latitude = first(Site_sample.latitude_aux),
    Site_sample.longitude = first(Site_sample.longitude_aux),
    Site_sample.datum = first(Site_sample.datum_aux),
    Site_sample.date = first(Site_sample.date_aux),
    Site_sample.time = first(Site_sample.time_aux),
    Site_sample.methodology = first(Site_sample.methodology_aux),
    Site_sample.methodology_details = paste0(archivo_origen, ": ",
      Site_sample.methodology_details_aux) %>%
        unique() %>%
        paste(collapse = ", "),
    Site_sample.data_aggregation_level = first(Site_sample.data_aggregation_level_aux),
    Site_sample.temperature = first(Site_sample.temperature_aux),
    Site_sample.depth = first(Site_sample.depth_aux)
  )

site_sample <- datos_globales_sitio %>%
  select(
    id = Site_sample.id,
    project_id = Project.id,
    name = Site_sample.name,
    country = Site_sample.country,
    healthy_reefs_region = Site_sample.healthy_reefs_region,
    location = Site_sample.location,
    reef_name = Site_sample.reef_name,
    reef_type = Site_sample.reef_type,
    reef_zone = Site_sample.reef_zone,
    subzone_habitat = Site_sample.subzone_habitat,
    inside_protected_area = Site_sample.inside_protected_area,
    protected_area = Site_sample.protected_area,
    inside_non_fishing_area = Site_sample.inside_non_fishing_area,
    latitude = Site_sample.latitude,
    longitude = Site_sample.longitude,
    datum = Site_sample.datum,
    date = Site_sample.date,
    time = Site_sample.time,
    methodology = Site_sample.methodology,
    methodology_details = Site_sample.methodology_details,
    data_aggregation_level = Site_sample.data_aggregation_level,
    temperature = Site_sample.temperature,
    depth = Site_sample.depth
  ) %>%
  unique()

# # Revisando que no haya NA's en campos específicos para un aspecto,
# datos_globales %>%
#   filter(archivo_origen == "bentos_desagregado") %>%
#   select(Codigo) %>%
#   table(useNA = "always")
# 
# datos_globales %>%
#   filter(archivo_origen == "corales_desagregados") %>%
#   select(Blaqueamieto) %>%
#   table(useNA = "always")
# 
# # Revisando que las "Especie" y "Code" en archivo_origen == "peces_desagregados"
# # tengan la misma cantidad de datos
# 
# datos_globales %>%
#   filter(archivo_origen == "peces_desagregados") %>%
#   select(
#     Especie,
#     Code
#   ) %>%
#   mutate(
#     Especie_bool = ifelse(is.na(Especie), 0, 1),
#     Code_bool = ifelse(is.na(Code), 0, 1)
#   ) %>%
#   summarise(
#     num_Especie = sum(Especie_bool),
#     num_Code = sum(Code_bool)
#   )

## 4. Agregando la información de transecto a "datos_globales_sitio".

datos_globales_transecto <- datos_globales_sitio %>%
  mutate(
    # Auxiliar para generar la llave primaria de la tabla en cuestión, debe ser
    # una llave natural en una sola columna de tipo factor (formada posiblemente
    # concatenando varias columnas), que posteriormente se convertirá a numérico.
    Transect_sample.id_aux = paste0(Site_sample.id, "_", Transecto %>%
      # quitando mayúsculas si existen
      tolower() %>%
      # quitando espacios
      stri_trim_both %>%
      # quitando primer 0
      stri_replace_first_regex("^0(.*)", "$1")),
    Transect_sample.id = Transect_sample.id_aux %>%
      as.factor() %>%
      as.numeric(),
    Transect_sample.name_aux = Transecto
  ) %>%
  group_by(Transect_sample.id) %>%
  mutate(
    Transect_sample.name = first(Transect_sample.name_aux)
  )

Transect_sample <- datos_globales_transecto %>%
  select(
    id = Transect_sample.id,
    site_sample_id = Site_sample.id,
    name = Transect_sample.name
  ) %>%
  unique()




#write_csv(site_sample, "productos/ejemplo_tabla_sitios.csv")
#write_csv(project, "productos/ejemplo_tabla_projectos.csv")

