library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("readr")
library("readxl")
library("lubridate")
library("measurements")
library("RSQLite")
library("ggplot2")

# ## Poniendo las opciones para las warnings
# options(nwarnings=50000)
# 
# nombres_archivos <- list.files("../BASES_INTEGRACION/conacyt/migracion_datos", full.names = TRUE)
# 
# # Leyendo cada archivo de Excel, que contiene información de cada aspecto:
# lista_exceles <- llply(nombres_archivos, function(x){
#   # Primero se leerá cada archivo, no importando warnings, con el fin de saber
#   # el número de columnas de cada uno y poder especificar que todas sean leídas
#   # como texto (y así evitar warnings la segunda vez que se lean)
#   aux <- read_excel(x, sheet = 2)
#   print(ncol(aux))
#   datos <- read_excel(x, sheet = 2, col_types = rep("text", ncol(aux))) %>%
#     # Se revisaron los datos, y todos los renglones no vacíos tienen el siguiente campo
#     filter(!is.na(Serie)) %>%
#     # Agregando el archivo origen a los datos correspondientes
#     mutate(
#       archivo_origen = (basename(x) %>%
#         stri_match_first_regex("(\\w+).xlsx"))[,2]
#       )
#   return(datos)
# })
# 
# names(lista_exceles) <- (basename(nombres_archivos) %>%
#   stri_match_first_regex("(\\w+).xlsx"))[,2]
# 
# # Escribiendo Warnings
# #sink("productos/warnings_lectura_archivos_excel.txt")
# #warnings()
# #sink()
# # 6005 + 3472 = 9477
# 
# # Escribiendo los data frames leídos en un objeto
# saveRDS(lista_exceles, "productos/lista_exceles.RData")

## 0. Leyendo los datos
lista_exceles <- readRDS("productos/lista_exceles.RData")

# Revisando que los exceles se hayan leído correctamente:
l_ply(names(lista_exceles), function(x){
  # Nombre de la tabla
  paste("\n", x, "\n") %>%
    cat()
  # Datos de la tabla
  glimpse(lista_exceles[[x]])
})

## 1. Creando una tabla con la información de todos los Exceles

# Para crear las tablas, primero se unirán los datos de todos los exceles
# (agregando columnas según se necesite), y luego se separarán esos datos en
# tablas
datos_globales <- Reduce(rbind.fill, lista_exceles) %>%
  mutate(
    id = 1: nrow(.)
  )

nombres_columnas <- colnames(datos_globales) %>%
  sort()

# Función auxiliar a la hora de programar, que busca en "nombres_columnas" los
# elementos que contienen cierto string.
# x: string a buscar en "nombre_columnas, sin contar mayúsculas ni minúsculas

encuentra_columnas <- function(x){
  indice <- stri_detect_fixed(nombres_columnas, x, case_insensitive = TRUE)
  return(nombres_columnas[indice])
}

## 2. Creando la información de proyecto en dicha tabla

datos_globales_proyecto <- datos_globales %>%
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

