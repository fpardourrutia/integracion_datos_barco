library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("readxl")
library("lubridate")
library("measurements")
library("RSQLite")

## Poniendo las opciones para las wanings
options(nwarnings=50000)

##############################################################
# Leyendo archivo por archivo, para revisar que todo esté bien
##############################################################

### BENTOS

bentos_conacyt <- read_excel("../BASES_INTEGRACION/conacyt/BENTOS_DESAGREGADOS.xlsx", sheet = 2, col_types = rep("text", 53)) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  # eliminando renglones vacíos
  filter(!is.na(Institucion))

# Revisando columna por columna
bentos_conacyt_revision_datos_columna <- llply(colnames(bentos_conacyt), function(x){
  bentos_conacyt[x] %>%
    unique()
})
names(bentos_conacyt_revision_datos_columna) <- colnames(bentos_conacyt)

# A arreglar:

# $Titulo:
# Evaluación de la efectividadde las Áreas Marinas Protegidas en los arrecifales del Caribe Mexicano

# $Metodo_de_Seleccion_de_Sitios = Estratégico

# Se agregarán fechas de inicio y término a cada CURP del proyecto, utilizando
# una tabla adicional que me pasaará Esme. Mejor se renombrará este proyecto para
# englobarlo en su totalidad.

# Esme me va a pasar un catálogo de ANP's para la columna $ANP FALTA

# Nombres de los sitios sin remuestreo = Sitio_autor.
# Nombres de los sitios con remuestreo
# Esto lo arreglarán Esme y Nuria posteriormente.

# Esme me va a hacer un catálogo de $Observador FALTA

# Las unidades de profundidad convertirlas a metros.

# Para transformar códigos de Excel a fechas:
#as.Date(42713, origin = "1899-12-30")

# Con las horas, multiplicar el decimal por 24 y usar lubridate para minutos y
# segundos.

# Longitud, todos con signo -.

# Nombre de proyecto = nombre de proyecto + año.
# Esme va a arreglar el Nombre del proyecto 
# para que sea único y corresponda con Project.name,

bentos_conacyt_rev <- bentos_conacyt %>%
  mutate(
    Titulo = "Evaluación de la efectividad de las Áreas Marinas Protegidas en los arrecifales del Caribe Mexicano",
    Cita = paste0("Álvarez-Filip, L. 2016. Evaluación de la efectividad",
      " de las Áreas Marinas Protegidas en los arrecifales del Caribe Mexicano. Proyecto CONACYT 247104."),
    Metodo_de_Seleccion_de_Sitios = "Estratégico",
    # Para suprimir warnings al convertir en numérico
    Profundidad_inicial = ifelse(Profundidad_inicial == "NA", NA, Profundidad_inicial) %>%
      as.numeric(),
    Profundidad_final = ifelse(Profundidad_final == "NA", NA, Profundidad_final) %>%
      as.numeric(),
    
    Profundidad_inicial = ifelse(Unidades_de_profundidad %>% tolower() == "ft",
      conv_unit(Profundidad_inicial, from = "ft", to = "m"),
      Profundidad_final),
    Profundidad_final = ifelse(Unidades_de_profundidad %>% tolower() == "ft",
      conv_unit(Profundidad_final, from = "ft", to = "m"),
      Profundidad_final),
    
    # Cambiando las unidades de profundidad a m, por consistencia
    Unidades_de_profundidad = "m",
    Longitud = -1 * Longitud %>% as.numeric() %>%
      abs(),
    Fecha = as.Date(Fecha %>%
        as.numeric(), origin = "1899-12-30"),
    Hora =  (parse_date_time("00:00:00", orders = "HMS") +
        as.numeric(Hora) * 24 * 60 * 60) %>%
      as.character() %>%
      stri_extract_first_regex(., "\\d\\d:\\d\\d:\\d\\d")
  )

### CORALES

corales_conacyt <- read_excel("../BASES_INTEGRACION/conacyt/CORALES_DESAGREGADOS.xlsx",
  sheet = 2, col_types = rep("text", 69)) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  # eliminando renglones vacíos
  filter(!is.na(Institucion))

# Revisando columna por columna
corales_conacyt_revision_datos_columna <- llply(colnames(corales_conacyt), function(x){
  corales_conacyt[x] %>%
    unique()
})
names(corales_conacyt_revision_datos_columna) <- colnames(corales_conacyt)

llply(corales_conacyt_revision_datos_columna, View)




bentos_project <- bentos_conacyt_rev %>%
  mutate(
    # Arreglando fechas para el proyecto de GreenPeace
    Fecha_de_inicio = ifelse(Fecha_de_inicio == "42713",
      Fecha_de_inicio %>%
        as.numeric() %>%
        as.Date(origin = "1899-12-30") %>%
        as.character(),
      Fecha_de_inicio),
    Fecha_de_termino = ifelse(Fecha_de_termino == "42722",
      Fecha_de_termino %>%
        as.numeric() %>%
        as.Date(origin = "1899-12-30") %>%
        as.character(),
      Fecha_de_termino)
  ) %>%
  # Nombre del proyecto es una llave natural del mismo
  group_by(Nombre_del_Proyecto) %>%
  summarise(
    description = first(Proposito),
    purpose = first(Tema),
    # Falta la tabla de Location
    #location = 
    start_date = first(Fecha_de_inicio),
    end_date = first(Fecha_de_termino),
    organization = first(Institucion),
    suborganization = first(Suborganizacion),
    person_in_charge = first(Autor_Administrador_Del_Proyecto),
    contact = first(Contacto),
    site_selection_method = first(Metodo_de_Seleccion_de_Sitios),
    comment = first(Notas), 
    reference = first(Cita)
  ) %>%
  ungroup() %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  select(
    id,
    name = Nombre_del_Proyecto,
    everything()
  )

bentos_site_sample <- bentos_conacyt_rev %>%
  group_by(
    # Llave natural del proyecto para asociarle sus sitios
    project.name = Nombre_del_Proyecto,
    
    # Llave natural de cada muestreo de sitio.
    Sitio_autor,
    Fecha
  ) %>%
  summarise(
    # Para identificar cuáles son remuestreos y cuáles no.
    Nombre_remuestreo = first(Nombre_remuestreo),
    country = first(PAIS),
    healthy_reefs_region = first(Region_del_arrecife_HR),
    location = first(Localidad),
    reef_name = first(Nombre_del_Arrecife),
    reef_type = first(Tipo_de_Arrecife),
    reef_zone = first(Zona_arrecifal),
    subzone_habitat = first(Subzona_de_habitat),
    anp = first(ANP),
    latitude = first(`Latitud `),
    longitude = first(Longitud) ,
    date = first(Fecha),
    time = first(Hora),
    methodology = first(Protocolo),
    #metodology_details = first(Metodo),
    data_aggregation_level = first(Nivel_de_agregacion_de_datos),
    temperature = first(Temperatura_celsius),
    # Profundidad del sitio la agregan Esme y Nuria
    comments = first(Notas)
  ) %>%
  ungroup() %>%
  mutate(
    id = 1:nrow(.),
    datum = "WGS84",
    source = "BARCOLab 2016",
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    temperature = as.numeric(temperature),
    # Lo va a simplificar Esme
    name = ifelse(!is.na(Nombre_remuestreo),
      Nombre_remuestreo, reef_name),
    inside_protected_area = ifelse(anp == "NA", FALSE, TRUE),
    protected_area = ifelse(anp == "NA", NA, anp)
  ) %>%
  inner_join(bentos_project %>%
      select(
        project_id = id,
        name
      ), by = c("project.name" = "name")) %>%
  select(
    Sitio_autor,
    id,
    project_id,
    name,
    country,
    healthy_reefs_region,
    location,
    reef_name,
    reef_type,
    reef_zone,
    subzone_habitat,
    inside_protected_area,
    protected_area,
    latitude,
    longitude,
    datum,
    date,
    time,
    methodology,
    #methodology_details,
    data_aggregation_level,
    temperature,
    #depth,
    comments
  )

bentos_transect_sample <- bentos_conacyt_rev %>%
  group_by(
    # Llave natural del proyecto para asociarle sus sitios
    project.name = Nombre_del_Proyecto,
    
    # Llave natural de cada muestreo de sitio.
    Sitio_autor,
    Fecha,
    
    # Llave natural (compuesta con las anteriores) de cada transecto
    Transecto
  ) %>%
  tally() %>%
  ungroup() %>%
  inner_join(bentos_site_sample %>%
      select(
        Sitio_autor,
        date,
        site_sample_id = id
      ),
    by = c("Sitio_autor" = "Sitio_autor", "Fecha" = "date")) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  select(
    # Auxiliares
    Sitio_autor,
    Sitio_fecha = Fecha,
    id,
    site_sample_id,
    name = Transecto
  )

bentos_benthos_transect_sample_info <- bentos_conacyt_rev %>%
  group_by(
    # Llave natural del proyecto para asociarle sus sitios
    project.name = Nombre_del_Proyecto,
    
    # Llave natural de cada muestreo de sitio.
    Sitio_autor,
    Fecha,
    
    # Llave natural (compuesta con las anteriores) de cada transecto
    Transecto
    
    # Notar que la llave natural de un transecto es automáticamente la de los
    # datos de bentos, pues "bentos_conacyt_rev" sólo tiene datos de bentos.
  ) %>%
  summarise(
    sampling_method = first(Metodo),
    surveyor = first(Observador),
    start_depth_m = first(Profundidad_inicial),
    end_depth_m = first(Profundidad_final),
    length_m = first(Longitud_del_transecto_metro),
    Puntos_o_cm_Reales_Transecto = first(Puntos_o_cm_Reales_Transecto),
    comments = first(Notas)
    #sampling_completed la va a arreglar Esme.
  ) %>%
  ungroup() %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  inner_join(
    bentos_transect_sample %>%
      select(
        transect_sample_id = id,
        everything()
      ),
    by = c("Sitio_autor" = "Sitio_autor",
      "Fecha" = "Sitio_fecha",
      "Transecto" = "name")
  ) %>%
  mutate(
    length_m = as.numeric(length_m),
    Puntos_o_cm_Reales_Transecto = as.numeric(Puntos_o_cm_Reales_Transecto),
    distance_between_points_if_pit_cm = (length_m/Puntos_o_cm_Reales_Transecto) * 100
  ) %>%
  select(
    # Auxiliares
    Sitio_autor,
    Fecha,
    Transecto,
    id,
    transect_sample_id,
    sampling_method,
    surveyor,
    start_depth_m,
    end_depth_m,
    length_m,
    distance_between_points_if_pit_cm,
    # sampling_completed,
    comments
  )

bentos_benthos_transect_sample_point <- bentos_conacyt_rev %>%
  # Como el desglose de esta tabla es idéntico al de bentos_conacyt_rev, no
  # se agrupa. Sin embargo, para sacar el número de punto, voy a usar un ddply por
  # muestra de transecto
  ddply(~ Sitio_autor + Fecha + Transecto, function(datos){
    datos %>%
      mutate(
        numero_punto = 1:nrow(.)
      )
  }) %>%
  inner_join(
    bentos_benthos_transect_sample_info %>%
      select(
        Sitio_autor,
        Fecha,
        Transecto,
        benthos_transect_sample_info_id = id
      ),
    by = c("Sitio_autor" = "Sitio_autor",
      "Fecha" = "Fecha",
      "Transecto" = "Transecto")
  ) %>%
  transmute(
    id = 1:nrow(.),
    benthos_transect_sample_info_id,
    point_no = numero_punto,
    species_code = Codigo,
    height_if_algae = as.numeric(Altura_Algas_)
  )

# base_output <- dbConnect(RSQLite::SQLite(), "~/Desktop/base_prueba_barco.sqlite")
# 
# # Insertando las tabla en la base
# dbWriteTable(base_output, "Project", bentos_project)
# dbWriteTable(base_output, "Site_sample", bentos_site_sample)
# 
# dbDisconnect(base_output)


# Si tiene las dos profundidades va por transecto, si tiene sólo una, por sitio.
inside_protected_area = ifelse(ANP == "NA", FALSE, TRUE)
  
### CORALES

corales_conacyt <- read_excel("../BASES_INTEGRACION/conacyt/CORALES_DESAGREGADOS.xlsx",
  sheet = 2, col_types = rep("text", 69)) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  # eliminando renglones vacíos
  filter(!is.na(Institucion))

# Revisando que se pueda hacer join con cada proyecto:
corales_conacyt %>%
  anti_join(
    bentos_project, by = c("Nombre_del_Proyecto" = "name")
  ) %>%
  group_by(Nombre_del_Proyecto) %>%
  tally()

# Extraer los datos de proyecto de todas las hojas, de sitios de todas las hojas
# y así por secciones, para crear cada tabla de projects, sites y transects.