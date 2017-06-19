library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("readxl")
library("lubridate")
library("measurements")

## Poniendo las opciones para las wanings
options(nwarnings=50000)

## Leyendo todas las tablas de CONACyT

# Enlistando las tablas

archivos_conacyt <- list.files(path = "../BASES_INTEGRACION/conacyt/")

# archivos <- llply(archivos_conacyt, function(x){
#   ruta_archivo <- paste0("../BASES_INTEGRACION/conacyt/", x)
#   read_excel(ruta_archivo, sheet = 2)
# })
# 
# names(archivos) <- archivos_conacyt
# 
# archivos$BENTOS_DESAGREGADOS.xlsx %>% View()

# Leyendo archivo por archivo, para revisar que todo esté bien

### BENTOS

bentos_conacyt <- read_excel("../BASES_INTEGRACION/conacyt/BENTOS_DESAGREGADOS.xlsx", sheet = 2, col_types = rep("text", 53)) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  # eliminando renglones vacíos
  filter(!is.na(Institucion))

# Revisando columna por columna
revision_datos_columna <- llply(colnames(bentos_conacyt), function(x){
  bentos_conacyt[x] %>%
    unique()
})
names(revision_datos_columna) <- colnames(bentos_conacyt)

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

# Esme me va a hacer un catálogo de $Observador FALTA

# Las unidades de profundidad convertirlas a metros.

# Para transformar códigos de Excel a fechas:
#as.Date(42713, origin = "1899-12-30")

# Con las horas, multiplicar el decimal por 24 y usar lubridate para minutos y
# segundos.

# Longitud, todos con signo -.

# Nombre de proyecto = nombre de proyecto + año.

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
    Hora_character =  (parse_date_time("00:00:00", orders = "HMS") +
        as.numeric(Hora) * 24 * 60 * 60) %>%
      as.character() %>%
      stri_extract_first_regex(., "\\d\\d:\\d\\d:\\d\\d")
  )

bentos_project <- bentos_conacyt_rev %>%
  select(
    Autor_Administrador_Del_Proyecto,
    Contacto,
    Titulo, #Descripción
    Documento,
    Cita,
    Institucion,
    Suborganizacion,
    
    # Esme lo va a arreglar para que sea único:
    Nombre_del_Proyecto,
    
    Curp_Proyecto,
    Tema,
    Proposito,
    Fecha_de_inicio,
    Fecha_de_termino
  ) %>%
  unique() %>%
  # Arreglando fechas para el proyecto de GreenPeace
  mutate(
    Fecha_de_inicio = as.numeric(Fecha_de_inicio),
    Fecha_de_termino = as.numeric(Fecha_de_termino),
    Fecha_de_inicio = ifelse(Fecha_de_inicio == 42713,
      as.Date(Fecha_de_inicio, origin = "1899-12-30"),
      Fecha_de_inicio)
  ) %>%
  transmute(
    
  )


### CORALES

corales_conacyt <- read_excel("../BASES_INTEGRACION/conacyt/CORALES_DESAGREGADOS.xlsx", sheet = 2, col_types = rep("text", 70)) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  # eliminando renglones vacíos
  filter(!is.na(Institucion))
