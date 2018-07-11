library("plyr")
library("dplyr")
library("RSQLite")

# Leyendo objetos que contienen las tablas:
load("../productos/v3/tablas_muestreos_sitios_transectos.RData")
load("../productos/v3/prueba_tablas_muestra_transecto_bentos.RData")
lista_catalogos <- readRDS("../productos/v3/lista_catalogos.RData")

base_output <- dbConnect(RSQLite::SQLite(), "../productos/v3/barco_db_v3.sqlite")

# Insertando las tablas en la base
dbWriteTable(base_output, "muestreo", muestreo)
dbWriteTable(base_output, "muestra_sitio", muestra_sitio)
dbWriteTable(base_output, "muestra_transecto", muestra_transecto)

dbWriteTable(base_output, "muestra_transecto_bentos_info", muestra_transecto_bentos_info)
dbWriteTable(base_output, "muestra_transecto_bentos_punto", muestra_transecto_bentos_punto)

dbWriteTable(base_output, "coral_transect_sample_info", coral_transect_sample_info)
dbWriteTable(base_output, "coral_transect_sample_observation", coral_transect_sample_observation)

dbWriteTable(base_output, "fish_transect_sample_info", fish_transect_sample_info)
dbWriteTable(base_output, "fish_transect_sample_count", fish_transect_sample_count)

dbWriteTable(base_output, "invertebrate_transect_sample_info", invertebrate_transect_sample_info)
dbWriteTable(base_output, "invertebrate_transect_sample_observation", invertebrate_transect_sample_observation)

dbWriteTable(base_output, "complexity_transect_sample_info", complexity_transect_sample_info)

dbWriteTable(base_output, "recruit_subquadrat_sample_from_transect_info", recruit_subquadrat_sample_from_transect_info)
dbWriteTable(base_output, "recruit_subquadrat_sample_from_transect_count", recruit_subquadrat_sample_from_transect_count)

# Insertando catálogos:
l_ply(1:length(lista_catalogos), function(i){
  dbWriteTable(base_output, names(lista_catalogos)[i], lista_catalogos[[i]])
})

# Creando tabla muestra para el uso de los catálogos
library("tidyr")

vista_bentos <- muestra_sitio %>%
  rename(muestra_sitio_id = id) %>%
  inner_join(muestra_transecto %>%
      rename(muestra_transecto_id = id), by = "muestra_sitio_id") %>%
  inner_join(muestra_transecto_bentos_info %>%
      rename(muestra_transecto_bentos_info_id = id), by = "muestra_transecto_id") %>%
  inner_join(muestra_transecto_bentos_punto, by = "muestra_transecto_bentos_info_id") %>%
  inner_join(lista_catalogos$catalogos_muestra_transecto_bentos_observacion__codigo %>%
      distinct(Codigo, .keep_all = TRUE),
    by = c("codigo" = "Codigo")) %>%
  select(
    muestra_sitio_id,
    nombre_sitio = nombre.x,
    fecha_visita = fecha,
    tipo_cobertura = Nivel_3
  ) %>%
  # Calculando el número de puntos de cada tipo de cobertura por punto
  group_by(muestra_sitio_id, nombre_sitio, fecha_visita, tipo_cobertura) %>%
  tally() %>%
  ungroup() %>%
  # Ahora contando el número total de puntos por sitio
  group_by(muestra_sitio_id) %>%
  mutate(
    total_puntos = sum(n)
  ) %>%
  ungroup() %>%
  # Calculando porcentajes de cobertura por sitio
  mutate(
    porcentaje_cobertura = round((n / total_puntos) * 100, 2)
  ) %>%
  select(-n) %>%
  spread(key = tipo_cobertura, value = porcentaje_cobertura, fill = 0)

dbWriteTable(base_output, "vista_bentos_muestreo_sitio", vista_bentos)
  
dbDisconnect(base_output)
