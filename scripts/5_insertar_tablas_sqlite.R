library("plyr")
library("dplyr")
library("RSQLite")

# Leyendo objetos que contienen las tablas:
load("../productos/tablas_proyectos_sitios_transectos.RData")
load("../productos/tablas_muestreo_aspectos_transecto.RData")

base_output <- dbConnect(RSQLite::SQLite(), "../productos/barco_db_v2.sqlite")

# # Insertando las tablas en la base
dbWriteTable(base_output, "project", project)
dbWriteTable(base_output, "site_sample", site_sample)
dbWriteTable(base_output, "transect_sample", transect_sample)

dbWriteTable(base_output, "benthos_transect_sample_info", benthos_transect_sample_info)
dbWriteTable(base_output, "benthos_transect_sample_point", benthos_transect_sample_point)

dbWriteTable(base_output, "coral_transect_sample_info", coral_transect_sample_info)
dbWriteTable(base_output, "coral_transect_sample_observation", coral_transect_sample_observation)

dbWriteTable(base_output, "fish_transect_sample_info", fish_transect_sample_info)
dbWriteTable(base_output, "fish_transect_sample_count", fish_transect_sample_count)

dbWriteTable(base_output, "invertebrate_transect_sample_info", invertebrate_transect_sample_info)
dbWriteTable(base_output, "invertebrate_transect_sample_observation", invertebrate_transect_sample_observation)

dbWriteTable(base_output, "complexity_transect_sample_info", complexity_transect_sample_info)

dbWriteTable(base_output, "recruit_subquadrat_sample_from_transect_info", recruit_subquadrat_sample_from_transect_info)
dbWriteTable(base_output, "recruit_subquadrat_sample_from_transect_count", recruit_subquadrat_sample_from_transect_count)
dbDisconnect(base_output)
