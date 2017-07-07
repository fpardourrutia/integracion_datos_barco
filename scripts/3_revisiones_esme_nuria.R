# En este script se pondrán los diversos queries que me pidan Esme y Nuria

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("readr")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

# Falta definir todas las tablas de la bd para leerlas.
datos_globales <- readRDS("../productos/datos_globales.RData")

# Tabla de números de transecto de cada tipo (bentos / peces) por "archivo_origen"
# y muestreo de sitio:
cantidad_transectos_por_tipo_archivo_origen_muestreo_sitio <- datos_globales %>%
  select(
    nombre_sitio,
    fecha_hora_muestreo_sitio,
    transecto,
    archivo_origen
  ) %>%
  distinct() %>%
  mutate(
    tipo_transecto = ifelse(stri_detect_coll(transecto, "bentos"), "bentos", "peces"),
    archivo_origen_tipo_transecto = paste0(archivo_origen, "_", tipo_transecto)
  ) %>%
  group_by(nombre_sitio, fecha_hora_muestreo_sitio, archivo_origen_tipo_transecto) %>%
  tally() %>%
  spread(key = archivo_origen_tipo_transecto, value = n, fill = 0)

# Esme ya me dijo, para los proyectos CONACyT y GreenPeace:
# - Bentos son 6 transectos. En cada transecto de bentos siempre hay observaciones.
# - Corales son menos, en promedio 2. En cada transecto de corales siempre hay
# al menos una observación.
# - Para cada muestreo de sitio, los invertebrados se toman ya sea en transectos
# de bentos, o de bentos y peces. Puede haber muestreos sin observaciones, pero
# esto está contemplado en los exceles (proyectos CONACyT y GreenPeace).
# - Cada muestreo en transecto de peces tuvo al menos una observación.
# - Cada muestreo de complejidad obviamente conlleva observaciones (pues no se
# está buscando algo)
#- Los muestreos de reclutas están a nivel de cuadrante, y no siempre hay
# observaciones, pero esto está tomado en cuenta a nivel de cuadrante en los
# exceles.

# write_csv(cantidad_transectos_por_tipo_archivo_origen_muestreo_sitio,
#   "../productos/cantidad_transectos_por_tipo_archivo_origen_muestreo_sitio.csv")

# Por ello, para este proyecto, siempre se tiene que dar que el número de
# transectos de bentos sea mayor que el de corales, reclutas, complejidad e
# invertebrados en transecto de bentos.
# De igual manera, el número de transectos de peces debe ser mayor que el de
# invertebrados sacados de peces.
# Revisando muestreos de sitio con cantidad anómala de transectos

cantidad_transectos_por_tipo_archivo_origen_muestreo_sitio %>%
  filter(
    BENTOS_DESAGREGADOS_V2_bentos < CORALES_DESAGREGADOS_V2_bentos |
      BENTOS_DESAGREGADOS_V2_bentos < RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2_bentos |
      BENTOS_DESAGREGADOS_V2_bentos < INVERTEBRADOS_DESAGREGADOS_V2_bentos |
      BENTOS_DESAGREGADOS_V2_bentos < RUGOSIDAD_DESAGREGADA_V2_bentos |
      PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2_peces < INVERTEBRADOS_DESAGREGADOS_V2_peces) %>%
  mutate(
    tipo_error = case_when(
      BENTOS_DESAGREGADOS_V2_bentos < CORALES_DESAGREGADOS_V2_bentos ~
        "faltan algunos transectos de bentos",
      BENTOS_DESAGREGADOS_V2_bentos < RECLUTAS_Y_SUSTRATO_DESAGREGADO_V2_bentos ~
        "faltan algunos transectos de bentos",
      BENTOS_DESAGREGADOS_V2_bentos < INVERTEBRADOS_DESAGREGADOS_V2_bentos ~
        "faltan algunos transectos de bentos",
      BENTOS_DESAGREGADOS_V2_bentos < RUGOSIDAD_DESAGREGADA_V2_bentos ~
        "faltan algunos transectos de bentos",
      PECES_DESAGREGADOS_CONACYT_GREENPEACE_V2_peces < INVERTEBRADOS_DESAGREGADOS_V2_peces ~
        "faltan algunos transectos de peces"
    )
  ) %>%
  select(
    nombre_sitio,
    fecha_hora_muestreo_sitio,
    tipo_error
  ) %>%
  View()
########

numero_transectos_tipo_muestreo_sitio <- project %>%
  select(
    project_id = id,
    project_name = name
  ) %>%
  inner_join(site_sample %>%
      select(
        site_sample_id = id,
        project_id,
        site_name = name,
        datetime), by = "project_id") %>%
  inner_join(transect_sample %>%
      mutate(
        tipo_transecto = ifelse(
          stri_detect_coll(name, "bentos"),
          "bentos",
          "peces")
      ) %>%
      group_by(site_sample_id, tipo_transecto) %>%
      tally() %>%
      ungroup(), by = "site_sample_id")

# write_csv(numero_transectos_tipo_muestreo_sitio, "../productos/numero_transectos_tipo_muestreo_sitio.csv")

# Especies de peces con dos registros asociados en el mismo transecto (cosa imposible
# por la manera en que están estructurados los datos)
especies_peces_varios_registros_mismo_transecto_muestra <- conteo_peces_transecto_llave_primaria %>%
  group_by(id_conteo_especie_talla) %>%
  mutate(
    cantidad_registros_identicos = n()
  ) %>%
  ungroup() %>%
  mutate(
    especie = estandariza_strings(especie)
  ) %>%
  filter(cantidad_registros_identicos > 1) %>%
  distinct(
    nombre_sitio,
    fecha_hora_muestreo_sitio,
    transecto,
    codigo,
    especie
  ) %>%
  select(
    nombre_sitio,
    fecha_hora_muestreo_sitio,
    transecto,
    codigo,
    especie
  ) %>%
  arrange(
    nombre_sitio,
    fecha_hora_muestreo_sitio,
    transecto,
    codigo,
    especie
  )

(conteo_peces_transecto_llave_primaria$codigo %>%
    unique() %>%
    length()) ==
    (conteo_peces_transecto_llave_primaria$especie %>%
        estandariza_strings() %>%
        unique() %>%
        length())
# Perfecto: TRUE!

# write_csv(especies_peces_varios_registros_mismo_transecto_muestra,
#   "../productos/especies_peces_varios_registros_mismo_transecto_muestra.csv")
