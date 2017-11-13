library("plyr")
library("dplyr")
library("tidyr")
library("readxl")
library("readr")

# Revisar duplicados en catálogo de peces
datos <- read_excel("../../../../Dropbox/carpetas_compartidas/barco_lab/bases_integracion/datos_v3_preeliminar/catalogos/_Fish_transect_sample_count.species__especie.xlsx", sheet = 1)
glimpse(datos)

duplicados <- datos %>%
  select(Especie) %>%
  filter(duplicated(.)) %>%
  unique()

datos_duplicados <- datos %>%
  inner_join(duplicados, by = "Especie") %>%
  select(
    Serie,
    Especie
  ) %>%
  arrange(Especie)

write_csv(datos_duplicados, "~/datos_duplicados.csv")

# Revisar número de cuadrantes de reclutas por transecto
datos_globales %>%
  filter(archivo_origen == "conacyt_greenpeace_2016_reclutas_desagregados_v3") %>%
  ddply(.(identificador_muestreo_sitio, transecto), function(df){
    resultado <- df$cuadrante %>%
      unique() %>%
      sort() %>%
      data_frame(
        nombre_sitio = unique(df$nombre_sitio),
        subcuadrantes_tomados = .)
    return(resultado)
  }) %>% group_by(nombre_sitio, identificador_muestreo_sitio, transecto) %>%
  tally() %>%
  View()

# Revisar si hay peces con nombre ambiguo en la base de CONACyT / GreenPeace
peces_duplicados <- read_csv("../productos/v3/lista_nombres_peces_duplicados_catalogo.csv")

datos_globales %>%
  filter(archivo_origen == "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3") %>%
  select(especie) %>%
  semi_join(peces_duplicados, by = c("especie" = "Especie"))


