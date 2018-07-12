
lista_datos_crudos_conacyt_greenpeace_2016 <- readRDS("../productos/v3/lista_datos_crudos_conacyt_greenpeace_2016.RData")
lista_catalogos <- readRDS("../productos/v3/lista_catalogos.RData")

###################################
# Revisión de catálogos
###################################

# Revisar duplicados varios catálogos:

# 

duplicados <- lista_catalogos$ %>%
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


# Revisar si hay peces con nombre ambiguo en la base de CONACyT / GreenPeace
peces_duplicados <- read_csv("../productos/v3/lista_nombres_peces_duplicados_catalogo.csv")

datos_globales %>%
  filter(archivo_origen == "conacyt_greenpeace_2016_peces_agregados_especie_talla_v3") %>%
  select(especie) %>%
  semi_join(peces_duplicados, by = c("especie" = "Especie"))


