# En este script se revisa la consistencia de las tablas finales de datos que
# fueron integradas en la base SQLite.

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")
library("readr")

################################################################################
# Obteniendo tablas generales necesarias para hacer las revisiones
################################################################################

conexion_barco_db <- src_sqlite(paste0(rutas_salida[9], "/barco_db.sqlite"))

Muestreo <- conexion_barco_db %>%
  tbl("Muestreo") %>%
  collect()

Muestra_sitio <- conexion_barco_db %>%
  tbl("Muestra_sitio") %>%
  collect() %>%
  mutate(
    # Para usar la función que permite encontrar duplicados
    serie = id
  ) %>%
  inner_join(Muestreo %>%
      select(
        muestreo_id = id,
        nombre_muestreo = nombre
      ), by = "muestreo_id") %>%
  select(-muestreo_id)
glimpse(Muestra_sitio)

################################################################################
# Revisando que no haya muestras de sitio duplicadas en la base de datos
################################################################################

# Recordar que una llave natural de un muestreo de sitio es básicamente el nombre
# del sitio y la fecha de muestreo. La hora de muestreo al parecer no es tan
# importante, ya que es difícil tener dos muestreos distintos del mismo sitio
# el mismo día

muestras_sitio_duplicadas <- (encuentra_duplicados(
  list("Muestra_sitio" = Muestra_sitio),
  list("Muestra_sitio" = c("nombre", "fecha")),
  tipo_resultado = "completo"
))$Muestra_sitio

write_csv(muestras_sitio_duplicadas,
  paste0(rutas_salida[10], "/muestras_sitio_duplicadas.csv"))
