# En este script se tratarán de encontrar nombres únicos para cada sitio,
# independientemente del remuestreo, tomando como base 3 columnas:
# nombre_arrecife, nombre_remuestreo, nombre_sitio

# Para ello:
# 0. Se seleccionarán todas las combinaciones que aparecen en los datos de
# valores entre las 3 columnas anteriores.
# 1. Se estandarizarán los strings de cada columna, con el fin de eliminar el
# mayor ruido posible.
# 2. Se definirá una relación de que dos nombres (strings) están relacionados
# si y sólo si están en el mismo registro.
# 3. Se obtendrá la cerradura de equivalencia de la relación anterior.
# 4. Las clases de equivalencia que define la relación anterior corresponderán
# (empíricamente) a sitios distintos.
# Cabe destacar que este análisis es totalmente preliminar, por lo que se necesitan
# curar los nombres de los sitios adecuadamente

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("relations")
# Cargando funciones auxiliares:
source("funciones_auxiliares.R")

# Leyendo datos globales ya semi-limpios
datos_globales <- readRDS("../productos/datos_globales.RData")
glimpse(datos_globales)

# Revisando el caos:
datos_globales %>%
  select(
    nombre_arrecife,
    nombre_remuestreo,
    nombre_sitio) %>%
  distinct() %>%
  View()

# Generando la relación entre los valores de las columnas "nombre_arrecife",
# "nombre_remuestreo" y "nombre_sitio". Dos valores están relacionados si
# existe un registro que los contiene a ambos:

# 0. Formando el data frame con las combinaciones distintas de valores de las
# columnas seleccionadas
df_aux_relacion_nombres_sitios <- datos_globales %>%
  transmute(
    nombre_arrecife = estandariza_strings(nombre_arrecife),
    nombre_remuestreo = estandariza_strings(nombre_remuestreo),
    nombre_sitio = estandariza_strings(nombre_sitio)
  ) %>%
  distinct()

# 1. Definiendo la relación de equivalencia entre los nombres.
relacion_nombres_sitios <- ldply(1:nrow(df_aux_relacion_nombres_sitios), function(i){
  # Obteniendo el i-ésimo registro de "df_aux_relacion_nombres_sitios"
  renglon_i <- df_aux_relacion_nombres_sitios[i,] %>%
    # Eliminando columnas vacías
    elimina_columnas_vacias() %>%
    # Casteando a vector de strings
    as.character() %>%
    # Quedándonos con valores únicos
    unique()
  
  # Generando el producto cartesiano, que es básicamente una relación de equivalencia
  # entre nombres que aparecen en el mismo renglón
  producto_cartesiano <- expand.grid(renglon_i, renglon_i, stringsAsFactors = FALSE)
  return(producto_cartesiano)
}) %>%
  # Quedándonos con renglones distintos
  distinct() %>%
  as.relation()

# 3. Obteniendo la cerradura de equivalencia de la relación anterior
# Sabemos que "relación nombres sitios" es reflexiva y simétrica
# (puesto que está realizada mediante productos cartesianos).
# Para que sea de equivalencia y poder hacer una partición de los nombres, falta
# sacarle la cerradura transitiva:
relacion_equivalencia_nombres_sitios <- transitive_closure(relacion_nombres_sitios)
#relation_is_equivalence(relacion_equivalencia_nombres_sitios) # TRUE

# 4. Revisando las clases de equivalencia generadas por la relación anterior
clases_equivalencia_nombres_sitios <- relation_classes(relacion_equivalencia_nombres_sitios)
# Son 69 clases

# Formando un data frame con el número de clase de equivalencia y el nombre en
# la base:
df_clases_equivalencia_nombres_sitios <- relacion_equivalencia_nombres_sitios %>%
  relation_class_ids() %>%
  data_frame(
    id_clase = .,
    nombre = names(.)
  )

df_conjunto_completo_representantes_nombres_sitios <- df_clases_equivalencia_nombres_sitios %>%
  group_by(id_clase) %>%
  summarise(
    representante_clase = first(nombre)
  ) %>%
  ungroup()


# Haciendo joins con "datos globales", columnas:
# "nombre_arrecife", "nombre_remuestreo", "nombre_sitio"
# para obtener el nombre estandarizado del sitio (no de la muestra del sitio)
datos_globales_nombre_sitio_estimado <- datos_globales %>%
  mutate(
    # Estandarizando para poder hacer el join
    nombre_arrecife = estandariza_strings(nombre_arrecife),
    nombre_remuestreo = estandariza_strings(nombre_remuestreo),
    nombre_sitio = estandariza_strings(nombre_sitio)
  ) %>%
  left_join(df_clases_equivalencia_nombres_sitios, by = c("nombre_arrecife" = "nombre")) %>%
  left_join(df_clases_equivalencia_nombres_sitios, by = c("nombre_remuestreo" = "nombre")) %>%
  left_join(df_clases_equivalencia_nombres_sitios, by = c("nombre_sitio" = "nombre")) %>%
  mutate(
    id_clase_final = case_when(
      !is.na(id_clase) ~ id_clase,
      !is.na(id_clase.x) ~ id_clase.x,
      TRUE ~ id_clase.y
    )
  ) %>%
  inner_join(df_conjunto_completo_representantes_nombres_sitios,
    by = c("id_clase_final" = "id_clase"))
glimpse(datos_globales_nombre_sitio_estimado)
# Todo bien pues se conservan los 59,341 registros

# Revisando:
datos_globales_nombre_sitio_estimado %>%
  select(
    id_clase,
    id_clase.x,
    id_clase.y
  ) %>%
  distinct() %>%
  View()
# Perfecto! Todos los valores en el mismo renglón son los mismos, ésto va de acuerdo
# con nuestra definición de la relación inicial.

datos_globales_nombre_sitio_estimado %>%
  group_by(id_clase_final, representante_clase) %>%
  tally() %>%
  View()
View(df_clases_equivalencia_nombres_sitios)
# Es muy probable que haya errores, porque hay sitios con muy pocos registros,
# sin embargo, se ven más o menos parejos (los sitios con muchos registros no son
# tan preocupantes porque puede haber remuestreo.)
# Sin embargo, se ve razonable al comparar con "df_clases_equivalencia_nombres_sitios".
# chankanaab parece tener remuestreo!

# Comparando con los sitios de bentos, que parece que son los mejor nombrados:
datos_globales %>%
  filter(archivo_origen == "BENTOS_DESAGREGADOS_V2") %>%
  transmute(
    nombre_remuestreo = nombre_remuestreo,
    nombre_sitio = nombre_sitio,
    fecha = paste0(anio, "-", mes, "-", dia) %>%
      as_date()
  ) %>%
  distinct() %>%
  arrange(nombre_remuestreo) %>%
  View()
# Parecen haber 67 sitios y 1 remuestreo, chankanaab, que va con lo anterior.
# Además, los sitios con menos muestras son los que no aparecen en Bentos
# ("Fish Market" y "La Bocana"). Todo coincide! Obviamente, podrían estos corresponder
# a un nombre numérico que no está en la base.

# # Una idea adicional: hacer una tabla de combinaciones de "nombre_sitio_estimado"
# # con fecha y hora de muestreo, para ver si "Fish Market" y "La Bocana" corresponden
# # con alguno de los otros sitios:
# datos_globales_nombre_sitio_estimado %>%
#   select(
#     representante_clase,
#     anio,
#     mes,
#     dia,
#     hora,
#     minutos
#   ) %>%
#   distinct() %>%
#   arrange(
#     anio,
#     mes,
#     dia,
#     hora,
#     minutos
#   ) %>%
#   View()
# # Tal vez sean el mismo sitio. Revisando
# 
# datos_globales_nombre_sitio_estimado %>%
#   filter(representante_clase %in% c("fish_market", "la_bocana")) %>%
#   select(
#     representante_clase,
#     archivo_origen,
#     transecto
#   ) %>%
#   distinct() %>%
#   arrange(
#     representante_clase,
#     archivo_origen,
#     transecto
#   ) %>%
#   View()
# 
# datos_globales_nombre_sitio_estimado %>%
#   filter(transecto %in% c("1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B")) %>%
#   select(
#     archivo_origen
#   ) %>%
#   distinct()
# # Tal parece que "La Bocana" así fue muestreada, y probablemente "Fish Market"
# # también

datos_globales_final <- datos_globales_nombre_sitio_estimado %>%
  select(
    id,
    archivo_origen,
    serie:localidad,
    nombre_sitio_estimado = representante_clase,
    transecto:altura_algas_cm,
    ancho_transecto_m:tamanio_cadena_m
  )

# saveRDS(datos_globales_final, "../productos/datos_globales_final.RData")