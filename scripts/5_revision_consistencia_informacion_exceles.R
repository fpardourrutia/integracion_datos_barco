# En este script se realizan revisiones que tienen que ver más con el formato
# y la consistencia de los datos almacenados, que con las restricciones descritas
# en el esquema de datos v5. El procedimiento seguido es el siguiente:

# 1. Se agregan los data frames en "lista_tablas_columnas_homologadas" en un sólo
# data frame.
# 2. Se hacen revisiones de qué tablas contienen qué columnas.
# 3. Se hacen revisiones los contenido de cada columna contenida en los exceles.
# Cabe destacar que a la hora de realizar estas revisiones por primera vez,
# estas se separaron con respecto a las tablas que les corresponden en el
# esquema de datos final (posiblemente duplicando algunas). Esta revisión es
# uno de los puntos más importantes de este script.
# 4. Se crea una relación con los archivos de excel que contienen los posibles
# errores encontrados en la revisión anterior
# 5. Se buscan los strings "NA" y se crea una relación de los archivos y columas
# que los contienen, con el fin de cambiarlos por celdas vacías.
# 6. Se revisan las tablas hoja de la base de datos. Esto debido a que revisar
# estos en tablas con la información no normalizada (separada en tablas), permite
# revisar todas las tablas en el esquema. Por ello, la revisión se enfoca en
# puntos como:
# - Que no haya muestras declarados varias veces en los archivos de Excel.
# - Que las muestras sin observaciones estén correctamente declaradas.
# - Que los datos que corresponden a agregados por muestreo espacial estén
#   correctamente agregados, y si son porcentajes de cobertura, sumen 100%.
# 7. Notas adicionales acerca de las columnas revisadas.

# Cargando archivo de configuración y funciones auxiliares
library("readr")
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo lista de tablas y creando data frame con la información integrada
################################################################################

# Leyendo lista de tablas con columnas homologadas
lista_tablas_columnas_homologadas <- readRDS(
  paste0(rutas_salida[2], "/lista_tablas_columnas_homologadas.RData"))

# Agregando dichas tablas en un solo data frame
tabla_revision <- Reduce(rbind.fill, lista_tablas_columnas_homologadas) %>%
  # Arreglando determinadas columnas que se necesitan en el punto 6.
  mutate(
    categoria_tamanio = case_when(
      tolower(categoria_tamanio) == "no considerada" ~ "No considerada",
      tolower(categoria_tamanio) == "r" ~ "R",
      tolower(categoria_tamanio) == "sc" ~ "SC",
      TRUE ~ NA_character_
    ),
    cobertura = as.numeric(cobertura)
  )

################################################################################
# 2. Revisando las relaciones exceles/columnas que contienen
################################################################################

names(lista_tablas_columnas_homologadas)
crea_resumen_columnas_df(lista_tablas_columnas_homologadas) %>%
  glimpse()

obtiene_archivos_columna(tabla_revision, "conteo")

################################################################################
# 3. Revisando cada columna contenida en los Exceles, separadas por tabla
# correspondiente en el esquema de datos,
################################################################################

# Clave:
# *: llave natural necesaria para crear tablas
# También se comenta:
# - El nombre de la columna en el esquema de datos.
# - Información acerca de si la columna es numérica o pertenece a un catálogo.
# - Supuestos adicionales que se realizarán a lo hora de integrar los datos.

lista_revision <- revisa_valores(tabla_revision)
names(lista_revision)

### Muestreo ###
names(lista_revision[["nombre_proyecto"]]) # Nombre del muestreo. *
names(lista_revision[["proposito"]]) # Descripción del muestreo
names(lista_revision[["tema"]]) # Propósito del muestreo. Catálogo
names(lista_revision[["localidad_proyecto"]]) # Área de estudio
names(lista_revision[["institucion"]]) # Organización. Sería bueno tener nombres completos.
names(lista_revision[["suborganizacion"]]) # Sería bueno tener nombres completos.
names(lista_revision[["autor_administrador_proyecto"]]) # Encargado
names(lista_revision[["contacto"]])
names(lista_revision[["cita"]]) # Referencia
names(lista_revision[["titulo"]]) # Nombre del proyecto
names(lista_revision[["anio_inicio_proyecto"]]) # Numérico
names(lista_revision[["anio_termino_proyecto"]]) # Numérico

### Muestra_sitio ###
names(lista_revision[["identificador_muestreo_sitio"]]) # Sólo presente en datos del CONACyT/GreenPeace
names(lista_revision[["identificador_sitio"]]) # Sólo presente en datos históricos y del 2017
names(lista_revision[["nombre_sitio"]]) # *
names(lista_revision[["nombre_original"]])
# Si este campo está lleno, entonces el nombre original del sitio es éste, sino
# copiarle "nombre_sitio".
names(lista_revision[["anio"]]) # Numérico *
names(lista_revision[["mes"]]) # Numérico *
names(lista_revision[["dia"]]) # Numérico *
names(lista_revision[["hora"]]) # Natural. Esme debe revisar el formato 24h
names(lista_revision[["minutos"]]) # Entero
names(lista_revision[["pais"]])
names(lista_revision[["region_healthy_reefs"]]) # Catálogo
names(lista_revision[["localidad"]])
names(lista_revision[["tipo_arrecife"]]) # Catálogo
names(lista_revision[["subtipo_arrecife"]]) # Catálogo
names(lista_revision[["zona_arrecife"]]) # Catálogo
names(lista_revision[["anp"]]) # Catálogo.
names(lista_revision[["area_no_pesca"]]) # Booleano
names(lista_revision[["latitud"]]) # Numérico
names(lista_revision[["longitud"]]) # Numérico
names(lista_revision[["metodo_seleccion_sitios"]]) # Catálogo
names(lista_revision[["protocolo"]]) # Catálogo. Metodología
names(lista_revision[["temperatura_c"]]) # Numérico
names(lista_revision[["profundidad_media_m_sitio"]]) # Numérico
names(lista_revision[["notas"]])
names(lista_revision[["observaciones"]])
# Notas son observaciones, sólo que en algunos Exceles se usó un nombre y en otros otro.
# Se agregarán por facilidad a los comentarios de "Muestra_sitio".

### Muestra_transecto ###
names(lista_revision[["transecto"]]) # Nombre del transecto *
names(lista_revision[["transecto_fijo"]]) # Booleano
names(lista_revision[["profundidad_inicial_m"]]) # Numérico
names(lista_revision[["profundidad_final_m"]]) # Numérico
names(lista_revision[["longitud_cuadrante_m"]]) # Numérico
names(lista_revision[["ancho_cuadrante_m"]]) # Numérico

################################################################################

### Muestra_sitio_bentos_info ###
names(lista_revision[["metodo"]]) # Catálogo
names(lista_revision[["nivel_agregacion_datos"]]) # Catálogo
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["puntos_o_cm_reales_transecto"]]) # Numérico
names(lista_revision[["muestreo_completo"]]) # Booleano

### Muestra_sitio_bentos_porcentaje ###
names(lista_revision[["codigo"]]) # Catálogo.
names(lista_revision[["cobertura"]]) # No debe haber negativos ni letras. Suman 100.

### Muestra_transecto_bentos_info ###
names(lista_revision[["metodo"]]) # Catálogo
names(lista_revision[["nivel_agregacion_datos"]]) # Catálogo
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["longitud_transecto_m"]]) # Numérico
names(lista_revision[["puntos_o_cm_reales_transecto"]]) # Numérico
names(lista_revision[["muestreo_completo"]]) # Booleano

### Muestra_transecto_bentos_punto ###
names(lista_revision[["codigo"]]) # Catálogo
names(lista_revision[["altura_algas_cm"]]) # Numérico

### Muestra_transecto_bentos_linea ### (No hay datos)

### Muestra_transecto_bentos_porcentaje ###
names(lista_revision[["codigo"]]) # Catálogo
names(lista_revision[["cobertura"]]) # No debe haber negativos ni letras. Suman 100.

################################################################################

### Muestra_transecto_corales_info ###
names(lista_revision[["metodo"]]) # Catálogo
names(lista_revision[["nivel_agregacion_datos"]]) # Catálogo
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["longitud_transecto_m"]]) # Numérico
names(lista_revision[["ancho_transecto_m"]]) # Numérico
# Tamaño mínimo de la colonia: siempre 4cm.
names(lista_revision[["muestreo_completo"]]) # Booleano

### Muestra_transecto_corales_observacion ###
names(lista_revision[["codigo"]]) # Catálogo.
names(lista_revision[["clump"]]) # Natural o "C", "F", NA. Suponer que siempre se mide
names(lista_revision[["d1_max_diam_cm"]]) # Numérico
names(lista_revision[["d2_min_diam_cm"]]) # Numérico
names(lista_revision[["altura_maxima_cm"]]) # Numérico
names(lista_revision[["blanqueamiento"]]) # Catálogo. Suponer que siempre se mide
names(lista_revision[["porcentaje"]]) # Numérico. Suponer que siempre se mide
names(lista_revision[["mortalidad_antigua"]]) # Numérico
names(lista_revision[["mortalidad_reciente"]]) # Numérico
names(lista_revision[["mortalidad_total"]]) # Numérico
names(lista_revision[["mortalidad_transicion"]]) # Numérico
# Las mortalidades deben sumár máximo 100. Suponer que siempre se miden
names(lista_revision[["enfermedades"]]) # Catálogo. Suponer que siempre se mide
names(lista_revision[["sobrecrecimiento"]]) # Catálogo
names(lista_revision[["sobrecrecimiento__1"]]) # Catálogo
names(lista_revision[["depredacion"]]) # Catálogo. Suponer que siempre se mide
names(lista_revision[["lesiones"]]) # Catálogo. Suponer que siempre se mide

################################################################################

### Muestra_transecto_peces_info ###
names(lista_revision[["metodo"]]) # Catálogo
names(lista_revision[["nivel_agregacion_datos"]]) # Catálogo
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["longitud_transecto_m"]]) # Numérico
names(lista_revision[["ancho_transecto_m"]]) # Numérico
names(lista_revision[["peces_muestreados"]]) # Catálogo
names(lista_revision[["muestreo_completo"]]) # Booleano

### Muestra_transecto_peces_cuenta ###
names(lista_revision[["nombre_cientifico_abreviado"]]) # Catálogo
# Catálogo. Eliminar de catálogos duplicados menos frecuentes.
# Peces: cantidad de peces por especie (fila) y columna (tamaño).
names(lista_revision[["peces_tamanio_0cm_5cm"]]) # Numérico
names(lista_revision[["peces_tamanio_101cm_110cm"]]) # Numérico
names(lista_revision[["peces_tamanio_101cm_9999cm"]]) # Numérico
names(lista_revision[["peces_tamanio_111cm_120cm"]]) # Numérico
names(lista_revision[["peces_tamanio_11cm_20cm"]]) # Numérico
names(lista_revision[["peces_tamanio_191cm_200cm"]]) # Numérico
names(lista_revision[["peces_tamanio_21cm_30cm"]]) # Numérico
names(lista_revision[["peces_tamanio_31cm_40cm"]]) # Numérico
names(lista_revision[["peces_tamanio_41cm_50cm"]]) # Numérico
names(lista_revision[["peces_tamanio_51cm_60cm"]]) # Numérico
names(lista_revision[["peces_tamanio_61cm_70cm"]]) # Numérico
names(lista_revision[["peces_tamanio_6cm_10cm"]]) # Numérico
names(lista_revision[["peces_tamanio_71cm_80cm"]]) # Numérico
names(lista_revision[["peces_tamanio_81cm_90cm"]]) # Numérico
names(lista_revision[["peces_tamanio_91cm_100cm"]]) # Numérico
names(lista_revision[["peces_tamanio_101cm_9999cm"]]) # Numérico

################################################################################

### Muestra_transecto_invertebrados_info ###
names(lista_revision[["metodo"]]) # Catálogo
names(lista_revision[["nivel_agregacion_datos"]]) # Catálogo
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["longitud_transecto_m"]]) # Numérico
names(lista_revision[["ancho_transecto_m"]]) # Numérico
names(lista_revision[["muestreo_completo"]]) # Booleano

### Muestra_transecto_invertebrados_cuenta ###
names(lista_revision[["tipo"]]) # Catálogo
names(lista_revision[["conteo"]]) # Numérico. También las tablas de bentos agregados lo contienen

################################################################################

### Muestra_subcuadrante_de_transecto_reclutas_info ###
names(lista_revision[["cuadrante"]])
names(lista_revision[["nivel_agregacion_datos"]]) # Catálogo
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["sustrato"]]) # Catálogo

### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
names(lista_revision[["codigo"]]) # Catálogo. Eliminar "NA"
names(lista_revision[["categoria_tamanio"]]) # Sólo debe haber "No considerada", "R", "SC" y NA
names(lista_revision[["tamanio_minimo_cm"]]) # Numérico
names(lista_revision[["tamanio_maximo_cm"]]) # Numérico
names(lista_revision[["n"]]) # Natural. Número de reclutas agregados por código y categoría de tamaño (si es el caso).

################################################################################

### Muestra_transecto_complejidad_info ###
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["longitud_transecto_m"]]) # Numérico
names(lista_revision[["tamanio_cadena_m"]]) # Numérico

### Muestra_subcuadrante_de_transecto_complejidad_info ###
names(lista_revision[["observador"]]) # Homologarlos
names(lista_revision[["cuadrante"]]) # Numérico
names(lista_revision[["relieve"]]) # Numérico (complejidad por máximo relieve)

################################################################################

### Eliminar ###

# names(lista_revision[["abundancia"]])
# names(lista_revision[["anio_muestreo"]])
# names(lista_revision[["anio_publicacion"]])
# names(lista_revision[["anp_redundante"]])
# names(lista_revision[["curp_proyecto"]])
# names(lista_revision[["fision"]])
# names(lista_revision[["longitud_teorica_m_bentos"]])
# names(lista_revision[["longitud_teorica_m_corales"]])
# names(lista_revision[["longitud_teorica_m_invertebrados_agrra_v5"]])
# names(lista_revision[["longitud_teorica_m_invertebrados_otros"]])
# names(lista_revision[["longitud_teorica_m_peces"]])
# names(lista_revision[["necrosis"]])
# names(lista_revision[["neoplasia"]])
# names(lista_revision[["nombre_no_remuestreo"]]
# names(lista_revision[["nombre_remuestreo"]]
# names(lista_revision[["numero_sitios_agregados"]])
# names(lista_revision[["profundidad_media_m"]])
# names(lista_revision[["profundidad_media_m_sitio_redundante"]])
# names(lista_revision[["proyecto_aux"]])
# names(lista_revision[["region_ask"]])
# names(lista_revision[["region_healthy_reefs_redundante"]])
# names(lista_revision[["region_jordan"]])
# names(lista_revision[["s"]])
# names(lista_revision[["sitio_autor"]])
# names(lista_revision[["subtipo_arrecife_redundante"]])
# names(lista_revision[["subzona_habitat"]])
# names(lista_revision[["superficie_cm2"]])
# names(lista_revision[["unidades_profundidad"]])
# names(lista_revision[["X__1"]])
# names(lista_revision[["X__2"]])
# names(lista_revision[["zona_arrecife_redundante"]])
# names(lista_revision[["tejido_vivo"]])
# names(lista_revision[["tipo_arrecife_redundante"]])+
# names(lista_revision[["subzona_habitat"]])

### Otros ###
# names(lista_revision[["archivo_origen"]])
# names(lista_revision[["serie"]])

################################################################################
# 4. Encontrando los archivos de Excel que contienen los valores incorrectos
################################################################################

relacion_columnas_valores_incorrectos <- c(
  ".proposito" = "aluación de arrecifes fuera y dentro de ANPs.",
  ".muestreo_completo" = "0.83333333333333337",
  ".autor_administrador_proyecto" = "Parquer Nacional Arrecifes de Cozumel",
  ".hora" = 1,
  ".hora" = 2,
  ".proposito" = "Monitore de áreas naturales protegidas del norte de Quintana Roo.",
  
  # Que la variable de "cuadrante" tenga puros valores numéricos
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = "R1",
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = "R2",
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = "R3",
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = "R4",
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = "R5",
  "conacyt_greenpeace_2016_reclutas_desagregados.cuadrante" = "R6",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = "R1",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = "R2",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = "R3",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = "R4",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = "R5",
  "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla.cuadrante" = "R6",
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = "R1",
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = "R2",
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = "R3",
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = "R4",
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = "R5",
  "historicos_y_2017_cuadrante_reclutas_desagregados.cuadrante" = "R6",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = "R1",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = "R2",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = "R3",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = "R4",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = "R5",
  "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve.cuadrante" = "R6"
)

valores_a_revisar <- revisa_columnas_valores(lista_tablas_columnas_homologadas,
  relacion_columnas_valores_incorrectos) %>%
  group_by(tabla, campo, valor) %>%
  tally()

write_csv(valores_a_revisar,
  paste0(rutas_salida[5], "/valores_a_revisar.csv"))

################################################################################
# 5. Encontrando columnas y archivos con "NA"
################################################################################

# Se desea tener celdas vacías en lugar de la palabra "NA".

campos_con_palabra_na <- datos_globales_crudos %>%
  gather(variable, valor, -archivo_origen) %>%
  filter(valor == "NA") %>%
  unique()

write_csv(campos_con_palabra_na,
  paste0(rutas_salida[5], "/campos_con_palabra_na.csv"))


################################################################################
# 6. Revisando las tablas hoja de la base de datos
################################################################################

# Revisando las tablas hoja del esquema de datos, enfocándonos a las siguientes
# cuestiones:

# 1. Que cuando se traten de datos agregados por unidad de muestreo, no haya
# varios registros que se refieran al mismo nivel por la variable que se supone
# que se agregó. Por ejemplo: cuando se habla de porcentajes de cobertura de
# bentos por sitio / transecto, que no haya dos registros que se refieran al
# mismo código de bentos sobre la misma unidad de muestreo (deberían estar
# agregados).
# 2. Que los muestreos realizados pero que no tuvieron observaciones estén
# razonablemente declarados. Por ejemplo, si los transectos de peces para un
# determinado muestreo de sitio son sólo 3, y los que corresponden a otros
# muestreos de sitio en el mismo proyecto son 9, algo puede andar mal.
# 3. Para cada tabla hoja del esquema de datos se deberá calcular al menos
# una tabla de agregados por unidad de muestreo, ya con eso podemos detectar:
#   - Si existen unidades de muestreo duplicadas o introducidas múltiples veces.
#     Por ejemplo, al calcular cantidad de puntos declarados por muestreo de
#     transecto de bentos, esta cantidad saldrá muy elevada si hay muestreos de
#     transecto declarados múltiples veces.
#   - Si existen datos que están tomados a un nivel de muestreo más general y se
#     encuentran declarados en el archivo equivocado. Por ejemplo, si calculamos
#     cantidad de colonias de coral por muestreo de transecto, podemos revisar
#     fácilmente si hay datos en los cuáles "transecto == NA".
#   - Al tener un listado de todas las unidades de muestreo, es fácil detectar
#     si hay unidades de muestreo que no tuvieron observaciones, y por error no
#     se declararon.

names(lista_tablas_columnas_homologadas)
glimpse(tabla_revision)

################################################################################

### Muestra_sitio ###

# Archivos involucrados: todos

# 1. Revisando la cantidad de muestras de sitio por proyecto

muestras_sitio_cantidad_por_muestreo <- tabla_revision %>%
  distinct(
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio
  ) %>%
  group_by(nombre_proyecto) %>%
  summarise(
    numero_sitios = n()
  ) %>%
  arrange(nombre_proyecto)

# View(muestras_sitio_cantidad_por_muestreo)

write_csv(muestras_sitio_cantidad_por_muestreo,
  paste0(rutas_salida[5],
    "/muestras_sitio_cantidad_por_muestreo.csv"))

# 2. Revisando los aspectos muestrados por cada muestra de sitio. En realidad,
# se está revisando en cuántos archivos de Excel correspondientes a cada aspecto
# está registrado un muestreo de sitio.

muestras_sitio_aspectos_muestrados <- tabla_revision %>%
  distinct(
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio,
    archivo_origen
  ) %>%
  mutate(
    # Cada registro de esta tabla contendrá infdormación si un muestreo de sitio
    # específico está contenido en un archivo correspondiente a un aspecto específico.
    # Notar que estos registros se pueden sumar para ver en cuántos archivos
    # correspondientes al mismo aspecto está registrado una misma muestra de
    # sitio. Debe revisarse con cuidado si la misma muestra está en dos o más
    # archivos del mismo aspecto.
    evidencia_bentos = ifelse(stri_detect_fixed(archivo_origen, "bentos"), T, F),
    evidencia_corales = ifelse(stri_detect_fixed(archivo_origen, "corales"), T, F),
    evidencia_peces = ifelse(stri_detect_fixed(archivo_origen, "peces"), T, F),
    evidencia_invertebrados = ifelse(stri_detect_fixed(archivo_origen, "invertebrados"), T, F),
    evidencia_reclutas = ifelse(stri_detect_fixed(archivo_origen, "reclutas"), T, F),
    evidencia_complejidad = ifelse(stri_detect_fixed(archivo_origen, "complejidad"), T, F)
  ) %>%
  select(-archivo_origen) %>%
  group_by(
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio
  ) %>%
  summarise(
    numero_archivos_bentos = sum(evidencia_bentos),
    numero_archivos_corales = sum(evidencia_corales),
    numero_archivos_peces = sum(evidencia_peces),
    numero_archivos_invertebrados = sum(evidencia_invertebrados),
    numero_archivos_reclutas = sum(evidencia_reclutas),
    numero_archivos_complejidad = sum(evidencia_complejidad)
  )

# View(muestras_sitio_aspectos_muestrados)

write_csv(muestras_sitio_aspectos_muestrados,
  paste0(rutas_salida[5],
    "/muestras_sitio_aspectos_muestrados.csv"))

# 3. Revisando si en la tabla anterior hay muestras de sitio registradas en más
# de un archivo correspondiente a un mismo aspecto (en teoría no debería de pasar).

muestras_sitio_aspectos_redundantes <- muestras_sitio_aspectos_muestrados %>%
  gather("numero_archivos_aspecto", "numero_archivos", contains("numero_archivos")) %>%
  separate(numero_archivos_aspecto, into = c("numero", "archivos", "aspecto")) %>%
  select(
    -numero,
    -archivos
  ) %>%
  filter(numero_archivos > 1)

# View(muestras_sitio_aspectos_redundantes)
# Por suerte no hay muestras de sitio registradas en 2 o más archivos
# correspondientes al mismo aspecto

write_csv(muestras_sitio_aspectos_redundantes,
  paste0(rutas_salida[5],
    "/muestras_sitio_aspectos_redundantes.csv"))

################################################################################

### Muestra_sitio_bentos_porcentaje ###

# Archivos involucrados:
# - "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura"

# 1. Revisando que para cada muestra de sitio con información de porcentaje de
# cobertura de bentos por código, no haya registros distintos con el mismo código.

bentos_agregados_por_sitio_codigos_duplicados_mismo_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    codigo
  ) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)

# View(bentos_agregados_por_sitio_codigos_duplicados_mismo_sitio)
# Como sí los hay, agregarlos.

write_csv(bentos_agregados_por_sitio_codigos_duplicados_mismo_sitio,
  paste0(rutas_salida[5],
    "/bentos_agregados_por_sitio_codigos_duplicados_mismo_sitio.csv"))

# 2. Revisando que para cada muestra de sitio con información de porcentaje de
# cobertura por tipo de código, los porcentajes de cobertura sumen 100%. Este
# data frame en realidad se puede considerar un resumen del contenido de los
# archivos asociados.

bentos_agregados_por_sitio_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    total_cobertura = sum(cobertura, na.rm = TRUE) # La presencia de NA's se revisó en "2_revisar_listas_exceles.R"
  )

# View(bentos_agregados_por_sitio_resumen_contenido_archivos)
# Sí suman 100%

write_csv(bentos_agregados_por_sitio_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/bentos_agregados_por_sitio_resumen_contenido_archivos.csv"))

################################################################################

### Muestra_transecto_bentos_punto ###

# Archivos involucrados:
# - "conacyt_greenpeace_2016_bentos_desagregados"
# - "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados"
# - "historicos_y_2017_transecto_bentos_desagregados_pit_lit"

# 1. Creando una tabla auxiliar para detectar fácilmente si hubo muestreos de
# transecto o sitio que se duplicaron por error.

bentos_desagregados_por_transecto_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_bentos_desagregados",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

# View(bentos_desagregados_por_transecto_resumen_contenido_archivos)

write_csv(bentos_desagregados_por_transecto_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/bentos_desagregados_por_transecto_resumen_contenido_archivos.csv"))

# 2. Revisando cantidad de transectos por muestreo de sitio

bentos_desagregados_por_transecto_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_bentos_desagregados",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(bentos_desagregados_por_transecto_cantidad_transectos_por_sitio)

write_csv(bentos_desagregados_por_transecto_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/bentos_desagregados_por_transecto_cantidad_transectos_por_sitio.csv"))

################################################################################

### Muestra_transecto_bentos_porcentaje ###

# Archivos involucrados:
# - "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura"

# 1. Revisando que para cada muestra de transecto con información de porcentaje de
# cobertura de bentos por código, no haya registros distintos con el mismo código.

bentos_agregados_por_transecto_codigos_duplicados_mismo_transecto <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    codigo
  ) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)

# View(bentos_agregados_por_transecto_codigos_duplicados_mismo_transecto)
# Sí los hay, por ello, se sumarán.

write_csv(bentos_agregados_por_transecto_codigos_duplicados_mismo_transecto,
  paste0(rutas_salida[5],
    "/bentos_agregados_por_transecto_codigos_duplicados_mismo_transecto.csv"))

# 2. Revisando cantidad de transectos por muestreo de sitio

bentos_agregados_por_transecto_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(bentos_agregados_por_transecto_cantidad_transectos_por_sitio)

write_csv(bentos_agregados_por_transecto_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/bentos_agregados_por_transecto_cantidad_transectos_por_sitio.csv"))

# 3. Revisando que para cada muestra de transecto con información de porcentaje
# de cobertura por tipo de código, los porcentajes de cobertura sumen 100%. Este
# data frame en realidad se puede considerar un resumen del contenido de los
# archivos asociados.

bentos_agregados_por_transecto_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    total_cobertura = sum(cobertura, na.rm = TRUE) # La presencia de NA's se revisó en "2_revisar_listas_exceles.R"
  )

# View(bentos_agregados_por_transecto_resumen_contenido_archivos)
# Revisarlo con Esme, muchos no suman ni cercano a 100%

write_csv(bentos_agregados_por_transecto_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/bentos_agregados_por_transecto_resumen_contenido_archivos.csv"))

################################################################################

### Muestra_transecto_corales_observacion ###

# Archivos involucrados:
# - "conacyt_greenpeace_2016_corales_desagregados"
# - "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"

# 1. Revisando si hay muestras de transecto de corales que no tuvieron observaciones.

transectos_corales_sin_observaciones <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_corales_desagregados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
  )) %>%
  # Primero, sólo nos quedaremos con un registro por código distinto para cada
  # muestra de transecto, ya que queremos encontrar los transectos que tienen
  # puros registros de colonias con "codigo == NA" (no importa cuántos tengan)
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    codigo
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio,
    transecto
  ) %>%
  mutate(
    numero_registros = n()
  ) %>%
  # Quedándonos sólo con los transectos que tienen un único registro y es NA
  filter(is.na(codigo), numero_registros == 1) %>%
  select(
    archivo_origen,
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio,
    transecto
  )

# View(transectos_corales_sin_observaciones)
# Todos los transectos de corales tienen observaciones

write_csv(transectos_corales_sin_observaciones,
  paste0(rutas_salida[5],
    "/transectos_corales_sin_observaciones.csv"))

# 2. Revisando cantidad de transectos por muestreo de sitio.

corales_desagregados_por_transecto_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_corales_desagregados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(corales_desagregados_por_transecto_cantidad_transectos_por_sitio)

write_csv(corales_desagregados_por_transecto_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/corales_desagregados_por_transecto_cantidad_transectos_por_sitio.csv"))

# 3. Creando una tabla auxiliar con la cantidad de observaciones por transecto.
# Este data frame en realidad se puede considerar un resumen del contenido de
# los archivos asociados.

corales_desagregados_por_transecto_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_corales_desagregados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

# View(corales_desagregados_por_transecto_resumen_contenido_archivos)
# Hay datos que están declarados por sitio ("transecto == NA").

write_csv(corales_desagregados_por_transecto_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/corales_desagregados_por_transecto_resumen_contenido_archivos.csv"))

################################################################################

### Muestra_transecto_peces_cuenta ###

# Archivos involucrados:
# - "conacyt_greenpeace_2016_peces_agregados_especie_talla"
# - "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados"
# - "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla"
# - "historicos_y_2017_transecto_peces_desagregados_especie_talla"

# 1. Revisando si hay muestras de transecto de peces que no tuvieron observaciones

transectos_peces_sin_observaciones <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",                               
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  )) %>%
  # Primero, sólo nos quedaremos con un registro por nombre científico distinto
  # para cada muestra de transecto, ya que queremos encontrar los transectos que
  # tienen puros registros de peces con "nombre_cientifico_abreviado == NA"
  # (no importa cuántos tengan)
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    nombre_cientifico_abreviado
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio,
    transecto
  ) %>%
  mutate(
    numero_registros = n()
  ) %>%
  # Quedándonos sólo con los transectos que tienen un único registro y es NA
  filter(is.na(nombre_cientifico_abreviado) & numero_registros == 1 ) %>%
  select(
    archivo_origen,
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio,
    transecto
  )

# View(transectos_peces_sin_observaciones)
# Sí hay transectos de peces sin observaciones

write_csv(transectos_peces_sin_observaciones, paste0(rutas_salida[5],
  "/transectos_peces_sin_observaciones.csv"))

# 2. Revisando si hay muestras de transecto con información de cuentas de
# peces que tienen registros distintos con el mismo código en los archivos de Excel 

peces_agregados_por_transecto_codigos_duplicados_mismo_transecto <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",                               
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla"
    # "historicos_y_2017_transecto_peces_desagregados_especie_talla" # Quitar los desagregados
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    nombre_cientifico_abreviado
  ) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)

# View(peces_agregados_por_transecto_codigos_duplicados_mismo_transecto)
# Sí hay, agregarlos

write_csv(peces_agregados_por_transecto_codigos_duplicados_mismo_transecto,
  paste0(rutas_salida[5],
    "/peces_agregados_por_transecto_codigos_duplicados_mismo_transecto.csv"))

# 3. Revisando cantidad de transectos por muestreo de sitio.

peces_por_transecto_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",                               
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(peces_por_transecto_cantidad_transectos_por_sitio)

write_csv(peces_por_transecto_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/peces_por_transecto_cantidad_transectos_por_sitio.csv"))

# 4. Creando una tabla auxiliar con la cantidad de registros por transecto 
# (registros = renglones de Excel). Este data frame en realidad se puede
# considerar un resumen del contenido de los archivos asociados.

peces_por_transecto_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",                               
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

View(peces_por_transecto_resumen_contenido_archivos)

write_csv(peces_por_transecto_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/peces_por_transecto_resumen_contenido_archivos.csv"))

################################################################################

### Muestra_transecto_invertebrados_cuenta ###

# Archivos involucrados:
# - "conacyt_greenpeace_2016_invertebrados_desagregados"
# - "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"

# 1. Revisando si hay muestras de transecto de invertebrados que no tuvieron
# observaciones.

transectos_invertebrados_sin_observaciones <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_invertebrados_desagregados",                               
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
  )) %>%
  # Primero, sólo nos quedaremos con un registro por código distinto para cada
  # muestra de transecto, ya que queremos encontrar los transectos que tienen
  # puros registros de invertebrados con "tipo == NA" (no importa cuántos tengan)
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    tipo
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto,
    nombre_sitio,
    identificador_muestreo_sitio,
    transecto
  ) %>%
  mutate(
    numero_registros = n()
  ) %>%
  # Quedándonos sólo con los transectos que tienen un único registro y es NA
  filter(is.na(tipo), numero_registros == 1) %>%
  select(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  )

# View(transectos_invertebrados_sin_observaciones)
# Hay muchos transectos de invertebrados sin observaciones

write_csv(transectos_invertebrados_sin_observaciones,
  paste0(rutas_salida[5],
    "/transectos_invertebrados_sin_observaciones.csv"))

# 2. Revisando que para cada muestra de transecto con información de cuentas de
# invertebrados, no haya en los archivos de Excel registros distintos con el mismo
# tipo.

invertebrados_agregados_por_transecto_tipos_duplicados_mismo_transecto <-
  tabla_revision %>%
  filter(archivo_origen %in% c(
    # conacyt_greenpeace_2016_invertebrados_desagregados,
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    tipo
  ) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)

# View(invertebrados_agregados_por_transecto_tipos_duplicados_mismo_transecto)
# Hay muchos tipos NA duplicados por muestreo de transecto... tener cuidado con
# esto a la hora de integrar los datos. Además, Esme debe revisarlos, ya que parece
# que hay muchos muestreos de sitio introducidos varias veces.

write_csv(invertebrados_agregados_por_transecto_tipos_duplicados_mismo_transecto,
  paste0(rutas_salida[5],
    "/invertebrados_agregados_por_transecto_tipos_duplicados_mismo_transecto.csv"))

# 3. Revisando cantidad de transectos por muestreo de sitio.

invertebrados_por_transecto_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_invertebrados_desagregados",
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(invertebrados_por_transecto_cantidad_transectos_por_sitio)

write_csv(invertebrados_por_transecto_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/invertebrados_por_transecto_cantidad_transectos_por_sitio.csv"))

# 4. Creando una tabla auxiliar con la cantidad de registros por transecto 
# (registros = renglones de Excel). Este data frame en realidad se puede
# considerar un resumen del contenido de los archivos asociados.

invertebrados_por_transecto_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_invertebrados_desagregados",                               
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

# View(invertebrados_por_transecto_resumen_contenido_archivos)
# Parece que hay transectos con una cantidad excesiva de observaciones. Hay que
# tener cuidado con estos.

write_csv(invertebrados_por_transecto_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/invertebrados_por_transecto_resumen_contenido_archivos.csv"))

################################################################################

### Muestra_transecto_complejidad_info ###

# Archivos involucrados:
# - "conacyt_greenpeace_2016_complejidad"
# - "historicos_y_2017_transecto_complejidad_desagregada_cadena"

# 1. Creando una tabla auxiliar con la cantidad de registros por transecto 
# (registros = renglones de Excel). Este data frame en realidad se puede
# considerar un resumen del contenido de los archivos asociados.

complejidad_por_transecto_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_complejidad",                               
    "historicos_y_2017_transecto_complejidad_desagregada_cadena"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

# View(complejidad_por_transecto_resumen_contenido_archivos)

write_csv(complejidad_por_transecto_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/complejidad_por_transecto_resumen_contenido_archivos.csv"))

# 2. Revisando cantidad de transectos por muestreo de sitio.

complejidad_por_transecto_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_complejidad",
    "historicos_y_2017_transecto_complejidad_desagregada_cadena"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(complejidad_por_transecto_cantidad_transectos_por_sitio)

write_csv(complejidad_por_transecto_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/complejidad_por_transecto_cantidad_transectos_por_sitio.csv"))

################################################################################

### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###

# Archivos involucrados:
# - "conacyt_greenpeace_2016_reclutas_desagregados"
# - "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla"
# - "historicos_y_2017_cuadrante_reclutas_desagregados".
# Nota: con respecto a los archivos:
# "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla
# e "historicos_y_2017_cuadrante_reclutas_desagregados" se comprendió erróneamente
# el concepto de "reclutas desagredasos"... no se entendió que los reclutas
# desagregados son los que fueron medidos individualmente, en otro caso, debían
# ser agregados por muestreo, código de especie y categoría de talla. Para evitar
# problemas, se decidió modificar el esquema de datos para que este error no
# fuera importante.

# 1. Revisando las muestras de cuadrante de reclutas que tuvieron observaciones.

cuadrantes_reclutas_con_observaciones <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_reclutas_desagregados",                               
    "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_cuadrante_reclutas_desagregados"
  )) %>%
  # Filtraremos las observaciones con "codigo == NA", de esta manera, se eliminarán
  # todos los cuadrantes con puros NA's en "código"
  filter(!is.na(codigo)) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    cuadrante
  )

# View(cuadrantes_reclutas_con_observaciones)

write_csv(cuadrantes_reclutas_con_observaciones,
  paste0(rutas_salida[5],
    "/cuadrantes_reclutas_con_observaciones.csv"))

# 2. Revisando que para cada muestra de cuadrante con información de cuentas de
# reclutas, no haya en los archivos de Excel registros distintos con el mismo
# código de especie y categoría de talla

reclutas_agregados_por_cuadrante_codigos_tamanios_duplicados_mismo_cuadrante <-
  tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    cuadrante,
    codigo,
    categoria_tamanio
  ) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# View(reclutas_agregados_por_cuadrante_codigos_tamanios_duplicados_mismo_cuadrante)
# Hay muchas combinaciones de código de especie y categoría de talla duplicadas
# para los mismos cuadrantes. Esme tiene que revisarlo porque yo simplemente
# agregaré los datos.

write_csv(reclutas_agregados_por_cuadrante_codigos_tamanios_duplicados_mismo_cuadrante,
  paste0(rutas_salida[5],
    "/reclutas_agregados_por_cuadrante_codigos_tamanios_duplicados_mismo_cuadrante.csv"))

# 3. Revisando cantidad de cuadrantes por muestreo de transecto

reclutas_por_cuadrante_cantidad_cuadrantes_por_transecto <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_reclutas_desagregados",                               
    "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_cuadrante_reclutas_desagregados"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    cuadrante
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    numero_cuadrantes = n()
  ) %>%
  arrange(numero_cuadrantes)

# View(reclutas_por_cuadrante_cantidad_cuadrantes_por_transecto)

write_csv(reclutas_por_cuadrante_cantidad_cuadrantes_por_transecto,
  paste0(rutas_salida[5],
    "/reclutas_por_cuadrante_cantidad_cuadrantes_por_transecto.csv"))

# 4. Revisando cantidad de transectos por muestreo de sitio.

reclutas_por_cuadrante_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_reclutas_desagregados",                               
    "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_cuadrante_reclutas_desagregados"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(reclutas_por_cuadrante_cantidad_transectos_por_sitio)

write_csv(reclutas_por_cuadrante_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/reclutas_por_cuadrante_cantidad_transectos_por_sitio.csv"))

# 5. Creando una tabla auxiliar con la cantidad de registros por cuadrante 
# (registros = renglones de Excel). Este data frame en realidad se puede
# considerar un resumen del contenido de los archivos asociados.

reclutas_por_cuadrante_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_reclutas_desagregados",                               
    "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_cuadrante_reclutas_desagregados"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    cuadrante
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

# View(reclutas_por_cuadrante_resumen_contenido_archivos)
# Parece que hay cuadrantes con una cantidad excesiva de observaciones. Hay que
# tener cuidado con estos.

write_csv(reclutas_por_cuadrante_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/reclutas_por_cuadrante_resumen_contenido_archivos.csv"))

################################################################################

### Muestra_subcuadrante_de_transecto_complejidad_info ###

# Archivos involucrados:
# - "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve"

# 1. Revisando cantidad de cuadrantes por muestreo de transecto

complejidad_por_cuadrante_cantidad_cuadrantes_por_transecto <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    cuadrante
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  summarise(
    numero_cuadrantes = n()
  ) %>%
  arrange(numero_cuadrantes)

View(complejidad_por_cuadrante_cantidad_cuadrantes_por_transecto)

write_csv(complejidad_por_cuadrante_cantidad_cuadrantes_por_transecto,
  paste0(rutas_salida[5],
    "/complejidad_por_cuadrante_cantidad_cuadrantes_por_transecto.csv"))

# 2. Revisando cantidad de transectos por muestreo de sitio.

complejidad_por_cuadrante_cantidad_transectos_por_sitio <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve"
  )) %>%
  distinct(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto
  ) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio # identificador de muestreo para proyectos CONACyT / GreenPeace
  ) %>%
  summarise(
    numero_transectos = n()
  ) %>%
  arrange(numero_transectos)

# View(complejidad_por_cuadrante_cantidad_transectos_por_sitio)

write_csv(complejidad_por_cuadrante_cantidad_transectos_por_sitio,
  paste0(rutas_salida[5],
    "/complejidad_por_cuadrante_cantidad_transectos_por_sitio.csv"))

# 3. Creando una tabla auxiliar con la cantidad de registros por cuadrante 
# (registros = renglones de Excel). Este data frame en realidad se puede
# considerar un resumen del contenido de los archivos asociados.

complejidad_por_cuadrante_resumen_contenido_archivos <- tabla_revision %>%
  filter(archivo_origen %in% c(
    "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve"
  )) %>%
  group_by(
    archivo_origen,
    nombre_proyecto, # nombre_del_muestreo con información de remuestreos de sitio,
    nombre_sitio,
    identificador_muestreo_sitio, # identificador de muestreo para proyectos CONACyT / GreenPeace
    transecto,
    cuadrante
  ) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

# View(complejidad_por_cuadrante_resumen_contenido_archivos)
# Parece que hay transectos con una cantidad excesiva de observaciones. Hay que
# tener cuidado con estos.

write_csv(complejidad_por_cuadrante_resumen_contenido_archivos,
  paste0(rutas_salida[5],
    "/complejidad_por_cuadrante_resumen_contenido_archivos.csv"))

################################################################################
# 7. Notas adicionales acerca de las columnas revisadas
################################################################################

#                                 archivo_origen                                    metodo
# 1                          conacyt_greenpeace_2016_bentos_desagregados_v3         PIT
# 2     historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura         CPF
# 3            historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados     PIT
# 4 historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura         LIT
# 5 historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura         PIT
# 6 historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura         Otro
# 7                  historicos_y_2017_transecto_bentos_desagregados_pit_lit        PIT
# Notar que bentos desagregados sólo son PIT, y los bentos agregados por sitio son CPF.

# Hay que homologar "observaciones" y "notas", ya que son columnas presentes en
# todos los Exceles, pero sólo se utiliza una.

# Hay que revisar "sobrecrecimiento" y "sobrecrecimiento__1", ya que a veces
# "sobrecrecimiento__1" está llena mientras "sobrecrecimiento" está vacía.

# Hay que recalcular "documento" --> "fuente", puesto que lo queremos para diferenciar
# datos que provienen de una base de datos (como Healthy Reefs) de los que no

# Por ahora, "rugosidad.longitud_lineal_fija" será marcada como TRUE.

################################################################################

# Para los proyectos CONACyT y GreenPeace:
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

# Por ello, para este proyecto, siempre se tiene que dar que el número de
# transectos de bentos sea mayor que el de corales, reclutas, complejidad e
# invertebrados en transecto de bentos.
# De igual manera, el número de transectos de peces debe ser mayor que el de
# invertebrados sacados de peces.
# Revisando muestreos de sitio con cantidad anómala de transectos
