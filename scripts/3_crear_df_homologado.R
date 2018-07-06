# En este script se crea un data frame homologado con la información en
# "lista_tablas_columnas_homologadas".

# El procedimiento seguido es el siguiente:
# 1. Agregar los data frames en "lista_tablas_columnas_homologadas" en un sólo
# data frame.
# 2. Revisar la tabla con la información integrada, para encontrar campos con
# valores incorrectos.
# 3. Crear un nuevo data frame a partir del que contiene los datos crudos. Este
# data frame contendrá las columnas necesarias para definir las tablas diseñadas
# en el esquema de datos correspondientes.

# Se supone que los datos están perfectos a la hora de hacer el data frame, anterior.
# Por ejemplo, que si el sitio no está dentro de un ANP hay un NA y no una palabra
# "NA". Toda imperfección en la base de datos de Excel la tendrá que arreglar Esme.

# En scripts subsecuentes, el data frame producido por este script se segmentará
# apropiadamente en tablas.

# Cargando archivo de configuración y funciones auxiliares
library("readr")
library("doMC") # procesamiento en paralelo
source("config.R")
source("funciones_auxiliares.R")

#Revisar número de nucleos en la terminal (Mac): sysctl hw.ncpu
registerDoMC(4)

################################################################################
# 1. Creando data frame con la información integrada:
################################################################################

# Leyendo lista de tablas con columnas homologadas
lista_tablas_columnas_homologadas <- readRDS(
  paste0(ruta_salidas_1_homologar_columnas, "/lista_tablas_columnas_homologadas.RData"))

# Agregando dichas tablas en un solo data frame
datos_globales_crudos <- Reduce(rbind.fill, lista_tablas_columnas_homologadas) %>%
  mutate(
    id = 1:nrow(.)
  )

################################################################################
# 2. Revisando la tabla con la información integrada:
################################################################################

names(lista_tablas_columnas_homologadas)
crear_resumen_columnas_df(lista_tablas_columnas_homologadas) %>%
  glimpse()

lista_revision <- revisa_valores(datos_globales_crudos)
names(lista_revision)

################################################################################
# *: llave natural necesaria para crear tablas

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
names(lista_revision[["hora"]]) # Natural. Esme debe revisar el formato 24h #####
names(lista_revision[["minutos"]]) # Entero #####
names(lista_revision[["pais"]])
names(lista_revision[["region_healthy_reefs"]]) # Catálogo
names(lista_revision[["localidad"]])
names(lista_revision[["tipo_arrecife"]]) # Ya se revisó al comparar con catálogos #####
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
names(lista_revision[["categoria_tamanio"]]) # Sólo debe haber "No considerada", "R", "SC" y NA #####
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
# names(lista_revision[["id"]])
# names(lista_revision[["archivo_origen"]])
# names(lista_revision[["serie"]])

################################################################################

# Encontrando archivos de Excel que contienen los valores incorrectos:

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

saveRDS(valores_a_revisar,
  paste0(ruta_salidas_3_crear_df_homologado, "/valores_a_revisar.RDS"))
write_csv(valores_a_revisar,
  paste0(ruta_salidas_3_crear_df_homologado, "/valores_a_revisar.csv"))

################################################################################

# Notas acerca de las columnas revisadas:

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

# Revisando "NA"'s en las columnas

datos_globales_crudos %>%
  gather(variable, valor, -archivo_origen) %>%
  filter(valor == "NA") %>%
  unique() %>%
  View()
  
################################################################################
# 3. Creando la tabla con información selecta para la integración
################################################################################

# Creando una tabla con las columnas de importancia la base de datos. Cabe
# destacar que se limpiaron, reestructuraron y cambió el tipo de datos de algunas
# de estas columnas.

datos_globales_columnas_selectas <- datos_globales_crudos %>%
  
  ## Mutate numeric eliminará por completo cualquier valor no numérico en las
  ## columnas seleccionadas
  mutate_numeric(
    
    ### Muestreo ###
    "anio_inicio_proyecto",
    "anio_termino_proyecto",
    
    ### Muestra_sitio ###
    "anio",
    "mes",
    "dia",
    "hora",
    "minutos",
    "latitud",
    "longitud",
    "temperatura_c",
    "profundidad_media_m_sitio",
    
    ### Muestra_transecto ###
    "profundidad_inicial_m",
    "profundidad_final_m",
    "longitud_cuadrante_m",
    "ancho_cuadrante_m",
    
    ### Muestra_sitio_bentos_info ###
    "puntos_o_cm_reales_transecto",
    
    ### Muestra_sitio_bentos_porcentaje ###
    "cobertura",
    
    ### Muestra_transecto_bentos_info ###
    "longitud_transecto_m",
    # "puntos_o_cm_reales_transecto",
    
    ### Muestra_transecto_bentos_punto ###
    "altura_algas_cm",
    
    ### Muestra_transecto_bentos_porcentaje ###
    # "cobertura",
    
    ### Muestra_transecto_corales_info ###
    # "longitud_transecto_m",
    "ancho_transecto_m",
    
    ### Muestra_transecto_corales_observacion ###
    "d1_max_diam_cm",
    "d2_min_diam_cm",
    "altura_maxima_cm",
    "porcentaje",
    "mortalidad_antigua",
    "mortalidad_reciente",
    "mortalidad_total",
    "mortalidad_transicion",
    
    ### Muestra_transecto_peces_info ###
    # "longitud_transecto_m",
    # "ancho_transecto_m",
    
    ### Muestra_transecto_peces_cuenta ###
    "peces_tamanio_0cm_5cm",
    "peces_tamanio_101cm_110cm",
    "peces_tamanio_101cm_9999cm",
    "peces_tamanio_111cm_120cm",
    "peces_tamanio_11cm_20cm",
    "peces_tamanio_191cm_200cm",
    "peces_tamanio_21cm_30cm",
    "peces_tamanio_31cm_40cm",
    "peces_tamanio_41cm_50cm",
    "peces_tamanio_51cm_60cm",
    "peces_tamanio_61cm_70cm",
    "peces_tamanio_6cm_10cm",
    "peces_tamanio_71cm_80cm",
    "peces_tamanio_81cm_90cm",
    "peces_tamanio_91cm_100cm",
    
    ### Muestra_transecto_invertebrados_info ###
    # "longitud_transecto_m",
    # "ancho_transecto_m",
    
    ### Muestra_transecto_invertebrados_cuenta ###
    "conteo",
    
    ### Muestra_subcuadrante_de_transecto_reclutas_info ###
    # "cuadrante",
    
    ### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
    "tamanio_minimo_cm",
    "tamanio_maximo_cm",
    "n",
    
    ### Muestra_transecto_complejidad_info ###
    # "longitud_transecto_m",
    "tamanio_cadena_m",
    
    ### Muestra_subcuadrante_de_transecto_complejidad_info ###
    # "cuadrante",
    "relieve",
    warnings = FALSE
  ) %>%
  
  ## Reestructurando columnas booleanas:
  mutate(
    area_no_pesca = case_when(
      tolower(area_no_pesca) == "si" ~ TRUE,
      tolower(area_no_pesca) == "no" ~ FALSE,
      TRUE ~ NA
    ),
    
    muestreo_completo = case_when(
      tolower(muestreo_completo) == "si" ~ TRUE,
      tolower(muestreo_completo) == "no" ~ FALSE,
      TRUE ~ NA
    ),
    
    transecto_fijo = case_when(
      tolower(transecto_fijo) == "si" ~ TRUE,
      tolower(transecto_fijo) == "no" ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  
  ## Quitando strings no deseados (como "NA")
  mutate(
    categoria_tamanio = case_when(
      tolower(categoria_tamanio) == "no considerada" ~ "No considerada",
      tolower(categoria_tamanio) == "r" ~ "R",
      tolower(categoria_tamanio) == "sc" ~ "SC",
      TRUE ~ NA_character_
    ),
    clump = case_when(
      stri_detect_fixed(tolower(clump), "f") ~ "FRAG",
      stri_detect_fixed(tolower(clump), "c") ~ "CLUMP",
      stri_detect_regex(clump, "[0-9]") ~ clump,
      TRUE ~ NA_character_
    )
  ) %>%
  
  ## Arreglando columnas más complejas:
  mutate(
    # Todas las tablas tienen ambas columnas ("notas" y "observaciones"), pero
    # sólo ocupan una a la vez
    comentarios = ifelse(!is.na(notas), notas, observaciones),
    
    # A veces está lleno el campo "sobrecrecimiento__1" y vacío el de "sobrecrecimiento"
    sobrecrecimiento = ifelse(is.na(sobrecrecimiento), sobrecrecimiento__1, sobrecrecimiento),
    sobrecrecimiento__1 = ifelse(sobrecrecimiento == sobrecrecimiento__1, NA, sobrecrecimiento__1)
  ) %>%
  
  select(
    -notas,
    -observaciones,
    sobrecrecimiento_1 = sobrecrecimiento__1
  ) %>%
  
  ## Eliminando columnas innecesarias:
  select(
    -abundancia,
    -anio_muestreo,
    -anio_publicacion,
    -anp_redundante,
    -curp_proyecto,
    -documento,
    -fision,
    -fuente,
    -longitud_teorica_m_bentos,
    -longitud_teorica_m_corales,
    -longitud_teorica_m_invertebrados_agrra_v5,
    -longitud_teorica_m_invertebrados_otros,
    -longitud_teorica_m_peces,
    -necrosis,
    -neoplasia,
    -nombre_no_remuestreo,
    -nombre_remuestreo,
    -numero_sitios_agregados,
    -profundidad_media_m,
    -profundidad_media_m_sitio_redundante,
    -proyecto_aux,
    -region_ask,
    -region_healthy_reefs_redundante,
    -region_jordan,
    -s,
    -sitio_autor,
    -subtipo_arrecife_redundante,
    -subzona_habitat,
    -superficie_cm2,
    -tejido_vivo,
    -tipo_arrecife_redundante,
    -unidades_profundidad,
    -X__1,
    -X__2,
    -x__1,
    -zona_arrecife_redundante
  )

################################################################################

# Creando la tabla con la información lista para ser integrada:

datos_globales <- datos_globales_columnas_selectas %>%

  ### Muestreo ####

  mutate(
    
    # En la columna "nombre_proyecto", se codificó al final una letra A/B/C para
    # distinguir entre remuestreos del mismo sitio, dentro del mismo proyecto. Esta
    # información la extraeré en una columna nueva. Esto no se hizo para los datos
    # de CONACyT / GreenPeace (en los que se tiene la columna "identificador_muestreo_sitio")
    # Supuesto: ningún proyecto termina con "A", "B" o "C", a menos que sea indicativo 
    # de un remuestreo de sitio dentro del mismo proyecto.
    Muestreo.nombre_si_A = stri_match_first(nombre_proyecto, regex = "(.*)A$")[,2],
    Muestreo.nombre_si_B = stri_match_first(nombre_proyecto, regex = "(.*)B$")[,2],
    Muestreo.nombre_si_C = stri_match_first(nombre_proyecto, regex = "(.*)C$")[,2],
    Muestreo.nombre = ifelse(!is.na(Muestreo.nombre_si_A), Muestreo.nombre_si_A,
      ifelse(!is.na(Muestreo.nombre_si_B), Muestreo.nombre_si_B,
        ifelse(!is.na(Muestreo.nombre_si_C), Muestreo.nombre_si_C,
          nombre_proyecto
    ))),
    
    # Variable que es A o B si el sitio fue remuestreado en el mismo muestreo,
    # y NA e.o.c
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo = ifelse(!is.na(Muestreo.nombre_si_A), "A",
      ifelse(!is.na(Muestreo.nombre_si_B), "B",
        ifelse(!is.na(Muestreo.nombre_si_C), "C", NA
    )))
  ) %>%
  rename(
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace = identificador_muestreo_sitio
  ) %>%
  
  select(
    -nombre_proyecto,
    -Muestreo.nombre_si_A,
    -Muestreo.nombre_si_B,
    -Muestreo.nombre_si_C
  ) %>%
  
  rename(
    Muestreo.descripcion = proposito,
    Muestreo.proposito = tema,
    Muestreo.area_estudio = localidad_proyecto,
    Muestreo.organizacion = institucion,
    Muestreo.suborganizacion = suborganizacion,
    Muestreo.encargado = autor_administrador_proyecto,
    Muestreo.contacto = contacto,
    Muestreo.referencia = cita
  ) %>%
  
  mutate(
    Muestreo.comentarios = NA_character_
  ) %>%
  
  rename(
    Muestreo.nombre_proyecto = titulo,
    Muestreo.anio_inicio_proyecto = anio_inicio_proyecto,
    Muestreo.anio_termino_proyecto = anio_termino_proyecto
  ) %>%
  
  # mutate(
  #   Muestreo.etapa_revision,
  #   Muestreo.compatibilidad_cliente
  # ) %>%
  
  ### Muestra_sitio ###
  
  rename(
    Muestra_sitio.nombre = nombre_sitio
  ) %>%
  mutate(
    nombre_original = estandariza_strings(nombre_original),
    Muestra_sitio.nombre_original = ifelse(!is.na(nombre_original),
      nombre_original, Muestra_sitio.nombre)
  ) %>%
  select(
    -nombre_original
  ) %>%
  mutate(
    # Agregando un 0 antes de meses y días cuando se requiera:
    mes = ifelse(as.numeric(mes) %in% c(1:9), paste0("0", mes), mes),
    dia = ifelse(as.numeric(dia) %in% c(1:9), paste0("0", dia), dia),
    Muestra_sitio.fecha = paste0(anio, "-", mes, "-", dia)
  ) %>%
  select(
    -anio,
    -mes,
    -dia
  ) %>%
  mutate(
    # Agregando un 0 antes de horas y minutos cuando se requiera:
    hora = ifelse(as.numeric(hora) %in% c(0:9), paste0("0", hora), hora),
    minutos = ifelse(as.numeric(minutos) %in% c(0:9), paste0("0", minutos), minutos),
    Muestra_sitio.hora = ifelse(
      is.na(hora) | is.na(minutos), NA,
      paste0(hora, ":", minutos))
  ) %>%
  select(
    -hora,
    -minutos
  ) %>%
  rename(
    Muestra_sitio.pais = pais,
    Muestra_sitio.region_healthy_reefs = region_healthy_reefs,
    Muestra_sitio.localidad = localidad,
    Muestra_sitio.tipo_arrecife = tipo_arrecife,
    Muestra_sitio.subtipo_arrecife = subtipo_arrecife,
    Muestra_sitio.zona_arrecifal = zona_arrecife
  ) %>%
  mutate(
    # Estoy suponiendo que Esme ya identificó todos los muestreos de sitio que
    # pertenecen a un ANP
    Muestra_sitio.dentro_area_natural_protegida = ifelse(!is.na(anp), TRUE, FALSE)
  ) %>%
  rename(
    Muestra_sitio.nombre_area_natural_protegida = anp,
    Muestra_sitio.dentro_area_no_pesca = area_no_pesca,
    Muestra_sitio.latitud = latitud,
    Muestra_sitio.longitud = longitud
  ) %>%
  mutate(
    Muestra_sitio.datum = "WGS84"
  ) %>%
  rename(
    Muestra_sitio.metodo_seleccion = metodo_seleccion_sitios,
    Muestra_sitio.metodologia = protocolo,
    Muestra_sitio.temperatura_c = temperatura_c,
    Muestra_sitio.profundidad_m = profundidad_media_m_sitio,
    Muestra_sitio.comentarios = comentarios
  ) %>%
  mutate(
    Muestra_sitio.excluir = FALSE,
    Muestra_sitio.datos_abiertos = ifelse(stri_detect_fixed(archivo_origen, "privados"), FALSE, TRUE),
    # Muestra_sitio.etapa_revision
    # Muestra_sitio.compatibilidad_cliente
    Muestra_sitio.fuente = ifelse(stri_detect_fixed(Muestreo.nombre, "Base de datos HRI"), "Base de datos", "Otro")
  ) %>%
  
  ### Muestra_transecto ###
  
  rename(
    Muestra_transecto.nombre = transecto,
    Muestra_transecto.transecto_fijo = transecto_fijo,
    Muestra_transecto.profundidad_inicial_m = profundidad_inicial_m,
    Muestra_transecto.profundidad_final_m = profundidad_final_m
  ) %>%
  
  # Calculando "Muestra_transecto.subcuadrantes_planeados":
  # Para cada muestra de transecto:
  group_by(
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
    Muestra_transecto.nombre) %>%
  mutate(
    # Con que un registro de dicha muestra de transecto tenga información de cuadrante,
    # quiere decir que el transecto tiene subcuadrantes planeados
    Muestra_transecto.subcuadrantes_planeados = sum(!is.na(cuadrante)) %>%
      as.logical()
  ) %>%
  ungroup() %>%
  rename(
    # Muestra_transecto.seleccion_aleatoria_centros_subcuadrantes,
    # Muestra_transecto.distancia_centros_subcuadrantes_m
  ) %>%
  
  # Calculando "Muestra_transecto.longitud_subcuadrante_m" y
  # "Muestra_transecto.ancho_subcuadrante_m" para cada muestra de
  # transecto. Si tiene subcuadrantes planeados, se pone el primer valor de la
  # columna "longitud_cuadrante_m", al ordenarla por ella misma (los NA's) quedan
  # al final, en otro caso se pone NA
  group_by(
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
    Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
    Muestra_transecto.nombre) %>%
  mutate(
    Muestra_transecto.longitud_subcuadrante_m = ifelse(Muestra_transecto.subcuadrantes_planeados,
      first(longitud_cuadrante_m, order_by = longitud_cuadrante_m), NA),
    Muestra_transecto.ancho_subcuadrante_m = ifelse(Muestra_transecto.subcuadrantes_planeados,
      first(ancho_cuadrante_m, order_by = ancho_cuadrante_m), NA)
  ) %>%
  ungroup() %>%
  mutate(
    Muestra_transecto.comentarios = NA_character_
  ) %>%
  select(
    -longitud_cuadrante_m,
    -ancho_cuadrante_m
  ) %>%
  
  ### Muestra_*_info ###
  
  # Para cualquier *:
  # - Muestra_*_info.metodo_muestreo = metodo 
  # - Muestra_*_info.nivel_agregacion_datos = nivel_agregacion_datos
  # - Muestra_*_info.observador = observador
  # - Muestra_*_info.longitud_muestreada_m = longitud_transecto_m
  # - Muestra_*_info.ancho_muestreado_m = ancho_transecto_m
  # - Muestra_*_info.muestreo_completado = muestreo_completo
  
  rename(
    Muestra_._info.metodo_muestreo = metodo,
    Muestra_._info.nivel_agregacion_datos = nivel_agregacion_datos,
    Muestra_._info.observador = observador,
    Muestra_._info.longitud_muestreada_m = longitud_transecto_m,
    Muestra_._info.ancho_muestreado_m = ancho_transecto_m,
    Muestra_._info.muestreo_completado = muestreo_completo
  ) %>%
  
  ### Muestra_sitio/transecto_bentos_info ###
  
  mutate(
    # Muestra_sitio_bentos_info.longitud_muestreada_media_m,
    Muestra_._bentos_info.numero_puntos_muestreados = case_when(
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "PIT" ~
        as.integer(puntos_o_cm_reales_transecto),
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "CPF" ~
        as.integer(puntos_o_cm_reales_transecto),
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "CPV" ~
        as.integer(puntos_o_cm_reales_transecto),
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "Otro" ~ 
        as.integer(puntos_o_cm_reales_transecto),
      TRUE ~ NA_integer_
    ),
    Muestra_transecto_bentos_info.longitud_contorno_transecto_si_lit_m = case_when(
      stri_detect_fixed(archivo_origen, "bentos") & Muestra_._info.metodo_muestreo == "LIT" ~
        round(puntos_o_cm_reales_transecto / 100, 2),
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    -puntos_o_cm_reales_transecto
  ) %>%
  
  mutate(
    Muestra_._bentos_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_corales_info ###
  
  mutate(
    # Muestra_transecto_corales_info.criterio_seleccion_colonias
    # Muestra_transecto_corales_info.tamanio_minimo_colonia_cm
    Muestra_transecto_corales_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_peces_info ###
  
  rename(
    Muestra_transecto_peces_info.peces_muestreados = peces_muestreados
  ) %>%
  mutate(
    Muestra_transecto_peces_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_invertebrados_info ###
  
  mutate(
    Muestra_transecto_invertebrados_info.todos_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ TRUE,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_carideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_estenopodideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_aquelados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_astacideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_braquiuros_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_anomuros_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_estomatopodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_decapodos_palinuridos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.crustaceos_no_decapodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.moluscos_gastropodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.moluscos_bivalvos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.moluscos_cefalopodos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.otros_moluscos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_crinoideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_asteroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_ofiuroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_equinoideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.equinodermos_holothuroideos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.otros_equinodermos_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.otros_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ NA,
      TRUE ~ NA
    ),
    Muestra_transecto_invertebrados_info.detalles_invertebrados_muestreados = case_when(
      stri_detect_coll(archivo_origen, 'conacyt_greenpeace_2016') ~ "",
      TRUE ~ NA_character_
    ),
    Muestra_transecto_invertebrados_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_transecto_complejidad_info ###
  
  rename(
    Muestra_transecto_complejidad_info.rugosidad_longitud_contorno_m = tamanio_cadena_m
  ) %>%
  mutate(
    Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_m = Muestra_._info.longitud_muestreada_m,
    # Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_fija,
    Muestra_transecto_complejidad_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_subcuadrante_de_transecto_reclutas_info ###
  
  mutate(
    Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante = cuadrante
  ) %>%
  rename(
    Muestra_subcuadrante_de_transecto_reclutas_info.sustrato = sustrato
  ) %>%
  mutate(
    Muestra_subcuadrante_de_transecto_reclutas_info.comentarios = NA_character_
  ) %>%
  
  ### Muestra_subcuadrante_de_transecto_complejidad_info ###
  
  mutate(
    Muestra_subcuadrante_de_transecto_complejidad_info.numero_cuadrante = cuadrante
  ) %>%
  select(
    -cuadrante
  ) %>%
  rename(
    Muestra_subcuadrante_de_transecto_complejidad_info.maximo_relieve_m = relieve
  ) %>%
  mutate(
    Muestra_subcuadrante_de_transecto_complejidad_info.comentarios = NA_character_
  ) %>%
  
  ### Tabla_observaciones ###
  
  # Las tablas de observaciones corresponden a las tablas que contienen los datos
  # correspondientes a un muestreo particular de determinado aspecto. Por ejemplo,
  # "Muestra_transecto_bentos_punto" o "Muestra_transecto_corales_observacion".
  
  # Cabe destacar que al operar sobre estas tablas, se debe tener cuidado de no
  # perder los registros de muestreos realizados pero donde no se obtuvieron
  # observaciones. Esto en particular afecta a las tablas de peces, reclutas e
  # invertebrados.
    
  # Para todas las tablas de observaciones:
  # - Tabla_observaciones.codigo = codigo
  # - La columna "Tabla_observaciones.numero_observacion" corresponde a la
  #   enumeración de observaciones para cada muestreo de determinado aspecto en
  #   determinado muestreo de transecto.
  
  # Calculando la cuenta de observaciones para cada muestreo de determinado aspecto
  # en determinado muestreo de transecto.
  # Supuestos:
  # - Para cada muestreo de transecto, el orden de aparición de las observaciones
  #   en el archivo corresponde al orden de observación.
  # - Para cada muestreo de transecto, en cada "archivo_origen" sólo
  #   aparecen observaciones de la misma naturaleza (por lo que tiene sentido
  #   enlistarlas).

  # Notar que este número no tiene significado para muchos archivos de Excel (los
  # que contienen datos de complejidad, de toma de datos en cuadrantes, o datos
  # agregados). Sin embargo, para no agregar complejidad al código, se decidió
  # calcularla para todos los archivos de Excel. Simplemente hacer caso omiso de
  # esta columna para las tablas en que no tiene sentido.
  
  ddply(
    .(Muestreo.nombre,
      Muestra_sitio.nombre,
      Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
      Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
      Muestra_transecto.nombre,
      archivo_origen),
    function(df){
      
      resultado <- df %>%
        mutate(
            Tabla_observaciones.numero_observacion = 1:nrow(df)
          )
      return(resultado)
  }, .parallel = TRUE) %>%
  
  rename(
    Tabla_observaciones.codigo = codigo
    # Para los archivos de peces se tiene la columna "nombre_cientifico_abreviado"
    # que es más útil, y para invertebrados se tiene únicamente "tipo". Por ello,
    # "Tabla_observaciones.codigo" sólamente es útil para bentos, corales y reclutas
  ) %>%
  
  ### Muestra_sitio/transecto_bentos_porcentaje
  
  rename(
    Muestra_._bentos_porcentaje.porcentaje_cobertura = cobertura
    # Esta columna hay que revisarla, pues contiene números negativos y otros que
    # no suman el 100% (ver archivo auxiliar). Por lo pronto se copiará tal cual.
  ) %>%
  
  ### Muestra_transecto_bentos_punto (PIT desagregado)
  rename(
    Muestra_transecto_bentos_punto.altura_si_alga_cm = altura_algas_cm
  ) %>%

  ### Muestra_transecto_bentos_linea ###
  # Aún no se calcula porque no se han encontrado datos de LIT desagregados.
  
  ### Muestra_transecto_corales_observacion ###
  rename(
    Muestra_transecto_corales_observacion.tipo_observacion = clump,
    Muestra_transecto_corales_observacion.d1_max_diam_cm = d1_max_diam_cm,
    Muestra_transecto_corales_observacion.d2_min_diam_cm = d2_min_diam_cm,
    Muestra_transecto_corales_observacion.altura_maxima_cm = altura_maxima_cm
  ) %>%
  
  # Para todas las siguientes variables, se supone que siempre se midieron, y que
  # un NA significa que no se observó el efecto buscado.
  mutate(
    Muestra_transecto_corales_observacion.tipo_blanqueamiento = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(blanqueamiento),
      "NO",
      blanqueamiento
    ),
    Muestra_transecto_corales_observacion.porcentaje_blanqueamiento = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & Muestra_transecto_corales_observacion.tipo_blanqueamiento == "NO",
      0,
      porcentaje
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_total = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_total),
      0,
      mortalidad_total
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_reciente = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_reciente),
      0,
      mortalidad_reciente
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_transicion = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_transicion),
      0,
      mortalidad_transicion
    ),
    Muestra_transecto_corales_observacion.porcentaje_mortalidad_antigua = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(mortalidad_antigua),
      0,
      mortalidad_antigua
    ),
    Muestra_transecto_corales_observacion.enfermedad = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(enfermedades),
      "NO",
      enfermedades
    ),
    Muestra_transecto_corales_observacion.sobrecrecimiento = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(sobrecrecimiento),
      "NO",
      sobrecrecimiento
    ),
    # No podemos suponer que siempre se anotaron sobrecrecimientos adicionales
    # (aún bajo el supuesto de que en todas las colonias se revisaron sobrecrecimientos)
    # Por ello, un NA en sobrecrecimiento adicional puede ser que existe y no
    # se anotó o que simplemente no existe. Es decir, hay que dejar los NA
    # como "no se".
    Muestra_transecto_corales_observacion.sobrecrecimiento_adicional = sobrecrecimiento_1,
    
    Muestra_transecto_corales_observacion.depredacion = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(depredacion),
      "NO",
      depredacion
    ),
    Muestra_transecto_corales_observacion.lesion = ifelse(
      stri_detect_fixed(archivo_origen, "corales") & is.na(lesiones),
      "NO",
      lesiones
    ),
    Muestra_transecto_corales_observacion.comentarios = NA_character_
  ) %>%
  select(
    -blanqueamiento,
    -porcentaje,
    -mortalidad_total,
    -mortalidad_reciente,
    -mortalidad_transicion,
    -mortalidad_antigua,
    -enfermedades,
    -sobrecrecimiento,
    -sobrecrecimiento_1,
    -depredacion,
    -lesiones
  ) %>%

  ### Muestra_transecto_peces_cuenta ###
  # Se tiene cuidado de no perder los muestreos realizados y que no tuvieron
  # observaciones
  
  # Haciendo el "gather" de las columnas de "peces_tamanio_._.", para los archivos de
  # peces. Se tratarán por separado peces desagregados de cuentas agregadas por
  # especie (y transecto), porque se quiere revisar al final que no se hayan
  # duplicado por error, en los archivos de Excel, registros de cuentas agregadas.
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)

    if(stri_detect_fixed(archivo_origen, "peces")){
      # Generando columnas de "minimo_tamanio_cm", "maximo_tamanio_cm" y "cuenta"
      resultado_sin_agregar_especies_duplicadas_mismo_muestreo <- df %>%
        gather(key = peces_tamanio_min_max, value = cuenta, dplyr::contains("peces_tamanio")) %>%
        # Filtrando tamaños no encontrados en un mismo conteo de especie en transecto.
        # Notar que si "nombre_cientifico_abreviado" es NA, se preserva el registro
        # pues puede corresponder a transectos sin observaciones. Estos se tendrán
        # que filtrar a la hora de crear la tabla: "Muestra_transecto_peces_cuenta".
        filter(!is.na(cuenta) | is.na(nombre_cientifico_abreviado)) %>%
        # Generando campos de "tamanio_minimo_cm" y "tamanio_maximo_cm"
        separate(col = peces_tamanio_min_max, into = c("etiqueta_1", "etiqueta_2",
          "peces_tamanio_minimo_cm", "peces_tamanio_maximo_cm")) %>%
        select(
          -etiqueta_1,
          -etiqueta_2
        ) %>%
        mutate(
          # Quitándoles la etiqueta de "cm" a "peces_tamanio_minimo_cm" y "peces_tamanio_maximo_cm"
          peces_tamanio_minimo_cm = stri_sub(peces_tamanio_minimo_cm, from = 1,
            to = (stri_length(peces_tamanio_minimo_cm)-2)) %>%
            as.numeric(),
          peces_tamanio_maximo_cm = stri_sub(peces_tamanio_maximo_cm, from = 1,
            to = (stri_length(peces_tamanio_maximo_cm)-2)) %>%
            as.numeric(),
          es_juvenil = NA # lógico
        ) %>%
        rename(
          Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado = nombre_cientifico_abreviado,
          Muestra_transecto_peces_cuenta.tamanio_minimo_cm = peces_tamanio_maximo_cm,
          Muestra_transecto_peces_cuenta.tamanio_maximo_cm = peces_tamanio_maximo_cm,
          Muestra_transecto_peces_cuenta.cuenta = cuenta,
          Muestra_transecto_peces_cuenta.es_juvenil = es_juvenil
        )
      
      # Si los datos de peces provienen de un archivo en el que se supone que
      # están desagregados, agregarlos; si no, no hacerlo porque se podrían ocultar
      # errores (esta distinción es la principal función de este if-else)
      if(stri_detect_fixed(archivo_origen, "peces_agregados")){
        resultado <- resultado_sin_agregar_especies_duplicadas_mismo_muestreo
      } else{
        resultado <- resultado_sin_agregar_especies_duplicadas_mismo_muestreo %>%
          group_by(
            Muestreo.nombre,
            Muestra_sitio.nombre,
            Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
            Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
            Muestra_transecto.nombre,
            # en el ddply ya se separó por "archivo_origen"
            Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado,
            Muestra_transecto_peces_cuenta.tamanio_minimo_cm,
            Muestra_transecto_peces_cuenta.tamanio_maximo_cm,
            Muestra_transecto_peces_cuenta.es_juvenil
          ) %>%
          mutate(
            Muestra_transecto_peces_cuenta.cuenta = sum(!is.na(Muestra_transecto_peces_cuenta.cuenta))
            # Notar que como los registros son observaciones (desagregadas) por
            # especie en un muestreo, cada registro suma máximo una cuenta a su
            # categoría de tamaño correspondiente
          ) %>%
          ungroup() %>%
          # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
          # hacer el mutate(). No se puede usar summarise() porque se pierden las
          # otras columnas
          distinct(
            Muestreo.nombre,
            Muestra_sitio.nombre,
            Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
            Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
            Muestra_transecto.nombre,
            # en el ddply ya se separó por "achivo_origen"
            Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado,
            Muestra_transecto_peces_cuenta.tamanio_minimo_cm,
            Muestra_transecto_peces_cuenta.tamanio_maximo_cm,
            Muestra_transecto_peces_cuenta.es_juvenil,
            .keep_all = TRUE
          )
      }
    } else{
      # Si "archivo_origen" no es de peces, se eliminan las columnas de
      # "peces_tamanio" y se agregan columnas dummy para que se pueda hacer la
      # fusión al final del ddply().
      resultado <- df %>%
        select(
          -dplyr::contains("peces_tamanio_")
        ) %>%
        rename(
          Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado = nombre_cientifico_abreviado
        ) %>%
        mutate(
          Muestra_transecto_peces_cuenta.tamanio_minimo_cm = NA_integer_,
          Muestra_transecto_peces_cuenta.tamanio_maximo_cm = NA_integer_,
          Muestra_transecto_peces_cuenta.cuenta = NA_integer_,
          Muestra_transecto_peces_cuenta.es_juvenil = NA # lógico
        )
    }
    return(resultado)
  }, .parallel = TRUE) %>%

  ### Muestra_transecto_invertebrados_cuenta ###
  # Se tiene cuidado de no perder los muestreos realizados y que no tuvieron
  # observaciones
  
  rename(
    Muestra_transecto_invertebrados_cuenta.tipo = tipo,
    Muestra_transecto_invertebrados_cuenta.cuenta = conteo
  ) %>%
  
  # Si los datos provienen de un archivo donde se supone que están desagregados,
  # se agregan; en otro caso, no se hace porque se podrían ocultar errores por
  # repetición de registros.
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)
    
    if(stri_detect_fixed(archivo_origen, "invertebrados_desagregados")){
      resultado <- df %>%
        group_by(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "archivo_origen"
          Muestra_transecto_invertebrados_cuenta.tipo
        ) %>%
        mutate(
          Muestra_transecto_invertebrados_cuenta.cuenta = sum(!is.na(Muestra_transecto_invertebrados_cuenta.cuenta))
        ) %>%
        ungroup() %>%
        # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
        # hacer el mutate(). No se puede usar summarise() porque se pierden las
        # otras columnas
        distinct(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_transecto_invertebrados_cuenta.tipo,
          .keep_all = TRUE
        )
    } else{
      resultado <- df
    }
    return(resultado)
  }) %>%
  
  ### Muestra_subcuadrante_de_transecto_reclutas_cuenta ###
  
  rename(
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio = categoria_tamanio,
    
    # Si los siguientes valores son iguales, entonces se tiene un recluta que
    # fue medido exactamente.
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm = tamanio_minimo_cm,
    Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm = tamanio_maximo_cm
  ) %>%
  
  # Para calcular "cuenta", depende. En los archivos de "reclutas agregados" es
  # simplemente "n". En los archivos de reclutas desagregados, éstos se agregan
  # por variables de muestreo (hasta nivel de cuadrante), código, categoría de
  # tamaño y tamaños mínimo y máximo:
  
  ddply(.(archivo_origen), function(df){
    archivo_origen <- unique(df$archivo_origen)
    
    if(stri_detect_fixed(archivo_origen, "reclutas_desagregados")){
      resultado <- df %>%
        group_by(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante,
          Tabla_observaciones.codigo,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm
        ) %>%
        mutate(
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.cuenta = sum(n, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Eliminando duplicados, porque ya se tomaron en cuenta a la hora de
        # hacer el mutate(). No se puede usar summarise() porque se pierden las
        # otras columnas.
        distint(
          Muestreo.nombre,
          Muestra_sitio.nombre,
          Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
          Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace,
          Muestra_transecto.nombre,
          # en el ddply ya se separó por "achivo_origen"
          Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante,
          Tabla_observaciones.codigo,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm,
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm,
          .keep_all = TRUE
        ) %>%
        select(-n)
    } else{
      resultado <- df %>%
        rename(
          Muestra_subcuadrante_de_transecto_reclutas_cuenta.cuenta = n
        )
    }
    return(resultado)
  }, .parallel = TRUE)
  
saveRDS(datos_globales,
  paste0(ruta_salidas_3_crear_df_homologado, "/datos_globales_preliminar.RDS"))

### Me quedé a punto de probar el código de la tabla "datos globales". Después
### de esto revisaré el código con calma, generaré las tablas, crearé varios
### resúmenes para revisarlas y las integraré en la base.

  ###############################################################
  # Transformaciones finales por "archivo_origen"
  ###############################################################
  
  ### Muestra_transecto_corales_observacion ###
  
  # Archivos de Excel relacionados:
  # - "conacyt_greenpeace_2016_corales_desagregados"
  # - "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
  
  # - Se revisará que todos los transectos estén declarados, independientemente
  # de si tuvieron o no observaciones.
  
  ### Muestra_transecto_peces_cuenta ###
  
  # Archivos de Excel relacionados:
  # - "conacyt_greenpeace_2016_peces_agregados_especie_talla"
  # - "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados"
  # - "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla"
  # - "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  
  # - Se cambiará el campo "especie" por "nombre_cientifico". Revisar que no haya
  #   registros duplicados al hacer el join con el catálogo, pues "especie" no es
  #   una llave natural de "catalogos_muestra_transecto_peces_cuenta__nombre_cientifico".
  # - Se agregarán registros con los mismos valores en: "Muestreo.nombre", "Muestra_sitio.nombre",
  #   "Muestra_sitio.aux_remuestreo_en_mismo_muestreo", "Muestra_transecto.nombre", "nombre_cientifico",
  #   por consistencia con la tabla de interés.
  # - Se revisará que todos los transectos estén incluídos, independiente si
  #   tienen o no observaciones.
  # - Se transformarán los datos para darles la estructura apropiada.
  
  ### Revisión de Esme con respecto a los puntos anteriores:
revision_numero_transectos_muestreo_sitio <- datos_globales %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_bentos_desagregados",
    "conacyt_greenpeace_2016_corales_desagregados",
    "conacyt_greenpeace_2016_invertebrados_desagregados",
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales",
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
    
    # Los transectos
  )) %>%
  select(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
    Muestra_transecto.nombre
  ) %>%
  distinct() %>%
  group_by(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo) %>%
  tally()

revision_numero_cuadrantes_muestreo_transecto <- datos_globales %>%
  filter(archivo_origen %in% c(
    "conacyt_greenpeace_2016_bentos_desagregados",
    "conacyt_greenpeace_2016_corales_desagregados",
    "conacyt_greenpeace_2016_invertebrados_desagregados",
    "conacyt_greenpeace_2016_peces_agregados_especie_talla",
    "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
    "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados",
    "historicos_y_2017_transecto_corales_desagregados_colonias_individuales",
    "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
    "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
    "historicos_y_2017_transecto_peces_desagregados_especie_talla"
  )) %>%
  select(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo,
    Muestra_transecto.nombre
  ) %>%
  distinct() %>%
  group_by(
    archivo_origen,
    Muestreo.nombre,
    Muestra_sitio.nombre,
    Muestra_sitio.aux_remuestreo_en_mismo_muestreo) %>%
  tally()


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# Esperando a Esme:
# 1. Ver si puedo cambiar el nombre de ""historicos_y_2017_transecto_invertebrados_agregados_conteos_especie""
# a "historicos_y_2017_cuadrante_invertebrados_agregados_conteos_especie"


# Para porcentaje, si blanqueamiento es NO o NA, vale NA. en otro caso, puede
# valer el porcentaje de blanqueamiento del tipo seleccionado o NA si no se
# registró el porcentaje.



  mutate(
    # Redondeando apropiadamente columnas numéricas que lo necesitan
    profundidad_inicial_m = round(profundidad_inicial_m, 1),
    profundidad_final_m = round(profundidad_final_m, 1),
    profundidad_media_m = round(profundidad_media_m, 2),
    profundidad_media_m_sitio = round(profundidad_media_m_sitio, 2),
    temperatura_c = round(temperatura_c, 2),
    
    # Creando columnas auxiliares útiles a la hora de generar las tablas
    strings_vacios = "",
    verdadero = TRUE,
    falso = FALSE,
    na_numerico = NA_real_,
    datum = "WGS84"
  ) # %>%

# mutate(
# Invertebrates_transect_sample_info
# "numero" siempre es 1 para conacyt_greenpeace_2016_invertebrados_desagregados_v3"

# Recruit_quadrant_sample_transect_count
# En realidad no son cuentas, sino incidencias

# # arreglando mortalidades si suman más de 100. Ésto se corregirá en v3.
# suma_mortalidades = mortalidad_antigua +
#   mortalidad_reciente +
#   mortalidad_transicion +
#   mortalidad_total
# 
# # Notar que si alguna mortalidad es NA, entonces la suma es NA y todas se
# # hacen NA (lo cuál no es un problema porque acabamos de castear mortalidades
# # NA a 0 para "CORALES_DESAGREGADOS_V2")
# mortalidad_antigua = ifelse(suma_mortalidades > 100, #!!!
#   (mortalidad_antigua / suma_mortalidades) * 100,
#   mortalidad_antigua),
# mortalidad_reciente = ifelse(suma_mortalidades > 100, #!!!
#   (mortalidad_reciente / suma_mortalidades) * 100 ,
#   mortalidad_reciente),
# mortalidad_transicion = ifelse(suma_mortalidades > 100, #!!!
#   (mortalidad_transicion / suma_mortalidades) * 100 ,
#   mortalidad_transicion),
# mortalidad_total = ifelse(suma_mortalidades > 100, #!!!
#   (mortalidad_total / suma_mortalidades) * 100 ,
#   mortalidad_total)

# )






