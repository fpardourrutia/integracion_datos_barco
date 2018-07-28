# En este script se usa "datos_globales" para generar las diversas tablas que se
# insertarán en la base de datos.

# La idea es que en "datos_globales" ya estén los datos lo más procesados posible,
# para simplemente "cortar" las tablas usando este script.

# Pasos a seguir:
# 1. Se lee el data frame "datos_globales".
# 2. Se generan columnas auxiliares que indican qué registros tomar en cuenta
# para generar qué tabla del esquema v5.
# 3. Se generan las columnas que corresponderán a llaves numéricas para cada
# tabla nueva, usando únicamente los registros que se deben tomar en cuenta para
# cada una. Recordar que, dada una tabla del esquema de datos v5, dos registros
# de "datos_globales" tendrán el mismo valor en su llave numérica correspondiente
# si y solo sí tienen los mismos valores en ciertas columnas especificadas como
# una llave natural para la misma.
# 4. Se "recortan" las tablas correspondientes al esquema v5 a partir del data
# frame que contiene las llaves numéricas para cada una de ellas. Este recorte
# se produce seleccionando las columnas que contendrá la nueva tabla, y para
# crear un registro correspondiente a determinada llave numérica, se utilizará
# una regla que permite seleccionar los valores de cada columna (por ejemplo, la
# moda).

# Supuestos de este proceso de generación de tablas
# 1. Muestreo:
#   - Todos los muestreos distintos correspondientes al mismo proyecto tienen
#     distinto nombre.
# 2. Muestra_sitio:
#   - Dos muestras de sitio corresponden espacialmente si y sólo si tienen el
#     mismo nombre
#   - Como la fecha es propensa a muchos errores (por ejemplo, causados por
#     diferencias entre el formato "general" y "fecha" en Excel), la tabla de
#     "datos_globales" contiene los campos "Muestra_sitio.aux_remuestreo_en_mismo_muestreo"
#     y "Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace" para
#   diferenciar remuestreos en los mismos sitios.
# 3. Muestra_transecto:
#   - Toda muestra de transecto está declarada en al menos algún archivo de Excel.
#   - Dado un muestreo de sitio, dos aspectos fueron muestreados sobre el mismo
#     transecto si y sólo si tienen el mismo valor en "Muestra_transecto.nombre".
# 4. Muestra_sitio_bentos_info:
#   - Existe máximo un muestreo de bentos por muestra de sitio.
# 5. Muestra_sitio_bentos_porcentaje:
#   - Los porcentajes de cobertura son agregados por código antes de ser
#    insertados a la base de datos, por lo que no importa el orden en estos últimos.
# 6. Muestra_transecto_bentos_info:
#   - Existe máximo un muestreo de bentos por muestra de transecto
# 7. Muestra_transecto_bentos_punto:
#   - Los registros están anotados en los archivos de Excel en el orden en que
#     se encontraron (el orden sí importa).
# 8. Muestra_transecto_bentos_porcentaje:
#   - Los porcentajes de cobertura son agregados por código antes de ser
#     insertados a la base de datos, por lo que no importa el orden en estos últimos.
# 9. Muestra_transecto_corales_info:
#   - Existe máximo un muestreo de corales por muestra de transecto
# 10. Muestra_transecto_corales_observacion:
#   - Los registros están anotados en los archivos de Excel en el orden en que
#     se encontraron (el orden sí importa).
#   - Al construir "datos_globales", se presupuso que varios campos fueron medidos
#     y un NA significó que no se encontraron rastros de la variable medida (vs
#     que esta variable no se midió). Ver "6_creacion_df_homologado.R" para más
#     detalles.
# 11. Muestra_transecto_peces_info:
#   - Existe máximo un muestreo de peces por muestra de transecto
#   - Los muestreos de peces que no tuvieron observaciones fueron declarados en
#     sus archivos de Excel correspondientes con código NA.
# 12. Muestra_transecto_peces_cuenta:
#   - Las cuentas de peces son agregadas antes de ser introducidas a la base de
#     datos, por lo que  no importa el orden en que se encuentren estos registros.
#   - Los nombres científicos abreviados son una llave natural del catálogo
#     "catalogos_registro_peces__nombre_cientifico_abreviado".
# 13. Muestra_transecto_invertebrados_info:
#   - Existe máximo un muestreo de invertebrados por muestra de transecto.
#   - Los muestreos de invertebrados que no tuvieron observaciones fueron
#     declarados en sus archivos de Excel correspondientes con tipo NA.
# 14. Muestra_transecto_invertebrados_cuenta:
#   - Las cuentas de invertebradas son agregadas antes de ser introducidas a la
#     base de datos, por lo que  no importa el orden en que se encuentren estos
#     registros.
# 15. Muestra_subcuadrante_de_transecto_reclutas_info:
#   - Existe máximo un muestreo de reclutas por muestreo de cuadrante. Si no
#     es así, a un sólo muestreo de reclutas en cuadrante se le asociarían todas
#     las observaciones de todos los muestreos redundantes en el mismo, inflando
#     por mucho el número de observaciones.
#   - Los muestreos de cuadrantes que no tuvieron observaciones fueron declarados
#     en sus archivos de Excel correspondientes con tipo NA.
# 16. Muestra_subcuadrante_de_transecto_reclutas_cuenta:
#   - No importa el orden de los registros pues los cuadrantes no permiten tenerlo.
# 17. Muestra_transecto_complejidad_info:
#   - Existe máximo un muestreo de complejidad por muestra de transecto.
# 18. Muestra_subcuadrante_de_transecto_complejidad_info
#   - Existe máximo un muestreo de complejidad por muestreo de cuadrante. Si no
#     es así, se asignarán las modas de los valores correspondientes al mismo
#     cuadrante como sus valores en cada una de las variables.

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo "datos_globales"
################################################################################

datos_globales <- readRDS(paste0(rutas_salida[6], "/datos_globales.RDS"))
glimpse(datos_globales)

datos_globales %>%
  pull(Auxiliar.archivo_origen) %>%
  unique()

################################################################################
# 2. Generando columnas que especifican qué registros tomar en cuenta para qué
# tablas
################################################################################

# Por ejemplo, todos los registros se toman en cuenta para formar "Muestreo" y
# "Muestra_sitio", pero sólo los de archivos de Excel de corales se toman en
# cuenta para formar "Muestra_transecto_corales_info" y
# "Muestra_transecto_corales_observacion"

datos_globales_columnas_indicadoras <- datos_globales %>%
  
  mutate(
    
    ### Generales ###
    
    Auxiliar.integrar_en_Muestreo = TRUE,
    
    Auxiliar.integrar_en_Muestra_sitio = TRUE,
    
    Auxiliar.integrar_en_Muestra_transecto = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      FALSE, TRUE),
    
    ### Bentos ###
    
    Auxiliar.integrar_en_Muestra_sitio_bentos_info = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_sitio_bentos_porcentaje = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_sitio_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_bentos_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_bentos_desagregados",
        "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_bentos_porcentaje = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_transecto_bentos_agregados_porcentajes_tipo_cobertura",
      TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_bentos_punto = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_bentos_desagregados",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit",
        "historicos_y_2017_transecto_bentos_desagregados_pit_lit_privados"
      ), TRUE, FALSE),
    
    # Auxiliar.integrar_en_Muestra_transecto_bentos_linea
    
    ### Corales ###
    
    Auxiliar.integrar_en_Muestra_transecto_corales_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_corales_desagregados",
        "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_corales_observacion = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_corales_desagregados",
        "historicos_y_2017_transecto_corales_desagregados_colonias_individuales"
      ), TRUE, FALSE),
    
    ### Peces ###
    
    Auxiliar.integrar_en_Muestra_transecto_peces_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_peces_agregados_especie_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
        "historicos_y_2017_transecto_peces_desagregados_especie_talla"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_peces_cuenta = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_peces_agregados_especie_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_transecto_peces_agregados_conteos_especie_categoria_talla_privados",
        "historicos_y_2017_transecto_peces_desagregados_especie_talla"
      ), TRUE, FALSE),
    
    ### Invertebrados ###
    
    Auxiliar.integrar_en_Muestra_transecto_invertebrados_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_invertebrados_desagregados",
        "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_transecto_invertebrados_cuenta = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_invertebrados_desagregados",
        "historicos_y_2017_transecto_invertebrados_agregados_conteos_especie"
      ), TRUE, FALSE),
    
    ### Reclutas ###
    
    Auxiliar.integrar_en_Muestra_subcuadrante_de_transecto_reclutas_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_reclutas_desagregados",
        "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_cuadrante_reclutas_desagregados"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_subcuadrante_de_transecto_reclutas_cuenta = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_reclutas_desagregados",
        "historicos_y_2017_cuadrante_reclutas_agregados_conteos_especie_categoria_talla",
        "historicos_y_2017_cuadrante_reclutas_desagregados"
      ), TRUE, FALSE),
    
    ### Complejidad ###
    
    Auxiliar.integrar_en_Muestra_transecto_complejidad_info = ifelse(
      Auxiliar.archivo_origen %in% c(
        "conacyt_greenpeace_2016_complejidad",
        "historicos_y_2017_transecto_complejidad_desagregada_cadena"
      ), TRUE, FALSE),
    
    Auxiliar.integrar_en_Muestra_subcuadrante_de_transecto_complejidad_info = ifelse(
      Auxiliar.archivo_origen == "historicos_y_2017_transecto_complejidad_desagregada_maximo_relieve",
      TRUE, FALSE)
  )

################################################################################
# 3. Generando llaves numericas
################################################################################

# Prefijo para localizar las columnas indicadoras de registros correspondientes
# a cada tabla nueva
prefijo_columnas_indicadoras <- "Auxiliar.integrar_en_"

# Sufijo para nombrar las columnas que contendrán las llaves numéricas
sufijo_columnas_llaves_numericas <- ".id"

relacion_tablas_columnas_llaves <- list(
  
  ### Tablas generales
  
  "Muestreo" = c(
    "Muestreo.nombre"
  ),
  
  # No se utiliza "Muestra_sitio.fecha" porque puede dar lugar a muchos errores
  "Muestra_sitio" = c(
    "Muestreo.id", # Recordar que las llaves numéricas se generan iterativamente
    "Muestra_sitio.nombre",
    "Muestra_sitio.aux_identificador_muestreo_sitio_conacyt_greenpeace",
    "Muestra_sitio.aux_remuestreo_en_mismo_muestreo"
  ),
  
  # Para las siguientes tablas recordar que antes de generar la llave numérica,
  # los registros que no deban ser tomados en cuenta serán filtrados automática-
  # mente de acuerdo a la columna indicadora de la tabla en cuestión.
  
  "Muestra_transecto" = c(
    "Muestra_sitio.id",
    "Muestra_transecto.nombre"
  ),
  
  ### Bentos ###
  
  "Muestra_sitio_bentos_info" = c(
    "Muestra_sitio.id"
  ),
  
  "Muestra_sitio_bentos_porcentaje" = c(
  ),
  
  "Muestra_transecto_bentos_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_transecto_bentos_porcentaje" = c(
  ),
  
  "Muestra_transecto_bentos_punto" = c(
  ),
  
  # "Muestra_transecto_bentos_linea" = c(),
  
  ### Corales ###
  
  "Muestra_transecto_corales_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_transecto_corales_observacion" = c(
  ),
  
  ### Peces ###
  
  "Muestra_transecto_peces_info" = c(
    "Muestra_transecto.id"
  ),
  
  # Recordar que al crear el data frame homologado ya se le dio forma a esta
  # tabla
  "Muestra_transecto_peces_cuenta" = c(
  ),
  
  ### Invertebrados ###
  
  "Muestra_transecto_invertebrados_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_transecto_invertebrados_cuenta" = c(
  ),
  
  ### Reclutas ###
  
  "Muestra_subcuadrante_de_transecto_reclutas_info" = c(
    "Muestra_transecto.id",
    "Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante"
  ),
  
  "Muestra_subcuadrante_de_transecto_reclutas_cuenta" = c(
  ),
  
  ### Complejidad ###
  
  "Muestra_transecto_complejidad_info" = c(
    "Muestra_transecto.id"
  ),
  
  "Muestra_subcuadrante_de_transecto_complejidad_info" = c(
    "Muestra_transecto.id",
    "Muestra_subcuadrante_de_transecto_complejidad_info.numero_cuadrante"
  )
)

datos_globales_llaves_numericas <- genera_llaves_varias_tablas(
  datos_globales_columnas_indicadoras,
  prefijo_columnas_indicadoras,
  sufijo_columnas_llaves_numericas,
  relacion_tablas_columnas_llaves
)

saveRDS(datos_globales_llaves_numericas,
  paste0(rutas_salida[7], "/datos_globales_llaves_numericas.RDS"))

################################################################################
# 4. Generando tablas a insertar en la base de datos final
################################################################################

# Sufijo para localizar las llaves numéricas generadas con la función
# "genera_llaves_varias_tablas()"
sufijo_columnas_llaves_numericas <- ".id"

# Nombre que tendrán las columnas numéricas en las nuevas tabla:
nombre_nuevo_columnas_llaves_numericas <- "id"

relacion_tablas_columnas_funciones <- list(
  
  ### Tablas generales ###
  
  "Muestreo#2" = c(
    "nombre" = "Muestreo.nombre",
    "descripcion" = "Muestreo.descripcion",
    "proposito" = "Muestreo.proposito",
    "area_estudio" = "Muestreo.area_estudio",
    "organizacion" = "Muestreo.organizacion",
    "suborganizacion" = "Muestreo.suborganizacion",
    "encargado" = "Muestreo.encargado",
    "contacto" = "Muestreo.contacto",
    "referencia" = "Muestreo.referencia",
    "nombre_proyecto" = "Muestreo.nombre_proyecto",
    "anio_inicio_proyecto" = "Muestreo.anio_inicio_proyecto",
    "anio_termino_proyecto" = "Muestreo.anio_termino_proyecto",
    "comentarios" = "Muestreo.comentarios"
    # "etapa_revision" = "Muestreo.etapa_revision",
    # "compatibilidad_cliente" = "Muestreo.compatibilidad_cliente",
  ),
  
  "Muestra_sitio#2" = c(
    "muestreo_id" = "Muestreo.id",
    "nombre" = "Muestra_sitio.nombre", 
    "nombre_original" = "Muestra_sitio.nombre_original",
    "fecha" = "Muestra_sitio.fecha",
    "hora" = "Muestra_sitio.hora",
    "pais" = "Muestra_sitio.pais",
    "region_healthy_reefs" = "Muestra_sitio.region_healthy_reefs",
    "localidad" = "Muestra_sitio.localidad",
    "tipo_arrecife" = "Muestra_sitio.tipo_arrecife",
    "subtipo_arrecife" = "Muestra_sitio.subtipo_arrecife",
    "zona_arrecifal" = "Muestra_sitio.zona_arrecifal",
    "dentro_area_natural_protegida" = "Muestra_sitio.dentro_area_natural_protegida",
    "nombre_area_natural_protegida" = "Muestra_sitio.nombre_area_natural_protegida",
    "dentro_area_no_pesca" = "Muestra_sitio.dentro_area_no_pesca",
    "latitud" = "Muestra_sitio.latitud",
    "longitud" = "Muestra_sitio.longitud",
    "datum" = "Muestra_sitio.datum",
    "metodo_seleccion" = "Muestra_sitio.metodo_seleccion",
    "metodologia" = "Muestra_sitio.metodologia",
    "temperatura_c" = "Muestra_sitio.temperatura_c",
    "profundidad_m" = "Muestra_sitio.profundidad_m",
    "comentarios" = "Muestra_sitio.comentarios",
    "excluir" = "Muestra_sitio.excluir",
    "datos_abiertos" = "Muestra_sitio.datos_abiertos",
    # "etapa_revision" = "Muestra_sitio.etapa_revision",
    # "compatibilidad_cliente" = "Muestra_sitio.compatibilidad_cliente
    "fuente" = "Muestra_sitio.fuente"
  ),
  
  "Muestra_transecto#2" = c(
    "muestra_sitio_id" = "Muestra_sitio.id",
    "nombre" = "Muestra_transecto.nombre",
    "transecto_fijo" = "Muestra_transecto.transecto_fijo",
    "profundidad_inicial_m" = "Muestra_transecto.profundidad_inicial_m",
    "profundidad_final_m" = "Muestra_transecto.profundidad_final_m",
    "subcuadrantes_planeados" = "Muestra_transecto.subcuadrantes_planeados",
    #"seleccion_aleatoria_centros_subcuadrantes" = "Muestra_transecto.seleccion_aleatoria_centros_subcuadrantes",
    #"distancia_centros_subcuadrantes_m" = "Muestra_transecto.distancia_centros_subcuadrantes_m",
    "longitud_subcuadrante_m" = "Muestra_transecto.longitud_subcuadrante_m",
    "ancho_subcuadrante_m" = "Muestra_transecto.ancho_subcuadrante_m",
    "comentarios" = "Muestra_transecto.comentarios"
  ),
  
  ### Bentos ###
  
  "Muestra_sitio_bentos_info#2" = c(
    "muestra_sitio_id" = "Muestra_sitio.id",
    "metodo_muestreo" = "Muestra_._info.metodo_muestreo",
    "nivel_agregacion_datos" = "Muestra_._info.nivel_agregacion_datos",
    "observador" = "Muestra_._info.observador",
    #"longitud_muestrada_media_m",
    "numero_puntos_muestreados_si_aplica" = "Muestra_._bentos_info.numero_puntos_muestreados_si_aplica",
    "muestreo_completado" = "Muestra_._info.muestreo_completado"
  ),
  
  "Muestra_sitio_bentos_porcentaje#1" = c(
    "muestra_sitio_bentos_info_id" = "Muestra_sitio_bentos_info.id",
    "codigo" = "Registro_observacion.codigo",
    "porcentaje_cobertura" = "Muestra_._bentos_porcentaje.porcentaje_cobertura"
  ),
  
  "Muestra_transecto_bentos_info#2" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "metodo_muestreo" = "Muestra_._info.metodo_muestreo",
    "nivel_agregacion_datos" = "Muestra_._info.nivel_agregacion_datos",
    "observador" = "Muestra_._info.observador",
    "longitud_muestreada_m" = "Muestra_._info.longitud_muestreada_m",
    "numero_puntos_muestreados_si_aplica" = "Muestra_._bentos_info.numero_puntos_muestreados_si_aplica",
    "longitud_contorno_transecto_si_lit_m" = "Muestra_transecto_bentos_info.longitud_contorno_transecto_si_lit_m",
    "muestreo_completado" = "Muestra_._info.muestreo_completado",
    "comentarios" = "Muestra_._bentos_info.comentarios"
  ),
  
  "Muestra_transecto_bentos_porcentaje#1" = c(
    "muestra_transecto_bentos_info_id" = "Muestra_transecto_bentos_info.id",
    "codigo" = "Registro_observacion.codigo",
    "porcentaje_cobertura" = "Muestra_._bentos_porcentaje.porcentaje_cobertura"
  ),
  
  "Muestra_transecto_bentos_punto#1" = c(
    "muestra_transecto_bentos_info_id" = "Muestra_transecto_bentos_info.id",
    "numero_punto" = "Registro_observacion.numero_observacion",
    "codigo" = "Registro_observacion.codigo",
    "altura_si_alga_cm" = "Muestra_transecto_bentos_punto.altura_si_alga_cm"
  ),
  
  # "Muestra_transecto_bentos_linea" = c(
  #   "muestra_transecto_bentos_info_id" = "Muestra_transecto_bentos_info.id",
  #   "numero_observacion" = "Registro_observacion.numero_observacion",
  #   "codigo" = "Registro_observacion.codigo",
  #   "longitud_cm"
  # ),
  
  ### Corales ###
  
  "Muestra_transecto_corales_info#2" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "metodo_muestreo" = "Muestra_._info.metodo_muestreo",
    "nivel_agregacion_datos" = "Muestra_._info.nivel_agregacion_datos",
    "observador" = "Muestra_._info.observador",
    "longitud_muestreada_m" = "Muestra_._info.longitud_muestreada_m",
    "ancho_muestreado_m" = "Muestra_._info.ancho_muestreado_m",
    # "criterio_seleccion_colonias",
    # "tamanio_minimo_colonia_cm",
    "muestreo_completado" = "Muestra_._info.muestreo_completado",
    "comentarios" = "Muestra_transecto_corales_info.comentarios"
  ),
  
  "Muestra_transecto_corales_observacion#1" = c(
    "muestra_transecto_corales_info_id" = "Muestra_transecto_corales_info.id",
    "numero_observacion" = "Registro_observacion.numero_observacion",
    "codigo" = "Registro_observacion.codigo",
    "tipo_observacion" = "Muestra_transecto_corales_observacion.tipo_observacion",
    "d1_diametro_maximo_cm" = "Muestra_transecto_corales_observacion.d1_diametro_maximo_cm",
    "d2_diametro_minimo_cm" = "Muestra_transecto_corales_observacion.d2_diametro_minimo_cm",
    "altura_maxima_cm" = "Muestra_transecto_corales_observacion.altura_maxima_cm",
    "tipo_blanqueamiento" = "Muestra_transecto_corales_observacion.tipo_blanqueamiento",
    "porcentaje_blanqueamiento" = "Muestra_transecto_corales_observacion.porcentaje_blanqueamiento",
    "porcentaje_mortalidad_reciente" = "Muestra_transecto_corales_observacion.porcentaje_mortalidad_reciente",
    "porcentaje_mortalidad_transicion" = "Muestra_transecto_corales_observacion.porcentaje_mortalidad_transicion",
    "porcentaje_mortalidad_antigua" = "Muestra_transecto_corales_observacion.porcentaje_mortalidad_antigua",
    "porcentaje_mortalidad_total" = "Muestra_transecto_corales_observacion.porcentaje_mortalidad_total",
    "enfermedad" = "Muestra_transecto_corales_observacion.enfermedad",
    "sobrecrecimiento" = "Muestra_transecto_corales_observacion.sobrecrecimiento",
    "sobrecrecimiento_adicional" = "Muestra_transecto_corales_observacion.sobrecrecimiento_adicional",
    "depredacion" = "Muestra_transecto_corales_observacion.depredacion",
    "lesion" = "Muestra_transecto_corales_observacion.lesion",
    "comentarios" = "Muestra_transecto_corales_observacion.comentarios"
  ),
  
  ### Peces ###
  
  "Muestra_transecto_peces_info#2" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "metodo_muestreo" = "Muestra_._info.metodo_muestreo",
    "nivel_agregacion_datos" = "Muestra_._info.nivel_agregacion_datos",
    "observador" = "Muestra_._info.observador",
    "longitud_muestreada_m" = "Muestra_._info.longitud_muestreada_m",
    "ancho_muestreado_m" = "Muestra_._info.ancho_muestreado_m",
    "peces_muestreados" = "Muestra_transecto_peces_info.peces_muestreados",
    "muestreo_completado" = "Muestra_._info.muestreo_completado",
    "comentarios" = "Muestra_transecto_peces_info.comentarios"
  ),
  
  "Muestra_transecto_peces_cuenta#1" = c(
    "muestra_transecto_peces_info_id" = "Muestra_transecto_peces_info.id",
    "nombre_cientifico_abreviado" = "Muestra_transecto_peces_cuenta.nombre_cientifico_abreviado",
    "es_juvenil" = "Muestra_transecto_peces_cuenta.es_juvenil",
    "tamanio_minimo_cm" = "Muestra_transecto_peces_cuenta.tamanio_minimo_cm",
    "tamanio_maximo_cm" = "Muestra_transecto_peces_cuenta.tamanio_maximo_cm",
    "cuenta" = "Muestra_transecto_peces_cuenta.cuenta"
  ),
  
  ### Invertebrados ###
  
  "Muestra_transecto_invertebrados_info#2" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "metodo_muestreo" = "Muestra_._info.metodo_muestreo",
    "nivel_agregacion_datos" = "Muestra_._info.nivel_agregacion_datos",
    "observador" = "Muestra_._info.observador",
    "longitud_muestreada_m" = "Muestra_._info.longitud_muestreada_m",
    "ancho_muestreado_m" = "Muestra_._info.ancho_muestreado_m",
    "todos_invertebrados_muestreados" = "Muestra_transecto_invertebrados_info.todos_invertebrados_muestreados",
    "crustaceos_decapodos_carideos_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_carideos_muestreados",
    "crustaceos_decapodos_estenopodideos_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_estenopodideos_muestreados",
    "crustaceos_decapodos_aquelados_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_aquelados_muestreados",
    "crustaceos_decapodos_astacideos_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_astacideos_muestreados",
    "crustaceos_decapodos_braquiuros_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_braquiuros_muestreados",
    "crustaceos_decapodos_anomuros_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_anomuros_muestreados",
    "crustaceos_decapodos_estomatopodos_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_estomatopodos_muestreados",
    "crustaceos_decapodos_palinuridos_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_decapodos_palinuridos_muestreados",
    "crustaceos_no_decapodos_muestreados" = "Muestra_transecto_invertebrados_info.crustaceos_no_decapodos_muestreados",
    "moluscos_gastropodos_muestreados" = "Muestra_transecto_invertebrados_info.moluscos_gastropodos_muestreados",
    "moluscos_bivalvos_muestreados" = "Muestra_transecto_invertebrados_info.moluscos_bivalvos_muestreados",
    "moluscos_cefalopodos_muestreados" = "Muestra_transecto_invertebrados_info.moluscos_cefalopodos_muestreados",
    "otros_moluscos_muestreados" = "Muestra_transecto_invertebrados_info.otros_moluscos_muestreados",
    "equinodermos_crinoideos_muestreados" = "Muestra_transecto_invertebrados_info.equinodermos_crinoideos_muestreados",
    "equinodermos_asteroideos_muestreados" = "Muestra_transecto_invertebrados_info.equinodermos_asteroideos_muestreados",
    "equinodermos_ofiuroideos_muestreados" = "Muestra_transecto_invertebrados_info.equinodermos_ofiuroideos_muestreados",
    "equinodermos_equinoideos_muestreados" = "Muestra_transecto_invertebrados_info.equinodermos_equinoideos_muestreados",
    "equinodermos_holothuroideos_muestreados" = "Muestra_transecto_invertebrados_info.equinodermos_holothuroideos_muestreados",
    "otros_equinodermos_muestreados" = "Muestra_transecto_invertebrados_info.otros_equinodermos_muestreados",
    "otros_invertebrados_muestreados" = "Muestra_transecto_invertebrados_info.otros_invertebrados_muestreados",
    "detalles_invertebrados_muestreados" = "Muestra_transecto_invertebrados_info.detalles_invertebrados_muestreados",
    "muestreo_completado" = "Muestra_._info.muestreo_completado",
    "comentarios" = "Muestra_transecto_invertebrados_info.comentarios"
  ),
  
  "Muestra_transecto_invertebrados_cuenta#1" = c(
    "muestra_transecto_invertebrados_info_id" = "Muestra_transecto_invertebrados_info.id",
    "tipo" = "Muestra_transecto_invertebrados_cuenta.tipo",
    "cuenta" = "Muestra_transecto_invertebrados_cuenta.cuenta"
  ),
  
  ### Reclutas ###
  
  "Muestra_subcuadrante_de_transecto_reclutas_info#2" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "numero_cuadrante" = "Muestra_subcuadrante_de_transecto_reclutas_info.numero_cuadrante",
    "nivel_agregacion_datos" = "Muestra_._info.nivel_agregacion_datos",
    "observador" = "Muestra_._info.observador",
    "substrato" = "Muestra_subcuadrante_de_transecto_reclutas_info.sustrato",
    "comentarios" = "Muestra_subcuadrante_de_transecto_reclutas_info.comentarios"
  ),
  
  "Muestra_subcuadrante_de_transecto_reclutas_cuenta#1" = c(
    "muestra_subcuadrante_de_transecto_reclutas_info_id" = "Muestra_subcuadrante_de_transecto_reclutas_info.id",
    "codigo" = "Registro_observacion.codigo",
    "categoria_tamanio" = "Muestra_subcuadrante_de_transecto_reclutas_cuenta.categoria_tamanio",
    "tamanio_minimo_cm" = "Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_minimo_cm",
    "tamanio_maximo_cm" = "Muestra_subcuadrante_de_transecto_reclutas_cuenta.tamanio_maximo_cm",
    "cuenta" = "Muestra_subcuadrante_de_transecto_reclutas_cuenta.cuenta"
  ),
  
  ### Complejidad ###
  
  "Muestra_transecto_complejidad_info#1" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "observador" = "Muestra_._info.observador",
    "rugosidad_longitud_contorno_m" = "Muestra_transecto_complejidad_info.rugosidad_longitud_contorno_m",
    "rugosidad_longitud_lineal_m" = "Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_m",
    #"rugosidad_longitud_lineal_fija" = "Muestra_transecto_complejidad_info.rugosidad_longitud_lineal_fija",
    "comentarios" = "Muestra_transecto_complejidad_info.comentarios"
  ),
  
  "Muestra_subcuadrante_de_transecto_complejidad_info#2" = c(
    "muestra_transecto_id" = "Muestra_transecto.id",
    "numero_cuadrante" = "Muestra_subcuadrante_de_transecto_complejidad_info.numero_cuadrante",
    "observador" = "Muestra_._info.observador",
    "maximo_relieve_m" = "Muestra_subcuadrante_de_transecto_complejidad_info.maximo_relieve_m",
    "comentarios" = "Muestra_subcuadrante_de_transecto_complejidad_info.comentarios"
    )
)

lista_tablas_bd <- genera_tablas(
  datos_globales_llaves_numericas,
  sufijo_columnas_llaves_numericas,
  nombre_nuevo_columnas_llaves_numericas,
  relacion_tablas_columnas_funciones
)

# Revisión rápida de las tablas creadas.
l_ply(1:length(lista_tablas_bd), function(i){
  print(names(lista_tablas_bd)[i])
  glimpse(lista_tablas_bd[[i]])
})

saveRDS(lista_tablas_bd, paste0(rutas_salida[7], "/lista_tablas_bd.RDS"))

################################################################################

library("RSQLite")

lista_catalogos <- readRDS(paste0(rutas_salida[1], "/lista_catalogos.RData"))

base_output <- dbConnect(RSQLite::SQLite(), "../productos/v3/barco_db_v3.sqlite")

# Insertando catálogos:
l_ply(1:length(lista_catalogos), function(i){
  dbWriteTable(base_output, names(lista_catalogos)[i], lista_catalogos[[i]])
})

# Insertando_tablas:
l_ply(1:length(lista_tablas_bd), function(i){
  dbWriteTable(base_output, names(lista_tablas_bd)[i], lista_tablas_bd[[i]])
})



