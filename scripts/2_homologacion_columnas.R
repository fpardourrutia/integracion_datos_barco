# En este script:
# 1. Se homologan los nombres de las columnas en cada archivo de Excel.
# 2. Se realiza el join de cada una de las tablas en una carpeta
# ("conacyt_greenpeace_2016", "datos_historicos_datos_2017") con su correspondiente
# tabla de campos individuales, para completar la información en cada una.

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")

################################################################################
# CONACyT / GreenPeace 2016
################################################################################

lista_tablas_crudas_exceles_conacyt_greenpeace_2016 <- readRDS(
  paste0(rutas_salida[1], "/lista_tablas_crudas_exceles_conacyt_greenpeace_2016.RData"))
tabla_campos_adicionales_conacyt_greenpeace_2016 <- readRDS(
  paste0(rutas_salida[1], "/tabla_campos_adicionales_conacyt_greenpeace_2016.RData"))

# Homologando los nombres de las columnas, y agregando las columnas en el archivo
# "campos_adicionales_sitio.xlsx"
lista_tablas_columnas_homologadas_conacyt_greenpeace_2016 <- lista_tablas_crudas_exceles_conacyt_greenpeace_2016 %>%
  renombra_columna("101a110cm", "peces_tamanio_101cm_110cm") %>%
  renombra_columna("111a120cm", "peces_tamanio_111cm_120cm") %>%
  renombra_columna("11a20cm     ll", "peces_tamanio_11cm_20cm") %>%
  renombra_columna("191a200cm", "peces_tamanio_191cm_200cm") %>%
  renombra_columna("21a30cm", "peces_tamanio_21cm_30cm") %>%
  renombra_columna("31a40cm", "peces_tamanio_31cm_40cm") %>%
  renombra_columna("41a50cm", "peces_tamanio_41cm_50cm") %>%
  renombra_columna("51a60cm", "peces_tamanio_51cm_60cm") %>%
  renombra_columna("61a70cm", "peces_tamanio_61cm_70cm") %>%
  renombra_columna("6a10cm", "peces_tamanio_6cm_10cm") %>%
  renombra_columna("71a80cm", "peces_tamanio_71cm_80cm") %>%
  renombra_columna("81a90cm", "peces_tamanio_81cm_90cm") %>%
  renombra_columna("91a100cm", "peces_tamanio_91cm_100cm") %>%
  renombra_columna("Ancho_del_cuadrante_m", "ancho_cuadrante_m") %>%
  renombra_columna("Ancho_del_transecto_metros", "ancho_transecto_m") %>%
  renombra_columna("Anio_de_muestreo", "anio_muestreo") %>%
  renombra_columna("Anio_De_Publicacion", "anio_publicacion") %>%
  renombra_columna("Autor_Administrador_Del_Proyecto", "autor_administrador_proyecto") %>%
  renombra_columna("Blaqueamieto", "blanqueamiento") %>%
  renombra_columna("Depredación", "depredacion") %>%
  renombra_columna("Día", "dia") %>%
  renombra_columna("Dia", "dia") %>%
  renombra_columna("Fecha_de_inicio", "anio_inicio_proyecto") %>%
  renombra_columna("Fecha_de_termino", "anio_termino_proyecto") %>%
  renombra_columna("Longitud_del_cuadrante_m", "longitud_cuadrante_m") %>%
  renombra_columna("Longitud_del_transecto_metro", "longitud_transecto_m") %>%
  renombra_columna("menores_a_5m", "peces_tamanio_0cm_5cm") %>%
  renombra_columna("Metodo_de_Seleccion_de_Sitios", "metodo_seleccion_sitios") %>%
  renombra_columna("Mortalidad_Transición", "mortalidad_transicion") %>%
  renombra_columna("Muestreo_completo", "muestreo_completo") %>%
  renombra_columna("Muestrreo_completo", "muestreo_completo") %>%
  renombra_columna("Nivel_de_agregacion_de_datos", "nivel_agregacion_datos") %>%
  renombra_columna("Nombre_del_sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_Sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_Proyecto", "nombre_proyecto") %>%
  renombra_columna("Numero", "conteo") %>%
  renombra_columna("Numero_de_sitios_agregados", "numero_sitios_agregados") %>%
  renombra_columna("Protocolo_utilizado", "metodo") %>% #!!!
  renombra_columna("Profundidad_final", "profundidad_final_m") %>%
  renombra_columna("Profundidad_inicial", "profundidad_inicial_m") %>%
  renombra_columna("Profundidad_media_m", "profundidad_media_m") %>%
  renombra_columna("Profundidad_media_metros", "profundidad_media_m") %>%
  renombra_columna("Region_del_arrecife_HR", "region_healthy_reefs") %>%
  renombra_columna("Tamanio_de_cadena_metros", "tamanio_cadena_m") %>%
  renombra_columna("Temperatura_celsius", "temperatura_c") %>% #importante
  renombra_columna("Temperatura_en _Celcius", "temperatura_c") %>% #importante
  renombra_columna("Unidades_de_profundidad", "unidades_profundidad") %>%
  renombra_columnas_minusculas() %>%
  llply(function(df){
    df %>%
      mutate(
        nombre_sitio = estandariza_strings(nombre_sitio)
      )
  }) %>%
  inner_join_lista(tabla_campos_adicionales_conacyt_greenpeace_2016, "nombre_sitio")
  
# Como no sacó ningún warning el inner_join_lista, los tamaños de todos los data
# frames se quedaron igual (no hubo artefactos).

# Revisando:
lista_tablas_columnas_homologadas_conacyt_greenpeace_2016 %>%
  crea_resumen_columnas_df() %>%
  glimpse()

################################################################################
# Datos históricos / datos 2017
################################################################################

lista_tablas_crudas_exceles_historicos_y_2017 <- readRDS(
  paste0(rutas_salida[1], "/lista_tablas_crudas_exceles_historicos_y_2017.RData"))
tabla_campos_adicionales_historicos_y_2017 <- readRDS(
  paste0(rutas_salida[1], "/tabla_campos_adicionales_historicos_y_2017.RData"))

# Homologando los nombres de las columnas, y agregando las columnas en el archivo
# "campos_adicionales_sitio.xlsx"

lista_tablas_columnas_homologadas_historicos_y_2017 <- lista_tablas_crudas_exceles_historicos_y_2017 %>%
  renombra_columna("Nombre_del_Sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_del_sitio", "nombre_sitio") %>%
  renombra_columna("Nombre_sitio", "nombre_sitio") %>%
  renombra_columna("menores_a_5m", "peces_tamanio_0cm_5cm") %>%
  renombra_columna("91-100", "peces_tamanio_91cm_100cm") %>%
  renombra_columna(">100", "peces_tamanio_101cm_9999cm") %>%
  renombra_columna("101a110cm", "peces_tamanio_101cm_110cm") %>%
  renombra_columna("11-20cm", "peces_tamanio_11cm_20cm") %>%
  renombra_columna("111a120cm", "peces_tamanio_111cm_120cm") %>%
  renombra_columna("191a200cm", "peces_tamanio_191cm_200cm") %>%
  renombra_columna("21-30cm", "peces_tamanio_21cm_30cm") %>%
  renombra_columna("31-40cm", "peces_tamanio_31cm_40cm") %>%
  renombra_columna("41-50", "peces_tamanio_41cm_50cm") %>%
  renombra_columna("51-60", "peces_tamanio_51cm_60cm") %>%
  renombra_columna("6-10cm", "peces_tamanio_6cm_10cm") %>%
  renombra_columna("61-70", "peces_tamanio_61cm_70cm") %>%
  renombra_columna("71-80", "peces_tamanio_71cm_80cm") %>%
  renombra_columna("81-90", "peces_tamanio_81cm_90cm") %>%
  renombra_columna("91-100", "peces_tamanio_91cm_100cm") %>%
  renombra_columna("Altura_Algas_", "altura_algas_cm") %>%
  renombra_columna("Altura_Algas_", "altura_algas_cm") %>%
  renombra_columna("Ancho transecto", "ancho_transecto_m") %>%
  renombra_columna("Ancho_del_transecto_metros", "ancho_transecto_m") %>%
  renombra_columna("Ancho_del_cuadrante_m", "ancho_cuadrante_m") %>%
  renombra_columna("Anio de muestreo", "anio_muestreo") %>%
  renombra_columna("Anio_de_muestreo", "anio_muestreo") %>%
  renombra_columna("Anio_De_Publicacion", "anio_publicacion") %>%
  renombra_columna("Año de publicación", "anio_publicacion") %>%
  renombra_columna("Año_De_Publicacion", "anio_publicacion") %>%
  renombra_columna("Autor_Administrador_Del_Proyecto", "autor_administrador_proyecto") %>%
  renombra_columna("Autor/administrador del proyecto", "autor_administrador_proyecto") %>%
  renombra_columna("Blaqueamieto", "blanqueamiento") %>%
  renombra_columna("Categoria_Tamanio (R o SC)", "categoria_tamanio") %>%
  renombra_columna("CIF", "clump") %>%
  renombra_columna("Curp Proyecto", "curp_proyecto") %>%
  renombra_columna("Curp_Proyecto", "curp_proyecto") %>%
  renombra_columna("Depredación", "depredacion") %>%
  renombra_columna("Dia", "dia") %>%
  renombra_columna("Día", "dia") %>%
  renombra_columna("Fecha de inicio", "anio_inicio_proyecto") %>%
  renombra_columna("Fecha_de_inicio", "anio_inicio_proyecto") %>%
  renombra_columna("Fecha de término", "anio_termino_proyecto") %>%
  renombra_columna("Fecha_de_termino", "anio_termino_proyecto") %>%
  renombra_columna("Institucion", "institucion") %>%
  renombra_columna("Institución", "institucion") %>%
  renombra_columna("Longitud del transecto (m)", "longitud_transecto_m") %>%
  renombra_columna("Longitud_del_transecto_metro", "longitud_transecto_m") %>%
  renombra_columna("Longitud_del_cuadrante_m", "longitud_cuadrante_m") %>%
  renombra_columna("Método de Selección de Sitios", "metodo_seleccion_sitios") %>%
  renombra_columna("Metodo_de_Seleccion_de_Sitios", "metodo_seleccion_sitios") %>%
  renombra_columna("Mortalidad_Transición", "mortalidad_transicion") %>%
  renombra_columna("Nivel de agregacion de datos", "nivel_agregacion_datos") %>%
  renombra_columna("Nivel_de_agregacion_de_datos", "nivel_agregacion_datos") %>%
  renombra_columna("Nombre del Proyecto", "nombre_proyecto") %>%
  renombra_columna("Nombre_del_Proyecto", "nombre_proyecto") %>%
  renombra_columna("Nombre_no_remuestreo", "nombre_no_remuestreo") %>%
  renombra_columna("Nombre_original", "nombre_original") %>%
  renombra_columna("Nombre_de_remuestreo", "nombre_remuestreo") %>%
  renombra_columna("Nombre_remuestreo", "nombre_remuestreo") %>%
  # También cambiarla en CONACyT/GreenPeace, ya que se tiene sólo para invertebrados.
  renombra_columna("Numero", "conteo") %>%
  renombra_columna("Numero de sitios agregados", "numero_sitios_agregados") %>%
  renombra_columna("Numero_de_sitios_agregados", "numero_sitios_agregados") %>%
  renombra_columna("PAIS", "pais") %>%
  renombra_columna("PAÍS", "pais") %>%
  renombra_columna("Profundidad media (m)", "profundidad_media_m") %>%
  renombra_columna("Profundidad_media_m", "profundidad_media_m") %>%
  renombra_columna("Profundidad_media_metros", "profundidad_media_m") %>%
  renombra_columna("Profundidad_inicial", "profundidad_inicial_m") %>%
  renombra_columna("Profundidad_final", "profundidad_final_m") %>%
  renombra_columna("profundidad_media_m_sitio", "profundidad_media_m_sitio") %>%
  renombra_columna("Profundidad_media_sitio", "profundidad_media_m_sitio") %>%
  renombra_columna("Profundidad_promedio_sitio", "profundidad_media_m_sitio") %>%
  renombra_columna("Profundidad_promedio_sitios", "profundidad_media_m_sitio") %>%
  renombra_columna("Regio_HR", "region_healthy_reefs") %>%
  renombra_columna("Región_del_arrecife", "region_healthy_reefs") %>%
  renombra_columna("Region_del_arrecife_HR", "region_healthy_reefs") %>%
  renombra_columna("Suborganizacion", "suborganizacion") %>%
  renombra_columna("Suborganización", "suborganizacion") %>%
  renombra_columna("subtipo_de_arrecife", "subtipo_arrecife") %>%
  renombra_columna("Subzona_de_habitat", "subzona_habitat") %>%
  renombra_columna("Tamanio_de_cadena", "tamanio_cadena_m") %>%
  renombra_columna("Temperatura_celsius", "temperatura_c") %>%
  renombra_columna("Tipo de Arrecife", "tipo_arrecife") %>%
  renombra_columna("Tipo_de_Arrecife", "tipo_arrecife") %>%
  renombra_columna("Titulo", "titulo") %>%
  renombra_columna("Tìtulo", "titulo") %>%
  renombra_columna("Unidades de profundidad", "unidades_profundidad") %>%
  renombra_columna("Unidades_de_profundidad", "unidades_profundidad") %>%
  renombra_columna("Zona arrecifal", "zona_arrecife") %>%
  renombra_columna("Zona_arrecifal", "zona_arrecife") %>%
  renombra_columnas_minusculas() %>%
  
  # columna redundantes con las de campos adicionales:
  renombra_columna("anp", "anp_redundante") %>%
  renombra_columna("profundidad_media_m_sitio", "profundidad_media_m_sitio_redundante") %>%
  renombra_columna("region_healthy_reefs", "region_healthy_reefs_redundante") %>%
  renombra_columna("tipo_arrecife", "tipo_arrecife_redundante") %>%
  renombra_columna("subtipo_arrecife", "subtipo_arrecife_redundante") %>%
  renombra_columna("zona_arrecife", "zona_arrecife_redundante") %>%
  
  llply(function(df){
    resultado <- df %>%
      mutate(
        nombre_sitio = estandariza_strings(nombre_sitio)
      )
  }) %>%
  inner_join_lista(
    tabla_campos_adicionales_historicos_y_2017 %>%
      mutate(nombre_sitio = estandariza_strings(nombre_sitio)),
    "nombre_sitio"
  )

# Como no sacó ningún warning el inner_join_lista, los tamaños de todos los data
# frames se quedaron igual (no hubo artefactos).

# Revisando:
lista_tablas_columnas_homologadas_historicos_y_2017 %>%
  crea_resumen_columnas_df() %>%
  glimpse()

################################################################################
# Pegando las listas anteriores y revisando nombres de las columnas
################################################################################

lista_tablas_columnas_homologadas <- append(
  lista_tablas_columnas_homologadas_conacyt_greenpeace_2016,
  lista_tablas_columnas_homologadas_historicos_y_2017
)

# Revisando nombres de las columnas
names(lista_tablas_columnas_homologadas)
lista_tablas_columnas_homologadas %>%
  crea_resumen_columnas_df() %>%
  glimpse()

# Revisando que no haya duplicados en los nombres de las columnas dentro de cada
# Excel, causados, por ejemplo, por renombrar una columna a un nombre anteriormente
# usado
ldply(lista_tablas_columnas_homologadas, function(df){
  numero_columnas_nombres_duplicados <- colnames(df) %>%
    duplicated() %>%
    sum()
  return(numero_columnas_nombres_duplicados)
})
# Perfecto!

saveRDS(lista_tablas_columnas_homologadas,
  paste0(rutas_salida[2], "/lista_tablas_columnas_homologadas.RData"))


