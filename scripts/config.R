# En este script se especifican los diversos parámetros que permean a todo los
# scripts de integración de datos, por lo que conviene tenerlos concentrados
# en un sólo lugar

# Ruta de la carpeta con los datos V3:
ruta_carpeta_datos_v3 <- "~/Dropbox/carpetas_compartidas/barco_lab/bases_integracion/datos_v3_preeliminar"

# Rutas de entrada adicionales:
ruta_carpeta_conacyt_greenpeace_2016 <- paste0(ruta_carpeta_datos_v3, "/conacyt_greenpeace_2016")
ruta_carpeta_historicos_y_2017 <- paste0(ruta_carpeta_datos_v3, "/historicos_y_2017")
ruta_carpeta_catalogos <- paste0(ruta_carpeta_datos_v3, "/catalogos")

# Cada una de las carpetas anteriores contendrá la siguiente carpeta, que contiene
# campos adicionales:
subruta_tabla_campos_adicionales_sitio <- "/_tablas_individuales/campos_adicionales_sitio.xlsx"

# Ruta de salida:
ruta_salida <- "../productos/v3"

# Rutas de salida por número de script, notar que los nombres de las carpetas
# correspondientes a cada script son derivados del nombre de este (no son los mismos,
# para mejorar el entendimiento de las salidas)
ruta_salidas_0_leer_exceles <- paste0(ruta_salida, "/0_tablas_crudas_exceles")
ruta_salidas_1_homologar_columnas <- paste0(ruta_salida, "/1_tablas_columnas_homologadas")
ruta_salidas_2_revisar_listas_exceles <- paste0(ruta_salida, "/2_revisiones_exceles")

# Creando carpetas de salida
dir.create(ruta_salidas_0_leer_exceles)
dir.create(ruta_salidas_1_homologar_columnas)
dir.create(ruta_salidas_2_revisar_listas_exceles)
