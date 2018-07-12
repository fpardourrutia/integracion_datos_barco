library("plyr")

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

# Ruta raiz de salida (relativa a la localización de los scripts)
ruta_raiz_salida <- "../productos/v3"
dir.create(ruta_raiz_salida)

# Vector de rutas de salida por número de script. Notar que los nombres de las
# carpetas correspondientes a cada script son derivados del nombre de este
# (no son los mismos, para mejorar el entendimiento de las salidas)
rutas_salida <- c()
rutas_salida[1] <- paste0(ruta_raiz_salida, "/1_tablas_crudas_exceles")
rutas_salida[2] <- paste0(ruta_raiz_salida, "/2_tablas_columnas_homologadas")
rutas_salida[3] <- paste0(ruta_raiz_salida, "/3_revisiones_catalogos")
rutas_salida[4] <- paste0(ruta_raiz_salida, "/4_revisiones_informacion_exceles_contra_esquema")
rutas_salida[5] <- paste0(ruta_raiz_salida, "/5_revisiones_consistencia_informacion_exceles")
rutas_salida[6] <- paste0(ruta_raiz_salida, "/6_datos_globales")

# Creando carpetas de salida
l_ply(rutas_salida, function(ruta){
  dir.create(ruta)
})