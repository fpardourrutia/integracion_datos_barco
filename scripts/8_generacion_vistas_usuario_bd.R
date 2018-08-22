# En este script se crean las vistas de usuario, es decir, los queries
# predefinidos que despliegan información de interés para el usuario.

# Cargando archivo de configuración y funciones auxiliares
source("config.R")
source("funciones_auxiliares.R")
library("readr")

# Pasos a seguir:
# 1. Se leen las listas de tablas y catálogos.
# 2. Se generan algunas tablas auxiliares que son tronco común para generar las
# vistas.
# 3. Se generan las vistas de usuario.
# 4. Se genera la lista de vistas que se utilizará para insertarlas en la base

################################################################################
# 1. Leyendo listas de tablas y catálogos
################################################################################

lista_tablas_bd <- readRDS(paste0(rutas_salida[7], "/lista_tablas_bd.RData"))
lista_catalogos_bd <- readRDS(paste0(rutas_salida[7], "/lista_catalogos_bd.RData"))

names(lista_tablas_bd)
names(lista_catalogos_bd)

################################################################################
# 2. Generando tablas auxiliares que serán de utilidad al generar las vistas
################################################################################

# Aunque es considerada mala práctica, en este caso el uso de attach() y detach()
# simplificarán por mucho la generación de vistas

attach(lista_tablas_bd)
attach(lista_catalogos_bd)

################################################################################

# La siguiente tabla es auxiliar para la generación de vistas y corresponde a una
# relación de muestreos - muestras de sitio. Cabe destacar que en las vistas se
# usan nombres de columnas menos técnicos y más amigables para el usuario.
Auxiliar_muestreos_sitios <- Muestreo %>%
  select(
    muestreo_id = id,
    muestreo = nombre
  ) %>%
  inner_join(
    Muestra_sitio %>%
      select(
        muestra_sitio_id = id,
        muestreo_id,
        sitio = nombre,
        fecha,
        pais,
        latitud,
        longitud,
        datum
      ), by = "muestreo_id") %>%
  select(-muestreo_id)

# La siguiente tabla es auxiliar para la generación de vistas y corresponde a una
# relación de muestreos - muestras de sitio - muestras de transecto

Auxiliar_muestreos_sitios_transectos <- Auxiliar_muestreos_sitios %>%
  # Notar que en el inner join desaparecen los sitios que no tienen transectos
  # declarados
  inner_join(
    Muestra_transecto %>%
      select(
        muestra_transecto_id = id,
        muestra_sitio_id
        # transecto = nombre # Con "muestra_transecto_id" es más que suficiente
      ), by = "muestra_sitio_id")
  # "muestra_sitio_id" es una llave tan importante que se decide mantenerla

################################################################################
# 3. Generando diversas vistas de usuario utilizando las anteriores tablas
################################################################################

# Al generar las vistas, si sabemos que la información de interés (por ejemplo,
# "muestra_sitio_bentos") está a nivel de sitio, es conveniente hacer el join
# con la tabla auxiliar apropiada, con el fin de evitar el riesgo de duplicar
# registros si de causalidad ese sitio tiene transectos asociados.

################################################################################

### Vista_sitios_visitados ###

# Vista que resume los sitios visitados. Supuestos:
# 1. Dos muestreos de sitio son remuestreos del mismo sitio si y solo si tienen
# el mismo nombre.
# 2. Entre remuestreos del mismo sitio, la latitud y longitud están calculadas
# sobre el mismo datum, y están expresadas con las mismas convenciones. Esto para
# que calcular el promedio de las latitudes y longitudes entre remuestreos tenga
# sentido.
# 3. No existen muestreos de sitio duplicados introducidos en la base (este hecho
# se revisará a detalle en scripts posteriores)

Vista_sitios_visitados <- Auxiliar_muestreos_sitios %>%
  group_by(sitio) %>%
  summarise(
    pais = first(pais),
    latitud_promedio = mean(latitud),
    longitud_promedio = mean(longitud),
    datum = first(datum),
    numero_muestreos = n()
  ) %>%
  ungroup() %>%
  arrange(pais, sitio)

################################################################################

### Vista_porcentaje_coberturas_bentos_sitio_nivel_3 ###

# Vista que calcula los porcentajes de cobertura por sitio de bentos categorizados
# de acuerdo al nivel 3 especificado en el catálogo:
# "Catalogo_registro_bentos__codigo". Supuestos:
# 1. Existe a lo más un registro de "Muestra_sitio_bentos_info" por muestra de
# sitio.
# 2. Los porcentajes de cobertura en la tabla de "Muestra_sitio_bentos_porcentaje"
# suman 100% para cada muestra de sitio.
# 3. Existe a lo más un registro "Muestra_transecto_bentos_info" por muestra
# de transecto.
# 4. Los porcentajes de cobertura de la tabla "Muestra_transecto_bentos_porcentaje"
# suman 100% para cada muestra de transecto.
# 5. No hay categorías / códigos duplicados en los catálogos. Si los hubiera, se
# duplicarían registos a la hora de hacer el inner join. para extraer la
# información contenida en estos.
# 6. Para calcular el porcentaje de cobertura por sitio para un tipo de
# cobertura determinado, simplemente se calcula el promedio de los porcentajes
# de cobertura que cada transecto tuvo de ese tipo (pueden ser 0 para
# determinados transectos).
# Por ahora no se contempla la ponderación por longitud de transecto.
# 7. No hay NA's en los porcentajes de cobertura de las tablas:
# "Muestra_sitio_bentos_porcentaje" y "Muestra_transecto_bentos_porcentaje".

Vista_porcentaje_coberturas_bentos_sitio_nivel_3 <- Auxiliar_muestreos_sitios %>%
  
  ### Datos en "Muestra_sitio_bentos_porcentaje" ###
  
  inner_join(Muestra_sitio_bentos_info %>%
      select(
        muestra_sitio_bentos_info_id = id,
        muestra_sitio_id), by = "muestra_sitio_id") %>%
  inner_join(Muestra_sitio_bentos_porcentaje %>%
      select(-id), by = "muestra_sitio_bentos_info_id") %>%
  
  # El join con los catálogos se hace de una vez y no se deja hasta el final
  # debido a que cuando se procese la información de porcentajes de cobertura
  # por sitio para datos registrados por transecto, se requerirá calcular los
  # promedios por tipo de cobertura, y es más inmediato hacerlo si obtenemos
  # los tipos de interés primero.
  inner_join(
    Catalogo_registro_bentos__codigo %>%
      transmute(
        codigo,
        nivel_3 = estandariza_strings(nivel_3)
      ), by = "codigo"
  ) %>%
  select(
    -muestra_sitio_bentos_info_id,
    -codigo
  ) %>%
  
  # Sumando porcentajes de cobertura para cada muestra de sitio y nivel
  # ocurren repetidos porque muchos códigos pertenecen al mismo nivel 3 en
  # el catálogo.
  group_by(
    muestreo,
    muestra_sitio_id,
    sitio,
    fecha,
    pais,
    latitud,
    longitud,
    datum,
    nivel_3
  ) %>%
  summarise(
    porcentaje_cobertura = sum(porcentaje_cobertura)
  ) %>%
  ungroup() %>%
  mutate(tabla = "Muestra_sitio_bentos_porcentaje") %>%
  
  ### Datos en "Muestra_transecto_bentos_porcentaje" ###
  
  rbind(
    Auxiliar_muestreos_sitios_transectos %>%
      inner_join(Muestra_transecto_bentos_info %>%
          select(
            muestra_transecto_bentos_info_id = id,
            muestra_transecto_id
          ), by = "muestra_transecto_id") %>%
      inner_join(Muestra_transecto_bentos_porcentaje %>%
          select(-id), by = "muestra_transecto_bentos_info_id"
      ) %>%
      inner_join(
        Catalogo_registro_bentos__codigo %>%
          transmute(
            codigo,
            nivel_3 = estandariza_strings(nivel_3)
          ), by = "codigo"
      ) %>%
      select(
        -muestra_transecto_bentos_info_id,
        -codigo
      ) %>%
      
      # Sumando porcentajes de cobertura para cada muestra de transecto y nivel
      group_by(
        muestreo,
        muestra_sitio_id,
        sitio,
        fecha,
        pais,
        latitud,
        longitud,
        datum,
        muestra_transecto_id,
        nivel_3
      ) %>%
      summarise(
        porcentaje_cobertura = sum(porcentaje_cobertura)
      ) %>%
      ungroup() %>%
      
      # Completando para cada muestreo de transecto distinto (distinguible por
      # su "muestra_transecto_id"), todos los posibles tipos de "nivel 3" (con
      # 0%), con el fin de poder simplemente sacar promedios para obtener los
      # porcentajes de cobertura por sitio.
      complete(
        
        # Para cada "muestra_transecto_id"
        nesting(
          muestreo,
          muestra_sitio_id,
          sitio,
          fecha,
          pais,
          latitud,
          longitud,
          datum,
          muestra_transecto_id
        ),
        
        # Complétame los valores de "nivel_3"
        nivel_3,
        
        # Con porcentaje de cobertura igual a 0.
        fill = list("porcentaje_cobertura" = 0)
      ) %>%
      
      # Calculando porcentajes de cobertura por sitio. Notar que si los
      # porcentajes de cobertura suman 100% por muestra de transecto, entonces,
      # por ley de las esperanzas iteradas, los porcentajes de cobertura por
      # muestra de sitio también suman 100%.
      group_by(
        muestreo,
        muestra_sitio_id,
        sitio,
        fecha,
        pais,
        latitud,
        longitud,
        datum,
        nivel_3
      ) %>%
      summarise(
        porcentaje_cobertura = mean(porcentaje_cobertura)
      ) %>%
      ungroup() %>%
      mutate(tabla = "Muestra_transecto_bentos_porcentaje")
  ) %>%
  
  ### Datos en "Muestra_transecto_bentos_punto" ###
  
  rbind(
    Auxiliar_muestreos_sitios_transectos %>%
      inner_join(Muestra_transecto_bentos_info %>%
          select(
            muestra_transecto_bentos_info_id = id,
            muestra_transecto_id
          ), by = "muestra_transecto_id") %>%
      inner_join(Muestra_transecto_bentos_punto %>%
          select(
            -id,
            -numero_punto,
            -altura_si_alga_cm
          ), by = "muestra_transecto_bentos_info_id"
      ) %>%
      inner_join(
        Catalogo_registro_bentos__codigo %>%
          transmute(
            codigo,
            nivel_3 = estandariza_strings(nivel_3)
          ), by = "codigo"
      ) %>%
      select(
        -muestra_transecto_bentos_info_id,
        -codigo
      ) %>%
      
      # Calculando número de puntos por muestreo de transecto
      group_by(
        muestreo,
        muestra_sitio_id,
        sitio,
        fecha,
        pais,
        latitud,
        longitud,
        datum,
        muestra_transecto_id
      ) %>%
      mutate(
        numero_puntos = n()
      ) %>%
      ungroup() %>%
      
      # Calculando porcentajes de cobertura por muestreo de transecto
      group_by(
        muestreo,
        muestra_sitio_id,
        sitio,
        fecha,
        pais,
        latitud,
        longitud,
        datum,
        muestra_transecto_id,
        nivel_3
      ) %>%
      summarise(
        porcentaje_cobertura = n() * 100 / first(numero_puntos)
      ) %>%
      ungroup() %>%
      
      # Completando para cada muestreo de transecto distinto (distinguible por)
      # su "muestra_transecto_id", todos los posibles tipos de "nivel 3" (con
      # 0%), con el fin de poder simplemente sacar promedios para obtener los
      # porcentajes de cobertura por sitio.
      complete(
        # Para cada "muestra_transecto_id"
        nesting(
          muestreo,
          muestra_sitio_id,
          sitio,
          fecha,
          pais,
          latitud,
          longitud,
          datum,
          muestra_transecto_id
        ),
        # Complétame los valores de "nivel_3"
        nivel_3,
        # Con porcentaje de cobertura igual a 0.
        fill = list("porcentaje_cobertura" = 0)
      ) %>%
      
      # Calculando porcentajes de cobertura por sitio. Notar que si los
      # porcentajes de cobertura suman 100% por muestra de transecto, entonces,
      # por ley de las esperanzas iteradas, los porcentajes de cobertura por
      # muestra de sitio también suman 100%.
      group_by(
        muestreo,
        muestra_sitio_id,
        sitio,
        fecha,
        pais,
        latitud,
        longitud,
        datum,
        nivel_3
      ) %>%
      summarise(
        porcentaje_cobertura = mean(porcentaje_cobertura)
      ) %>%
      ungroup() %>%
      mutate(tabla = "Muestra_transecto_bentos_punto")
  ) %>%
  
  # Al terminar de unir todas las tablas mediante rbind(), calcular totales de
  # porcentajes de cobertura por muestra de sitio
  ddply(.(muestra_sitio_id, tabla), function(df){
    resultado <- df %>%
      
      # Calculando un renglón único que contiene la suma de los porcentajes de
      # cobertura para el muestreo de sitio en cuestión
      rbind(
        df %>%
          group_by(
            muestreo,
            muestra_sitio_id,
            sitio,
            fecha,
            pais,
            latitud,
            longitud,
            datum,
            tabla
          ) %>%
          summarise(
            nivel_3 = "total",
            porcentaje_cobertura = sum(porcentaje_cobertura)
          ) %>%
          ungroup()
      )
    return(resultado)
  }
  ) %>%
  
  # Dándole a la vista un formato más amigable para el usuario.
  spread(nivel_3, porcentaje_cobertura, 0)

write_csv(Vista_porcentaje_coberturas_bentos_sitio_nivel_3,
  paste0(rutas_salida[8], "/vista_porcentaje_coberturas_bentos_sitio_nivel_3.csv"))
  
################################################################################

detach("lista_catalogos_bd")
detach("lista_tablas_bd")

################################################################################
# 4. Generando la lista que contendrá las vistas para facilitar su integración
# a la base de datos
################################################################################

lista_vistas_bd <- list(
  "Vista_sitios_visitados" = Vista_sitios_visitados,
  "Vista_porcentaje_coberturas_bentos_sitio_nivel_3" = Vista_porcentaje_coberturas_bentos_sitio_nivel_3
)
  
saveRDS(lista_vistas_bd,
  paste0(rutas_salida[8], "/lista_vistas_bd.RData"))
