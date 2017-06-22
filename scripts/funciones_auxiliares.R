library("plyr")
library("dplyr")
library("stringi")

###### Funciones auxiliares sobre listas de data frames

# Función auxiliar para renombrar una columna en todos los exceles. Para usarla
# un supuesto es que la columna "nombre_nuevo" no existe si existe "nombre_anterior":
# lista_df: lista de data frames
# nombre_anterior: nombre de la columna a renombrar
# nombre_nuevo: nombre nuevo de la columna
# La función regresa una lista de data frames, por lo que se puede pipear.
renombra_columna <- function(lista_df, nombre_anterior, nombre_nuevo){
  llply(lista_df, function(df, nombre_anterior, nombre_nuevo){
    # Obteniendo nombres de las columnas del df
    nombres_columnas <- names(df)
    # Renombrando columna
    nombres_columnas[nombres_columnas == nombre_anterior] <- nombre_nuevo
    # Sustituyendo nombres en el df
    names(df) <- nombres_columnas
    return(df)
  }, nombre_anterior, nombre_nuevo)
}

###### Funciones auxiliares sobre data frames

# Función auxiliar a la hora de programar, que regresa los nombres de las columnas
# de un df que contienen cierto string.
# df: data frame de interés
# x: string de interés
# La función regresa un vector con los nombres de las columnas de df que contienen
# "string".
encuentra_columnas <- function(df, x){
  nombres_columnas <- names(df) %>%
    sort()
  indice <- stri_detect_fixed(nombres_columnas, x, case_insensitive = TRUE)
  return(nombres_columnas[indice])
}

# Función auxiliar para intercambiar un valor por otro de determinada columna
# en un data frame (mientras exista la columna).
# df: data frame de interés
# nombre_columna: nombre de la columna de interés.
# valor_anterior: valor de la columna a sustituir (puede ser NA).
# valor_nuevo
# la función regresa un data frame, por lo que se puede pipear.
cambia_valor_columna <- function(df, nombre_columna, valor_anterior, valor_nuevo){
  # Si la columna está en df, se cambia el valor de la misma, de lo contrario,
  # el df se deja idéntico y se envía un warning
  if(nombre_columna %in% names(df)){
    
    # Si valor_anterior es NA, se debe hacer un procedimiento distinto
    if(is.na(valor_anterior)){
      df[[nombre_columna]][is.na(df[[nombre_columna]])] <- valor_nuevo
    }else{
      df[[nombre_columna]] <- ifelse(
        !is.na(df[[nombre_columna]]) & df[[nombre_columna]] == valor_anterior,
        valor_nuevo, df[[nombre_columna]]
      )
    }
  }else{
    warning("La columna especificada no está en el data frame")
  }
  return(df)
}

# Función para transformar un conjunto de variables a tipo numérico:
# df: data_frame cuyas variables se quieren transformar
# ...: nombres de las variables a transformar
mutate_numeric <- function(df, ...){
  # Obteniendo variables como un vector
  variables <- list(...) %>%
    as.character() 
  
  # Formando la expresión para el mutate_
  expresion <- paste0("as.numeric(", variables, ")") %>%
    as.list()
  
  # Asignando nombres a la expresión porque el mutate_ los necesita para asignar
  # nombres a las nuevas variables
  names(expresion) <- variables
  
  resultado <- df %>%
    mutate_(.dots = expresion)
  
  return(resultado)
}

# Función para transformar un conjunto de variables a tipo lógico:
# df: data_frame cuyas variables se quieren transformar
# ...: nombres de las variables a transformar
mutate_logical <- function(df, ...){
  # Obteniendo variables como un vector
  variables <- list(...) %>%
    as.character() 
  
  # Formando la expresión para el mutate_
  expresion <- paste0("as.logical(", variables, ")") %>%
    as.list()
  
  # Asignando nombres a la expresión porque el mutate_ los necesita para asignar
  # nombres a las nuevas variables
  names(expresion) <- variables
  
  resultado <- df %>%
    mutate_(.dots = expresion)
  
  return(resultado)
}

# Función auxiliar para generar una llave numérica a partir de una llave natural:
# df: data frame al que se le agregará una columna con una llave numérica
# a partir de una llave natural (definida a partir de una o varias columnas
# que contiene). Es decir, dos registros tendrán el mismo número si y sólo si
# tienen los mismos valores en las columnas de la llave natural.
# nombre_columna_llave: nombre de la columna que contendrá la llave numérica
# ...: nombre de las variables que definen la llave natural.
# El resultado es el df con la llave generada como una nueva columna
genera_llave <- function(df, nombre_columna_llave, ...){
  
  # Generando la expresión para el mutate_:
  nombres_variables_llave_natural <- c(...)
  
  expresion = paste0(
    "paste0(",
    paste0(".data$", nombres_variables_llave_natural) %>%
      paste0(collapse = ", \"_\", "),
    ") ",
    "%>% as.factor() %>% as.numeric()"
  ) %>%
    as.list()
  
  # Asignando el nombre de la columna que contendrá la llave numérica a la expresión
  # porque el mutate_ lo requiere
  names(expresion) <- nombre_columna_llave
  
  resultado <- df %>%
    mutate_(.dots = expresion)
  
  return(resultado)
}

# Función auxiliar para generar una tabla a partir de 
