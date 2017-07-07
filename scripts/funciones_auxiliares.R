library("plyr")
library("dplyr")
library("stringi")
library("forcats") # Para trabajar con factores, en "genera_llave"

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

# Funcion auxiliar para pasar a minúsculas todos los nombres de las columnas de
# los df en una lista:
# lista_df: lista de data frames
# La función regresa una lista de data frames, por lo que se puede pipear.
renombra_columnas_minusculas <- function(lista_df){
  llply(lista_df, function(df){
    names(df) <- names(df) %>%
      tolower()
    return(df)
  })
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

# Función auxiliar a la hora de programar, para hacer una tabla para cada columna
# de un data frame, y así visualizar valores a corregir
# df: data frame cuyas valores en cada columna se quieren revisar
# La función regresa una lista de tablas con los valores de cada columna de df.
revisa_valores <- function(df){
  # Obteniendo nombres de las columnas en el data frame
  nombres_columnas <- colnames(df) %>%
    sort()
  
  # Asignando nombres para el llply:
  names(nombres_columnas) <- nombres_columnas
  
  # Generando una lista con tablas de valores, una para cada columna
  resultado <- llply(nombres_columnas, function(x){
    resultado <- df[[x]] %>%
      table(useNA = "always")
    return(resultado)
  })
  
  return(resultado)
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
    } else{
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

# Función para eliminar columnas vacías (de puros NA's) de un data frame
# df: dataframe a eliminar columnas vacías
# La función regresa el data frame sin columnas vacías
elimina_columnas_vacias <- function(df){
  
  # Obteniendo columnas a eliminar
  columnas_eliminar <- df %>%
    # Convirtiendo todas los valores de todas las columnas a TRUE si el valor
    # correspondiente es NA y FALSE e.o.c
    mutate_all(is.na) %>%
    # Calculando la cantidad de NA0s que tiene cada columa
    summarise_all(sum) %>%
    # Viendo si la columna tiene puros NA's, en dicho caso, el resultado será
    # TRUE (columna a eliminar)
    mutate_all(
      funs(. == nrow(df))
    ) %>%
    # Poniendo los nombres de las columnas en una misma variable
    gather("columna", "eliminar") %>%
    filter(eliminar) %>%
    # Generando expresión para el select_
    mutate(
      expresion = paste0("-", columna)
    ) %>%
    pull(expresion)
  
  # Eliminando dichas columnas del df
  if(length(columnas_eliminar) == 0)
    # Si no se debe eliminar ninguna columna
    resultado <- df
  else
    resultado <- df %>%
      select_(.dots = columnas_eliminar)
  
  return(resultado)
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
# df: data frame cuyas variables se quieren transformar
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

# Función auxiliar para transformar NA's en "" en columnas de tipo
# caracter.
# df: data frame de interés
# El resultado es un data frame con los NA's sustituídos por "" en columnas de
# tipo caracter.
# Nota: como es muy útil trabajar con NA's en R, esta transformación se recomienda
# realizarla hasta tener las tablas finales, listas para introducir en la base de
# datos.
cambia_na_strings_vacios <- function(df){
  resultado <- df %>%
    mutate_if(
      is.character, funs(ifelse(is.na(.), "", .))
    )
  return(resultado)
}

# Función auxiliar para generar una llave numérica a partir de una llave natural:
# df: data frame al que se le agregará una columna con una llave numérica
# a partir de una llave natural (definida a partir de una o varias columnas
# que contiene). Es decir, dos registros tendrán el mismo número si y sólo si
# tienen los mismos valores en las columnas de la llave natural.
# nombre_columna_llave: nombre de la columna que contendrá la llave numérica
# ...: nombre de las variables que definen la llave natural.
# El resultado es el df con la llave generada como una nueva columna.
# Si no se pasa argumentos extras a la función (en ...), la llave generada
# será simplemente una cuenta de todos los renglones.
genera_llave <- function(df, nombre_columna_llave, ...){
  
  nombres_columnas_llave_natural <- c(...)
  
  if(length(nombres_columnas_llave_natural) > 0){
    
    # Generando la expresión para ordenar primero el df por los factores, ya
    # que estos se generarán en orden de aparición (forcats::as_factor())
    expresion_arrange_ = as.list(nombres_columnas_llave_natural)
    
    # Generando la expresión para el mutate_:
    expresion_mutate_ = paste0(
      "paste0(",
      paste0(".data$", nombres_columnas_llave_natural) %>%
        paste0(collapse = ", \"_\", "),
      ") ",
      # forcats::as_factor() crea los factores en el órden de aparición, sin ordenar
      # primero (lo cuál causaba problemas de "11" < "2" al usar as.factor())
      "%>% as_factor() %>% as.numeric()"
    ) %>%
      as.list()
    
    # Asignando el nombre de la columna que contendrá la llave numérica a la expresión
    # porque el mutate_ lo requiere
    names(expresion_mutate_) <- nombre_columna_llave
    
    resultado <- df %>%
      # generando una columna para poder regresar el data frame a su orden original
      # después de generar las llaves
      mutate(
        orden_aux_funcion_genera_llave = 1:nrow(.)
        ) %>%
      arrange_(.dots = expresion_arrange_) %>%
      mutate_(.dots = expresion_mutate_) %>%
      arrange(orden_aux_funcion_genera_llave) %>%
      select(-orden_aux_funcion_genera_llave)
    
  } else{
    # Si no se pasan columnas que especifiquen una llave natural:
    
    expresion_mutate_ = list("1:nrow(.)")
    names(expresion_mutate_) <- nombre_columna_llave
    
    resultado <- df %>%
      mutate_(.dots = expresion_mutate_)
  }
  
  return(resultado)
}

# Función auxiliar para generar una tabla a partir de una columna de llave 
# y una lista nombrada de columnas adicionales, para calcular el valor de cada
# columna adicional correspondiente a un valor de "nombre_columna_llave" se
# utilizará el primer valor encontrado.
# df: df que contiene la columna "nombre_columna_llave" y las columnas a incluir en
# la nueva tabla.
# nombre_columna_llave: nombre de la columna que será una llave de la tabla nueva.
# nombre_nuevo_columna_llave: el nombre que tendrá la llave en el nuevo data frame
# lista_columnas_adicionales: lista nombrada de columnas adicionales a incluir
# en la tabla nueva.
# Para cada elemento en la lista, el "nombre" corresponde al nombre de la columna
# en el data frame nuevo, y el valor al nombre de ésta en el df anterior..
# La función regresa la tabla generada con las especificaciones anteriores
genera_tabla <- function(df, nombre_columna_llave, nombre_nuevo_columna_llave,
  lista_columnas_adicionales){
  
  # Generando la expresión para el group_by:
  expresion_group_by_ <- nombre_columna_llave %>%
    as.list()
  
  # Generando la expresión para el rename (de la columna de la llave)
  expresion_rename_ <- list(nombre_columna_llave)
  names(expresion_rename_) <- nombre_nuevo_columna_llave

  # Hay dos casos distintos: si lista_columnas_adicionales es vacía o no:
  if(length(lista_columnas_adicionales) > 0){
    
    # Generando la expresión para el summarise:
    expresion_summarise_ <- paste0("first(.data$",
      lista_columnas_adicionales, ")")
        
    # Asignando nombres de variables en la nueva tabla:
    names(expresion_summarise_) <- names(lista_columnas_adicionales)
    
    # Generando resultado
    resultado <- df %>%
      group_by_(.dots = expresion_group_by_) %>%
      summarise_(
        .dots = expresion_summarise_
      ) %>%
      ungroup() %>%
      rename_(
        .dots = expresion_rename_
      )
    
  } else{
    # Generando resultado en el caso de "lista_columnas_adicionales" sea vacía,
    # en este caso, la tabla tendrá como única columna "nombre_columna_llave"
    resultado <- df %>%
      group_by_(.dots = expresion_group_by_) %>%
      summarise() %>%
      ungroup() %>%
      rename_(
        .dots = expresion_rename_
      )
  }
  
  return(resultado)
}

###### Funciones auxiliares sobre vectores

# Función para que, dado un vector de strings (frases), capitalice la primera
# letra de cada palabra de cada una de sus entradas, y las otras letras las minimiza.
# vec: Vector cuyas entradas son strings (frases)
# La función regresa vec con cada palabra en cada entrada capitalizada apropiadamente.
# Además, quita espacios en blanco al inicio y término de cada string
simple_cap <- function(vec){
  
  # Quitando NA's para que la función funcione como se espera, pero guardando
  # sus posiciones para volver a insertarlas al finalizar la función:
  posicion_NAs <- is.na(vec)
  vec[posicion_NAs] <- ""
  
  lista_palabras_por_string <- vec %>%
    # quitando espacios en blanco
    stri_trim_both() %>%
    # cortando cada entrada del vector por palabras
    stri_split_coll(pattern = " ")
  
  # Capitalizando cada letra de inicio de cada palabra y minimizando las otras
  resultado <- llply(lista_palabras_por_string, function(x){
    
    # Capitalizando primera letra de cada palabra de x
    primera_letra_palabras <- stri_sub(x, from = 1, to = 1) %>%
      toupper()
    # Minimizando las otras letras
    otras_letras <- stri_sub(x, from = 2) %>%
      tolower()
    # Formando de nuevo las palabras
    palabras <- paste0(primera_letra_palabras, otras_letras)
    # Formando de nuevo el string
    resultado <- paste(palabras, collapse = " ")
    return(resultado)
  }) %>%
    as.character()
  
  # Sustituyendo NA's de nuevo
  resultado[posicion_NAs] <- NA
  return(resultado)
}

# Función auxiliar para estandarizar lo más posible strings escritos de manera
# distinta: dado un vector de strings, a cada una de sus entradas la función le
# aplica lo siguiente:
# 1. lo pasa a minúsculas.
# 2. Le elimina espacios vacíos al principio y al final.
# 3. Le quita acentos a vocales y cambia "ñ" por "ni".
# 4. Cambia: comas, puntos, dos puntos, puntos y comas, diagonales, símbolos de
# interrogación, admiración, paréntesis, guiones altos y espacio vacíos por guiones
# bajos.
# 5. Después del procedimiento anterior, si encuentra dos o más guines bajos pegados
# los colapsa en uno solo
# vec: vector de strings a estandarizar
# La función regresa un vector de la misma longitud que vec, pero habiéndole
# aplicado el procedimiento anterior.
estandariza_strings <- function(vec){
  resultado <- vec %>%
    tolower() %>%
    stri_trim_both() %>%
    stri_replace_all_coll(
      c("á","é","í","ó","ú","ñ",",",".",":",";","/","¿","?","¡","!","(",")","-"," "),
      c("a","e","i","o","u","ni","_","_","_","_","_","_","_","_","_","_","_","_","_"),
      vectorize_all = FALSE) %>%
    stri_replace_all_regex("_+", "_")
  
  return(resultado)
}