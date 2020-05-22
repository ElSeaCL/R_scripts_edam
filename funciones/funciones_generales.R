#################################################################################################################
# FUNCIONES GENERALES
# Por: Sebastián Gonzalez
# 03-004-2020
#################################################################################################################

#################################################################################################################
# FUNCIONES OPCULTAS
#################################################################################################################

# Funciones para categorizar los resultados por centrífuga
## Resultados de sequedad y tasa de captura positivos
.resultado_positivo <- function (sequedad, tasaCaptura, sequedad_min, sequedad_max,  tasaCaptura_min) {
  return(as.numeric(sequedad >= sequedad_min & sequedad < sequedad_max & tasaCaptura >= tasaCaptura_min)) 
}

## Resultados de la sequedad o tasa de captura positivos
.resultado_medio <- function(sequedad, tasaCaptura, sequedad_min, sequedad_max,  tasaCaptura_min) {
  return(as.numeric(xor(tasaCaptura > tasaCaptura_min & sequedad < sequedad_min, 
                        sequedad >= sequedad_min & sequedad < sequedad_max & tasaCaptura < tasaCaptura_min)
  ))
}

## Resultados de la quedad y tasa de captura negativos
.resultado_negativo <- function(sequedad, tasaCaptura, sequedad_min, sequedad_max, tasaCaptura_min) {
  return(as.numeric(xor(sequedad < sequedad_min, sequedad >= sequedad_max) & 
                      tasaCaptura < tasaCaptura_min))
}

.resultado_exceso <- function(sequedad, tasaCaptura, sequedad_min, sequedad_max,  tasaCaptura_min) {
  return(as.numeric(sequedad > sequedad_max & tasaCaptura > tasaCaptura_min))
}

#################################################################################################################
# FUNCIONES PÚBLICAS
#################################################################################################################

# operador notin
`%notin%` <- Negate(`%in%`)

cargar_librerias <- function(...) {
  #' Instala/actuliza/carga los paquetes a utilizar
  #' 
  #' `cargar_librerias` toma como argumento los nombres en formato caracter
  #' de las librerias que deseo utilizar, revisa si ya se encuentra instalado,
  #' si durante la sesión ya se realizó una actualización de las librerias, y 
  #' una vez que esto se cumple, carga la librería.
  #' 
  #' @param ... es character del nombre de la librería a utilizar
  
  #TODO: Realizar la verificación tomando en cuenta la version del paquete y actualizar en caso de ser 
  #TODO: Veríficar las dependencias de las librerias.
  
  package_req <- list(...)
  package_list <- installed.packages()[,1]
  
  # Actualizar los paquetes
  if (!(exists(".val"))) {
    update.packages(ask = FALSE)
    .val <- 1
    assign(".val", .val, envir = .GlobalEnv)
  }
  
  for (i in seq(package_req)){
    
    # Comprobar si la librería ya está cargada
    if (package_req[i] %in% (.packages())){
      next
    }
    
    # Comprobar si la librería ya está instalada. Si no es así
    # se procede a instalarla.
    if (!(package_req[i] %in% package_list)){
      install.packages(as.character(package_req[i]), character.only = TRUE )
    }
    
    # Se carga la libreria
    require(as.character(package_req[i]), character.only = TRUE)
  }
}

obtener_rangos <- function(str){
  #' Rangos de aceptables de resultados en centrífugas.
  #' 
  #' `obtener_rangos` retorna los rangos de operación de sequedad y tasa de
  #' captura para los distintos casos de operación de las centrifugas.
  #' 
  #' Esta función retorna una lista de 4 números, cada unp representa
  #' un límite de operación establecido previamente para las centrífugas.
  #' Los primeros dos elementos corresponde a los mínimo y máximo de sequedad,
  #' mientras que los dos ultimos son los límites de tasa de captura.
  #' El parámtero de ingreso `str` respresanta el uso que se le da a la centrífuga y
  #' debe encontrarse entre las posibles opciones definidas en como casos.
  #' 
  #' `cases <- c("CAMBI", "690", "1690", "Deshidratacion")`
  #' 
  #' En caso de que `str` no se encuentre en la lista anterior retorna un mensaje 
  #' de error.
  #' 
  #' @param str is a string
  #' 
  cases <- c("CAMBI", "690", "1690", "Deshidratacion")
  str <- stringi::stri_trans_general(str, id = "Latin-ASCII")
  
  if(str %notin% cases){
    return(cat("Caso de operación no existente, ingresar caso válido: \n
                \'CAMBI\', \'690\', \'1690\', \'Deshidratación\'."))
  }
  
  case <- match(str, cases)
  
  switch(case,
         return(limites = c(15, 17, 90, 100)),
         return(limites = c(4, 6, 90, 100)),
         return(limites = c(4, 6, 90, 100)),
         return(limites = c(27, 29, 90, 100))
         )
}

clasificar_resultado <- function(sequedad, tasaCaptura, direccion) {
  #' Califica los resultados de una centrífuga según alineación.
  #' 
  #' `clasificar_resultado` toma como argumento la sequedad y la tasa de 
  #' captura obtenida como resultado de una centrífuga y, dependiendo
  #' de la dirección de operación, lo clasifica como un resultado positivo si
  #' cumple con ambas muestras, como medio si cumple con una, negativo si no
  #' cumple con ninguno y exceso si cumple la tasa de captura y excede la
  #' sequedad máxima.
  #' 
  #' @param sequedad es numeric.
  #' @param tasaCaptura es numeric.
  #' @param dirección es string opción de `obtener_rangos`.
  #'

  limites <- obtener_rangos(direccion)
  sequedad_min <- limites[1]
  sequedad_max <- limites[2]
  tasaCaptura_min <- limites[3]
  tasaCaptura_max <- limites[4]
  
  # vector resultados
  posivo   <- .resultado_positivo(sequedad, tasaCaptura, sequedad_min, sequedad_max, tasaCaptura_min)
  exceso   <- .resultado_exceso(sequedad, tasaCaptura, sequedad_min, sequedad_max, tasaCaptura_min)
  medio    <- .resultado_medio(sequedad, tasaCaptura, sequedad_min, sequedad_max, tasaCaptura_min)
  negativo <- .resultado_negativo(sequedad, tasaCaptura, sequedad_min, sequedad_max, tasaCaptura_min)
  
  resultado <- c(posivo, exceso, medio, negativo)
  
  return(resultado)
}

# Crea un directorio para almacenar las figuras
asigna_wd <- function(path, nombreCarpeta) {
  #' Crea y luego asigna el directorio ingresado como directorio de trabajo.
  #' 
  #' `asgina_wd` toma como argumento un directorio y una el nombre a asignar
  #' para la nueva capeta a crear. Una vez creada se asigna como working
  #' directory.
  #' 
  #' @param path es un caracter de un directorio completo ya existente.
  #' @param nombreCarpeta es el nombre de la carpeta a crear.
  #' 
  #' TODO: Verificar que le path inicial exista.
  #' TODO: En caso de que exista pero tiene un error de tipeo imprimir sugerencia.
  
  # Carpeta a verificar
  list_dir <- strsplit(nombreCarpeta, "/")
  
  for (carpeta in list_dir[[1]]){
    
    full_path <- paste0(path, "/", carpeta)
    
    # Revisa si ya existe el directorio, en caso de que no sea así se crea
    if (!(carpeta %in% list.files(path))) {
      dir.create(full_path)
    } else {
      print(paste("Directorio ", full_path, " ya existía"))
    }
    path <- full_path
  }
  setwd(full_path)
}




