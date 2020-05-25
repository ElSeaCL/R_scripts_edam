#################################################################################################################
# DATOS ESPESAMIENTO
# Por: Sebastián Gonzalez
# 17-02-2020
#################################################################################################################

# TODO:
# - No cargar automaticamente los distintos datos. Crear una función que permita llamar 
#   Al dataframe especifico de acuerdo a la necesidad.
# - Chequear que las librerías necesarias estén cargadas.

# Se debine como working directory la carpeta donde se encuentran los archivos
return = tryCatch(setwd("T:/PROCESOS/18. Seguimientos/Espesamiento/src"), error=function(e) return(1))
if (return==1) {
  try(setwd("//srv-fstbl/servidor/PROCESOS/18. Seguimientos/Espesamiento/src"),silent=FALSE)
}

#################################################################################################################
# FUNCIONES PRIVADAS
#################################################################################################################

.load_workbook_esp <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "Comparativo espesamiento.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.espesamiento")) {
    wb.espesamiento <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Espesamiento/src/Comparativo espesamiento 2020.xlsx")
    assign("wb.espesamiento", wb.espesamiento, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.espesamiento)) {
    read.xlsx(wb.espesamiento,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}

.load_clasificacion_esp <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "categoria_resultados.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.resultados.espesamiento")) {
    wb.resultados.espesamiento <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Espesamiento/src/categoria_resultados_esp.xlsx")
    assign("wb.resultados.espesamiento", wb.resultados.espesamiento, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.resultados.espesamiento)) {
    read.xlsx(wb.resultados.espesamiento,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}

.load_avisos_esp <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "categoria_resultados.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.avisos.espesamiento")) {
    wb.resultados.espesamiento <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Espesamiento/src/avisos_esp.xlsx")
    assign("wb.avisos.espesamiento", wb.resultados.espesamiento, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.avisos.espesamiento)) {
    read.xlsx(wb.avisos.espesamiento,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}

.load_workbook_dh <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "categoria_resultados.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.deshidratacion")) {
    wb.deshidratacion <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Deshidratacion/src/Comparativo deshidratacion 2020.xlsx")
    assign("wb.deshidratacion", wb.deshidratacion, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.deshidratacion)) {
    read.xlsx(wb.deshidratacion,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}

.load_clasificacion_dh <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "categoria_resultados.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.resultados.deshidratacion")) {
    wb.resultados.deshidratacion <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Deshidratacion/src/categoria_resultados_dh.xlsx")
    assign("wb.resultados.deshidratacion", wb.resultados.deshidratacion, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.resultados.deshidratacion)) {
    read.xlsx(wb.resultados.deshidratacion,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}

.load_avisos_dh <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "categoria_resultados.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.avisos.deshidratacion")) {
    wb.resultados.espesamiento <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Deshidratacion/src/avisos_dh.xlsx")
    assign("wb.avisos.deshidratacion", wb.resultados.espesamiento, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.avisos.deshidratacion)) {
    read.xlsx(wb.avisos.deshidratacion,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}


.load_workbook_pol <- function(x) {
  # Carga el workbook deseado perteneciente a
  # "categoria_resultados.xlsx"
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  if (!exists("wb.polimero")) {
    wb.polimero <- 
      loadWorkbook("T:/PROCESOS/18. Seguimientos/Polimero/src/Comparativo polimero 2020.xlsx")
    assign("wb.polimero", wb.polimero, envir = .GlobalEnv)
  }
  
  if (x %in% names(wb.polimero)) {
    read.xlsx(wb.polimero,
              sheet=x,
              colNames = TRUE,
              skipEmptyRows = TRUE,
              skipEmptyCols= TRUE,
              detectDates = TRUE
    )
  } else {
    print("Sheet not found")
  }
}

#################################################################################################################
# FUNCIONES PUBLICAS
#################################################################################################################

#################################################################################################################
## ESPESAMIENTO
#################################################################################################################

return_df_esp <- function() {
  # Carga el worksheet "centrifugas" perteneciente a
  # "Comparativo espesamiento.xlsx"
  #
  # Arguments:
  #  None
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro, y transformado en
  #  el tipo de datos esperado.
  centrifugas <- .load_workbook_esp("centrifugas")
  diccionario <- .load_workbook_esp("diccionario")
  
  colnames(centrifugas) <- diccionario$varR
  
  centrifugas <- centrifugas[,1:23]
  
  for (i in c(6:7, 9:13, 16:23)) {
    
    if (is.character(centrifugas[,i])) {
      centrifugas[, i] <- as.numeric(centrifugas[,i])
    }
  }
  
  return(centrifugas)
}

return_clasificacion_esp <- function() {
  # Carga el worksheet "Resultados" perteneciente a
  # "categoria_resultados.xlsx".
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  resultados <- .load_clasificacion_esp("Resultados")
  
  resultados$ano <- as.numeric(resultados$ano)
  resultados$semana <- as.numeric(resultados$semana)
  resultados$centrifuga <- as.character(resultados$centrifuga)
  resultados$resultado <- as.character(resultados$resultado)
  resultados$total <- as.numeric(resultados$total)
  
  return(resultados)
}

return_avisos_esp <- function() {
  resultados <- .load_avisos_esp("avisos")
  return(resultados)
}

return_df_pre_in <- function() {
  resultados <- .load_workbook_esp("pre_esp_ingreso")
  
  resultados$SST <- as.numeric(resultados$SST)
  resultados$MV <- as.numeric(resultados$MV)
  
  return(resultados)
  
}

return_df_pre_out <- function() {
  resultados <- .load_workbook_esp("pre_esp_salida")
  
  resultados$indice_marcha <- as.numeric(resultados$indice_marcha)
  resultados$manto <- as.numeric(resultados$manto)
  resultados$MS <- as.numeric(resultados$MS)
  
  return(resultados)
  
}

return_df_cent_in <- function() {
  resultados <- .load_workbook_esp("cent_in")
  resultados$MSscada1 <- as.numeric(resultados$MSscada1)
  resultados$MSlab <- as.numeric(resultados$MSlab)
  resultados$MVlab <- as.numeric(resultados$MVlab)
  return(resultados)
}

return_df_cambi <- function() {
  resultados <- .load_workbook_esp("cambi_prod")
  
  resultados$ciclos <- as.numeric(resultados$ciclos)
  
  return(resultados)
  
}

#################################################################################################################
## DESHIDRATACIÓN
#################################################################################################################

return_df_dh <- function() {
  # Carga el worksheet "deseado"centrifugas" perteneciente a
  # "Comparativo deshidratacion.xlsx"
  #
  # Arguments:
  #  None
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro formateada con los
  #  tipos de dato listo para ser utilizada.
  
  centrifugas <- .load_workbook_dh("centrifugas")
  diccionario <- .load_workbook_dh("diccionario")
  
  colnames(centrifugas) <- diccionario$varR
  
  #for (i in 1:length(colnames(centrifugas))){
  #  colnames(centrifugas)[i] <- diccionario$VarR[i]
  #}
  
  for (i in c(5:9, 12:16, 19:23)) {
    
    if (is.character(centrifugas[,i])) {
      centrifugas[, i] <- as.numeric(centrifugas[,i])
    }
  }
  
  return(centrifugas)
}

return_df_dig <- function() {
  # Carga el worksheet "deseado"centrifugas" perteneciente a
  # "Comparativo deshidratacion.xlsx"
  #
  # Arguments:
  #  None
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro formateada con los
  #  tipos de dato listo para ser utilizada.
  
  df <- .load_workbook_dh("digestion")
  
  df$ms_690 <- as.numeric(df$ms_690)
  df$porc_lb_690 <- as.numeric(df$porc_lb_690)
  df$porc_lp_690 <- as.numeric(df$porc_lp_690)

  return(df)
}

return_clasificacion_dh <- function() {
  # Carga el worksheet "Resultados" perteneciente a
  # "categoria_resultados.xlsx".
  #
  # Arguments:
  #  x: Nombre del libro que se quiere cargar.
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro.
  resultados <- .load_clasificacion_dh("Resultados")
  
  resultados$ano <- as.numeric(resultados$ano)
  resultados$semana <- as.numeric(resultados$semana)
  resultados$centrifuga <- as.character(resultados$centrifuga)
  resultados$resultado <- as.character(resultados$resultado)
  resultados$total <- as.numeric(resultados$total)
  
  return(resultados)
}

return_avisos_dh <- function() {
  resultados <- .load_avisos_dh("avisos")
  return(resultados)
}

#################################################################################################################
## POLIMERO
#################################################################################################################

.load_workbook_pol

return_df_pol <- function(){
  # Carga el worksheet "stock_polimero" perteneciente a
  # "Comparativo polimero 2020.xlsx"
  #
  # Arguments:
  #  None
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro, y transformado en
  #  el tipo de datos esperado.
  polimero <- .load_workbook_pol("stock_polimero")
  
  polimero$proceso <- factor(polimero$proceso)
  polimero$polimero <- factor(polimero$polimero)
  return(polimero)
}

return_pol_scada <- function(){
  # Carga el worksheet "preparacion_polimero" perteneciente a
  # "Comparativo polimero 2020.xlsx"
  #
  # Arguments:
  #  None
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro, y transformado en
  #  el tipo de datos esperado.
  polimero <- .load_workbook_pol("preparacion_polimero")
  
  polimero$unidad <- factor(polimero$unidad)
  polimero$marca <- factor(polimero$marca)
  polimero$polvo <- as.numeric(polimero$polvo)
  polimero$agua <- as.numeric(polimero$agua)
  
  polimero <- polimero %>%
    mutate(agua = if_else(agua < 0, 0, agua))
  
  return(polimero)
}

return_pol_lab <- function(){
  # Carga el worksheet "analisis_laboratorio" perteneciente a
  # "Comparativo polimero 2020.xlsx"
  #
  # Arguments:
  #  None
  #
  # Returns:
  #  Data Frame con la información obtenida en el libro, y transformado en
  #  el tipo de datos esperado.
  polimero <- .load_workbook_pol("analisis_polimero")
  
  polimero$unidad <- factor(polimero$unidad)
  polimero$noche <- as.numeric(polimero$noche)
  polimero$manana <- as.numeric(polimero$manana)
  polimero$tarde <- as.numeric(polimero$tarde)
  
  polimero <- polimero %>%
    tidyr::gather(`noche`, `manana`, `tarde`, key="horario", value="concentracion") %>%
    mutate(horario = factor(horario))
  
  return(polimero)
}

return_comp_pol <- function(df_scada, df_lab) {
  
  .tmp_prom_scada <- df_scada %>%
    mutate(concentracion = polvo/agua) %>%
    mutate(concentracion = if_else(polvo == 0, 0, concentracion),
           concentracion = if_else(is.na(concentracion), lag(concentracion, 1), concentracion)) %>%
    group_by(unidad) %>%
    mutate(conc_mov_scada = (lag(concentracion,0)+lag(concentracion,1)+lag(concentracion,2)+lag(concentracion,3)+lag(concentracion,4)+lag(concentracion,5)+lag(concentracion,6))/7
    ) %>%
    ungroup(unidad)
  
  .tmp_prom_lab <- df_lab %>%
    group_by(fecha, unidad) %>%
    summarise(conc_mean = mean(concentracion),
              conc_sd = sd(concentracion)
    ) %>%
    ungroup(fecha, unidad) %>%
    group_by(unidad) %>%
    mutate(conc_mov_lab = (lag(conc_mean,0)+lag(conc_mean,1)+lag(conc_mean,2)+lag(conc_mean,3)+lag(conc_mean,4)+lag(conc_mean,5)+lag(conc_mean,6))/7
    ) %>%
    ungroup(unidad)
  
  .tmp_final <- inner_join(.tmp_prom_scada, .tmp_prom_lab) %>%
    select(fecha, unidad, conc_mov_scada, conc_mean) %>%
    transmute(fecha = fecha,
              unidad = unidad,
              lab = conc_mean,
              scada = conc_mov_scada) %>%
    gather(`scada`, `lab`, key="origen", value="concentracion")
  
  return(.tmp_final)
}

return_stock_dia <- function(dfStock, proc, year){
  
  .tmp_stock_dia <- dfStock %>%
    filter(proceso == proc) %>%
    mutate(rango_fecha = as.numeric(fecha_termino - fecha_inicio + 1)) %>%
    mutate(dia_mes = day(fecha_inicio)) %>%
    group_by(fecha_inicio, fecha_termino) %>%
    summarise(consumo = sum(cantidad),
              rango_fecha = mean(rango_fecha)) %>%
    mutate(mes = month(fecha_inicio),
           dia_mes = day(fecha_inicio)) %>%
    group_by(mes) %>%
    mutate(consumo_rango = if_else(dia_mes == 1, consumo, lag(consumo, 0) - lag(consumo, 1)),
           consumo_dia = consumo_rango / rango_fecha) %>%
    filter(year(fecha_inicio) == year) %>%
    ungroup(mes)
  
  return(.tmp_stock_dia)
  
}


#################################################################################################################
## GENERALES
#################################################################################################################

clasificador_semanal <- function(df_centrifugas, year, week, cent){
  # Se procede a comprobar que hayan muestras para la centrífuga en esa semana.
  # Se cuentan los casos en los que se cumplen...
  resultados <- c(0,0,0,0)
  
  .tmp_data <- df_centrifugas %>%
    dplyr::filter(indice == 1,
                  ano == year,
                  sem == week,
                  centrifuga == cent)
  
  # en caos de que no hayan resultados para esta semana
  if (nrow(.tmp_data) != 0){
    for(i in 1:nrow(.tmp_data)){
      
      var <- clasificar_resultado(.tmp_data$sequedad[i],
                                  .tmp_data$tasaCaptura[i],
                                  .tmp_data$direccion[i]
                                  )
      resultados <- resultados + var
    }
  }
  
  return(resultados)
}

add_resultados <- function(df_resultados, resultados, year, week, cent) {
  # Se escriben los datos dentro del data frame
  ## Se crea datatable temporal para comprobar si ya se han ingresado datos para la centrifuga y la semana
  .tmp_empty <- df_resultados %>%
    filter(centrifuga == cent, semana == week, ano == year)
  
  if (nrow(.tmp_empty) == 0) {
    # Si se encuentra que la tabla temporal está vacia se agregan los datos
    df_resultados <- df_resultados %>%
      add_row(ano = year, semana = week, centrifuga = cent, resultado = "positivo", total = resultados[1]) %>%
      add_row(ano = year, semana = week, centrifuga = cent, resultado = "exceso", total = resultados[2]) %>%
      add_row(ano = year, semana = week, centrifuga = cent, resultado = "medio", total = resultados[3]) %>%
      add_row(ano = year, semana = week, centrifuga = cent, resultado = "negativo", total = resultados[4])
    # Si se encuentra que ya hay datos, se procede a actualizarlos
  } else {
    # Sería lindo si pudiera hacer esto con dplyr...pero no se meocurre como
    print("Los datos analizados ya fueron ingresados, se procede a actualizar")
    df_resultados[
      which(df_resultados$ano == year & df_resultados$semana == week & df_resultados$centrifuga == cent),5][1] <- resultados[1]
    
    df_resultados[
      which(df_resultados$ano == year & df_resultados$semana == week & df_resultados$centrifuga == cent),5][2] <- resultados[2]
    
    df_resultados[
      which(df_resultados$ano == year & df_resultados$semana == week & df_resultados$centrifuga == cent),5][3] <- resultados[3]
    
    df_resultados[
      which(df_resultados$ano == year & df_resultados$semana == semana & df_resultados$centrifuga == cent),5][4] <- resultados[4]
  }
  return(df_resultados)
}

return_prom_movil <- function(dfCentrifugas, filter=NULL){
  
  filter <- stringi::stri_trans_general(filter, id="Latin-ASCII")
  
  .tmp_esp_st <- dfCentrifugas %>%
    filter(direccion == filter) %>%
    mutate(ratio_lodo = ratio * flujoMS,
           seq_lodo = sequedad * flujoMS,
           tc_lodo = tasaCaptura * flujoMS) %>%
    group_by(fecha) %>%
    summarise(ratio_mean = sum(ratio_lodo) / sum(flujoMS),
              seq_mean = sum(seq_lodo) / sum(flujoMS),
              tc_mean = sum(tc_lodo) / sum(flujoMS),
              ratio_sd = sd(ratio),
              seq_sd = sd(sequedad),
              tc_sd = sd(tasaCaptura)) %>%
    ungroup(fecha) %>%
    mutate(ratio_prom = (lag(ratio_mean,0)+lag(ratio_mean,1)+lag(ratio_mean,2)+lag(ratio_mean,3)+lag(ratio_mean,4)+lag(ratio_mean,5)+lag(ratio_mean,6))/7,
           seq_prom = (lag(seq_mean,0)+lag(seq_mean,1)+lag(seq_mean,2)+lag(seq_mean,3)+lag(seq_mean,4)+lag(seq_mean,5)+lag(seq_mean,6))/7,
           tc_prom = (lag(tc_mean,0)+lag(tc_mean,1)+lag(tc_mean,2)+lag(tc_mean,3)+lag(tc_mean,4)+lag(tc_mean,5)+lag(tc_mean,6))/7
    )
  
  return(.tmp_esp_st)
}

return_prom_pre <- function(df_pre, filter=NULL){
  
  .tmp_pre <- df_pre %>%
    mutate(MSlab_prom = (lag(MSlab,0)+lag(MSlab,1)+lag(MSlab,2)+lag(MSlab,3)+lag(MSlab,4)+lag(MSlab,5)+lag(MSlab,6))/7,
           MScalc_prom = (lag(MScalc,0)+lag(MScalc,1)+lag(MScalc,2)+lag(MScalc,3)+lag(MScalc,4)+lag(MScalc,5)+lag(MScalc,6))/7,
           MVlab_prom = (lag(MVlab,0)+lag(MVlab,1)+lag(MVlab,2)+lag(MVlab,3)+lag(MVlab,4)+lag(MVlab,5)+lag(MVlab,6))/7,
           MVlab_prom = MVlab_prom * 100,
           MSscada_prom = (lag(MSscada1,0)+lag(MSscada1,1)+lag(MSscada1,2)+lag(MSscada1,3)+lag(MSscada1,4)+lag(MSscada1,5)+lag(MSscada1,6))/7
    )
  
  return(.tmp_pre)
}

return_prom_dig <- function(df_dig, filter=NULL){
  
  .tmp_dig <- df_dig %>%
    mutate(caudal_total = caudal_690 + caudal_anillo + caudal_1690,
           ms_total = (caudal_690 * ms_690 + caudal_anillo * ms_anillo + caudal_1690 * ms_1690) 
           / caudal_total,
           mv_total = (caudal_690 * ms_690 * mv_690 + 
                         caudal_anillo * ms_anillo * mv_anillo + 
                         caudal_1690 * ms_1690 * mv_anillo) 
           / (caudal_total * ms_total)
           ) %>%
    mutate(MS_prom = (lag(ms_total,0)+lag(ms_total,1)+lag(ms_total,2)+lag(ms_total,3)+lag(ms_total,4)+lag(ms_total,5)+lag(ms_total,6))/7,
           MV_prom = (lag(mv_total,0)+lag(mv_total,1)+lag(mv_total,2)+lag(mv_total,3)+lag(mv_total,4)+lag(mv_total,5)+lag(mv_total,6))*100/7
           )
  
  return(.tmp_dig)
}


######################################################333
# OJO: Al PARECER NO UTILIZO ESTA FUNCION, BORRAR
#########################################################  
return_horas_op <- function(dfCentrifuga, semana) {
  .tmp_df <- .tmp_esp %>%
    mutate(centrifuga = factor(centrifuga)) %>%
    group_by(centrifuga, sem) %>%
    summarise(horas = sum(horas)) %>%
    filter(sem == 18)
  return(.tmp_df)
}
#########################################################
print("Funciones cargadas")



