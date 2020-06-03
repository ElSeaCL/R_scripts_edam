#################################################################################################################
# FUNCIONES GENERALES
# Por: Sebastián Gonzalez
# 19-02-2020
#################################################################################################################

#################################################################################################################
# TODO
#################################################################################################################
# 1.- Finalizar la función de tasa de captura vs sequedad. Crearla generica sin asignación de intervalo
# 2.- Crear función de captura vs. sequedad para 1 semana..
# 3.- Incluir título de la figura.

#################################################################################################################
# FUNCIONES
#################################################################################################################
#################################################################################################################
# FIGURAS CATEGORIZACIÓN
#################################################################################################################

figura_cap_vs_seq <- function(sequedad, tasa_captura, ratio, direccion, tema=NULL) {
  
  #TODO: comparar si los distintos argumentos tienen el mismo largo.
  
  # definición de los limites para el fill. Limites figura 
  min_ratio <- min(ratio)
  max_ratio <- max(ratio)
  limit_fill = c(floor(min_ratio), ceiling(max_ratio))
  
  # Definición de los limites. Se toma como referente la alineación del primer día de la semana.
  # Este referente se utiliza solo para las gráficas. el cálculo de clasificción se usa por dato.
  limites <- obtener_rangos(direccion)
  seqmin <- limites[1]
  seqmax <- limites[2]
  capmin <- limites[3]
  capmax <- limites[4]
  
  limite_seq <- c(min(sequedad - 1, seqmin - 1),
                  max(c(sequedad + 1,seqmax + 1)))
  
  limite_cap <- c(max(min(tasa_captura - 5, capmin - 5), 0),
                  min(max(tasa_captura + 5, capmax), 100))
  
  tmp_df <- data.frame(sequedad, tasa_captura, ratio)
  
  temp_plot <- tmp_df %>%
    ggplot(aes(x = sequedad, y = tasa_captura)) +
    geom_rect(aes(xmin = seqmin, xmax = seqmax, ymin = capmin, ymax = capmax), fill = "turquoise", alpha = 0.05 ) +
    geom_point(aes(color = ratio), shape = 19, size = 5) +
    labs(x = "Sequedad obtenida (%)", y = "Tasa de Captura (%)") +
    scale_color_gradient(low = "green", high = "red", limits = limit_fill) +
    scale_x_continuous(limits = limite_seq) +
    scale_y_continuous(limits = limite_cap) +
    annotate("text", x = 15, y = 91, label = paste("ratio prom. =", round(mean(ratio), digits = 2)))
  
  
  return(temp_plot)
}

fig_capseq_sem <- function(df_centrifugas, 
                           destino, 
                           cent,
                           year,
                           week, 
                           tema=NULL) {
  
  destino_plot <- destino
  destino <- stringi::stri_trans_general(destino, id="Latin-ASCII")
  
  # Filtro los datos por la centrifuga
  tmp_datos <- df_centrifugas %>%
    dplyr::filter(centrifuga == cent,
                   direccion == destino,
                   ano == year,
                   sem == week)
  
  if (nrow(tmp_datos) == 0){
    print(paste0("no se encuentran datos de la centrífuga ", cent, " en la semana ", week))
    return()
  }
   
  # Se define el destino final de la centrífuga pra agregarlo al titulo de la figura
  if (destino == "Deshidratacion"){
    tipo_cent <- destino_plot
  } else{
    tipo_cent <- "Espesamiento"
  }
  
  # Variable se usará para el título de la figura
  archivo_png <- paste(c("Semana", semana, " Cent. ", cent, " ", tipo_cent), sep = "", collapse = "")
    
  # La función figura_Cap_vs_seq genera la figura la cual se graba en tmp_plot
  tmp_plot <- 
    figura_cap_vs_seq(tmp_datos$sequedad, 
                      tmp_datos$tasaCaptura, 
                      tmp_datos$ratio, 
                      destino, 
                      tema=NULL)
  
  # Se le agrega titulo
  tmp_plot <- tmp_plot + ggtitle(archivo_png)
  
  return(tmp_plot)
}

fig_clasificacion <- function(datosResultados, year, week, tipo_centrifuga){
  
  figura_temp <- datosResultados %>%
    filter(total != 0,
           semana == week,
           ano == year) %>%
    ggplot(aes(x=centrifuga, y=total, fill=resultado)) +
    geom_bar(stat="identity", position="stack") +
    labs(title= paste0("Clasificaci\u00F3n semana ", week, " "),
            subtitle = tipo_centrifuga)
  
  return(figura_temp)
  
}

fig_clasificacion_anual <- function(datosResultados, year, semana_inicio,semana_termino, tipo_centrifuga) {
  
  #semana_inicio <- lubridate::isoweek(today()) - delta_semana
  #semana_final <- lubridate::isoweek(today()) - 1
  
  figura_temp <- datosResultados %>%
    filter(ano == year,
           semana >= semana_inicio,
           semana <= semana_termino) %>%
    group_by(semana, resultado) %>%
    summarise(total = sum(total)) %>%
    ggplot() +
    geom_col(aes(x=semana, y=total, fill=resultado), position="stack") +
    scale_x_continuous(breaks = seq(semana_inicio, semana_termino, 2)) +
    labs(x= "Semana", y= "Total") +
    labs(title= paste0("Clasificaci\u00F3n semanal "),
         subtitle= tipo_centrifuga)
  
  return(figura_temp)
}

#################################################################################################################
# PRE_ESPESADORES
#################################################################################################################

fig_carga_pre <- function(df_pre, fecha_in, fecha_out, intervalo) {
  
  .tmp_df <- return_carga_pre(df_pre, fech_in, fecha_out, intervalo)
  
  titulo <- labs(subtitle = intervalo)
  axis_y <- labs(y="Carga Entrada (ton)")
  
  if (intervalo == "Diario") {
    
    .tmp_plot <- .tmp_df %>%
      ggplot() +
      geom_line(aes(x=fecha, y=carga_prom), color = "Blue") +
      geom_point(aes(x=fecha, y=carga_in), color = "Red") +
      labs(x="Fecha") +
      axis_y +
      titulo
    
    return(.tmp_plot)
    
  } else if (intervalo == "Semanal") {
    
    .tmp_plot <- .tmp_df %>% 
      ggplot() +
      geom_col(aes(x=ano_sem, y=carga_tot), fill = "red4") +
      labs(x="Semana") +
      axis_y +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      titulo
    
    return(.tmp_plot)
    
  } else if (intervalo == "Mensual") {
    
    .tmp_plot <- .tmp_df %>%
      ggplot() +
      geom_col(aes(x=ano_mes, y=carga_tot), fill="green4") +
      labs(x="Mes") +
      axis_y +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      titulo
    
    return(.tmp_plot)
    
  } else {
    
    print("Opcion no valida")
    
  }
}

fig_dif_carga_pre <- function(df_in, df_out, fecha_in, fecha_out, intervalo, color="red4") {
  
  if (intervalo == "Diario") {
    plot_x <- "fecha"
    x_lab <- labs(x="Fecha")
    theme_x <- NULL
  } else if (intervalo == "Semanal") {
    plot_x <- "ano_sem"
    x_lab <- labs(x="Semanal")
    theme_x <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    plot_x <- "ano_mes"
    x_lab <- labs(x="Mes")
    theme_x <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  carga_dif <- "carga_dif"
  
  y_axis <- labs(y="Carga (ton)")
  subtitle <- labs(subtitle = intervalo)
  
  .tmp_in <- return_carga_pre(.tmp_pre_in, fech_in, fecha_out, intervalo)
  .tmp_out <- return_carga_pre(.tmp_pre_out, fech_in, fecha_out, intervalo)
  
  inner_join(.tmp_in, .tmp_out, by = plot_x) %>%
    mutate(carga_in = carga_tot.x,
           carga_out = carga_tot.y,
           carga_dif = carga_in - carga_out) %>%
    ggplot() +
    geom_bar(aes_string(x=plot_x, y=carga_dif), fill=color, stat="identity") +
    x_lab + theme_x + y_axis + subtitle
  
}


#################################################################################################################
# SERIES TIEMPO ENTRADA CENTRIFUGAS
#################################################################################################################

fig_prom_in <- function(dfCentrifuga, atributo, year, week_inicio, week_termino) {
  
  # Atributo puede ser ratio, sequedad o promedio movil
  atributo <- toupper(atributo)
  
  # Se verifica si la semana inicial es mayor a la final
  # esto significa que estamos lidiando con datos del año pasado
  if (week_inicio > week_termino) {
    year_inicio = year - 1
  } else {
    year_inicio = year
  }
  
  # Transformación de semana-año a fecha
  week_inicio <- week_inicio - 1
  
  if (week_inicio == 0) {
    
    week_inicio <- 52
    year_inicio <- year_inicio - 1
    
  } else if (week_inicio < 10) {
    week_inicio <- paste0("0", week_inicio)
    }
  
  inicio <- paste0(year_inicio, week_inicio, 1)
  dia_inicio <- as.Date(inicio, "%Y%W%u")
  
  if (week_termino == 53){
    year <- year + 1
    for(i in 1:7){
      
      termino <- paste0(year, "01", i)
      dia_termino <- as.Date(termino, "%Y%W%u")
      
      if (!(is.na(as.character(dia_termino)))) {
        break
      }
    }
  } else {
    if (week_termino < 10) {
      week_termino <- paste0("0", week_termino)
    }
    fin <- paste0(year, week_termino, 7)
    dia_termino <- as.Date(fin, "%Y%W%u")
  }
  
  fecha <- "fecha"
  Blue <- "Blue"
  Red <- "Red"
  
  if (atributo == "MS") {
    atributo1 <- "MSlab_prom"
    atributo2 <- "MSscada_prom"
    line <- geom_line(aes_string(x= fecha, y=atributo2, color=Red), size=1)
    axis <- labs(x="Fecha", y="MS(g/L)")
    
    
  } else {
    atributo1 <- "MVlab_prom"
    atributo2 <- NULL
    axis <- labs(x="Fecha", y="MV(%)")
    line <- NULL
  }
  
  .tmp_df <- dfCentrifuga %>%
    filter(fecha >= dia_inicio,
           fecha <= dia_termino) %>%
    drop_na(!!atributo1, !!atributo2)
  
  limits_y <- c(min(.tmp_df[,atributo1]) -5, max(.tmp_df[,atributo1]) +5)
  
  .tmp_plot <- ggplot(.tmp_df) +
    geom_line(aes_string(x=fecha, y= atributo1, color=Blue), size=1) +
    line +
    scale_color_manual(name = "Origen", values = c("Blue" = "Blue", "Red" = "Red"), labels = c("Lab", "SCADA")) +
    scale_y_continuous(limits = limits_y) +
    labs(x="Fecha", y="Concentraci\u00F3n (kg/ton)") +
    labs(title = "Concentraci\u00F3n Lodo Pre-Esp a Centr\u00EDfugas", subtitle = "SCADA vs. Lab") +
    #theme(legend.position = "none") +
    axis
  
  return(.tmp_plot)
  
}

fig_prom_dig <- function(dfCentrifuga, atributo, year, week_inicio, week_termino) {
  
  # Atributo puede ser ratio, sequedad o promedio movil
  atributo <- toupper(atributo)
  
  # Se verifica si la semana inicial es mayor a la final
  # esto significa que estamos lidiando con datos del año pasado
  if (week_inicio > week_termino) {
    year_inicio = year - 1
  } else {
    year_inicio = year
  }
  
  # Transformación de semana-año a fecha
  week_inicio <- week_inicio - 1
  
  if (week_inicio == 0) {
    
    week_inicio <- 52
    year_inicio <- year_inicio - 1
    
  } else if (week_inicio < 10) {
    week_inicio <- paste0("0", week_inicio)
  }
  
  inicio <- paste0(year_inicio, week_inicio, 1)
  dia_inicio <- as.Date(inicio, "%Y%W%u")
  
  if (week_termino == 53){
    year <- year + 1
    for(i in 1:7){
      
      termino <- paste0(year, "01", i)
      dia_termino <- as.Date(termino, "%Y%W%u")
      
      if (!(is.na(as.character(dia_termino)))) {
        break
      }
    }
  } else {
    if (week_termino < 10) {
      week_termino <- paste0("0", week_termino)
    }
    fin <- paste0(year, week_termino, 7)
    dia_termino <- as.Date(fin, "%Y%W%u")
  }
  
  fecha <- "fecha"
  Blue <- "Blue"
  Red <- "Red"
  
  if (atributo == "MS") {
    atributo1 <- "MS_prom"
    axis <- labs(x="Fecha", y="MS(g/L)")
    
    
  } else {
    atributo1 <- "MV_prom"
    axis <- labs(x="Fecha", y="MV(%)")
    line <- NULL
  }
  
  .tmp_df <- dfCentrifuga %>%
    filter(fecha >= dia_inicio,
           fecha <= dia_termino) %>%
    drop_na(!!atributo1)
  
  limits_y <- c(min(.tmp_df[,atributo1]) -5, max(.tmp_df[,atributo1]) +5)
  
  .tmp_plot <- ggplot(.tmp_df) +
    geom_line(aes_string(x=fecha, y= atributo1, color=Blue), size=1) +
    scale_color_manual(name = "Origen", values = c("Blue" = "Blue"), labels = c("Lab")) +
    scale_y_continuous(limits = limits_y) +
    labs(x="Fecha", y="Concentraci\u00F3n (kg/ton)") +
    labs(title = "Concentraci\u00F3n Lodo Digerido a Centr\u00EDfugas") +
    #theme(legend.position = "none") +
    axis
  
  return(.tmp_plot)
  
}

#################################################################################################################
# SERIES TIEMPO RESULTADOS
#################################################################################################################

fig_prom_movil <- function(dfCentrifugas, atributo, year, week_inicio, week_termino) {
  
  # Atributo puede ser ratio, sequedad o promedio movil
  atributo <- toupper(atributo)
  
  # Se verifica si la semana inicial es mayor a la final
  # esto significa que estamos lidiando con datos del año pasado
  if (week_inicio > week_termino) {
    year_inicio = year - 1
  } else {
    year_inicio = year
  }
  
  # Transformación de semana-año a fecha
  week_inicio <- week_inicio - 1
  
  if (week_inicio == 0) {
    week_inicio <- 52
    year_inicio <- year_inicio - 1
    
  } else {
    if (week_inicio < 10) {
      week_inicio <- paste0("0", week_inicio)
    }
  }
  inicio <- paste0(year_inicio, week_inicio, 1)
  dia_inicio <- as.Date(inicio, "%Y%W%u")
  
  
  if (week_termino == 53){
    year <- year + 1
    for(i in 1:7){
      
      termino <- paste0(year, "01", i)
      dia_termino <- as.Date(termino, "%Y%W%u")
      
      if (!(is.na(as.character(dia_termino)))) {
        break
      }
    }
  } else {
    if (week_termino < 10) {
      week_termino <- paste0("0", week_termino)
    }
    fin <- paste0(year, week_termino, 7)
    dia_termino <- as.Date(fin, "%Y%W%u")
  }
  
  if(atributo == "RATIO"){
    atributo <- "ratio"
    axis <- labs(x="Fecha", y="Ratio (kg/ton)")
    title <- labs(title="Ratios ponderados", subtitle = "Puntual y promedio movil")
  } else if(atributo == "SEQUEDAD") {
    atributo <- "seq"
    axis <- labs(x="Fecha", y="Sequedad (%)")
    title <- labs(title="Sequedad ponderada", subtitle = "Puntual y promedio movil")
  } else if(atributo == "TASA DE CAPTURA") {
    atributo <- "tc"
    axis <- labs(x="Fecha", y="Tasa de Captura (%)")
    title <- labs(title="Tasa de Captura ponderada", subtitle = "Puntual y promedio movil")
  } else {
    print("Opción no valida.")
    print("Atributo debe ser 'ratio', 'sequedad' o 'Tasa de Captura'.")
    return(NULL)
  }
  
  atributo_prom <- paste0(atributo,"_prom")
  atributo_mean <- paste0(atributo, "_mean")
  fecha <- "fecha"
  
  .tmp_df <- dfCentrifugas %>%
    filter(fecha >= dia_inicio,
           fecha <= dia_termino) %>%
    drop_na(!!atributo_prom)
  
  limits_y <- c(min(.tmp_df[,atributo_prom]) -1, max(.tmp_df[,atributo_prom]) +1)
  
  .tmp_plot <- ggplot(.tmp_df) +
    geom_line(aes_string(x=fecha, y= atributo_prom), size=1, color="Blue") +
    geom_point(aes_string(x= fecha, y=atributo_mean), color="Red") +
    scale_y_continuous(limits = limits_y) +
    labs(x="Fecha", y="Ratio (kg/ton)") +
    theme(legend.position = "none") +
    axis +
    title

  return(.tmp_plot)
}

#################################################################################################################
# FIGURAS HORAS DE OPERACIÓN
#################################################################################################################

fig_horas_semana <- function(dfEspesamiento, week, year) {
  .tmp_df <- dfEspesamiento %>%
    filter(ano == year) %>%
    mutate(horas = if_else(indice == 0, 0, horas),
           centrifuga = factor(centrifuga)) %>%
    group_by(centrifuga, sem) %>%
    summarise(horas = sum(horas)) %>%
    filter(sem == week)
  
  .mean_value <- mean(.tmp_df$horas)
  
  .tmp_plot <- .tmp_df %>%
    ggplot() +
    geom_bar(aes(x=centrifuga, y=horas), stat="identity", position="stack", fill="deepskyblue4") +
    coord_flip() +
    geom_hline(aes(yintercept=.mean_value), size=1, color="firebrick") +
    labs(x="Centr\u00EDfuga", y="Horas") +
    labs(title=paste0("Horas de operaci\u00F3n semana ", week),
         subtitle = "Total Centr\u00EDfuga")
  
  return(.tmp_plot)
}

fig_cum_horas <- function(dfCentrifuga, year, week_inicio, week_termino){
  
  # Se verifica si la semana inicial es mayor a la final
  # esto significa que estamos lidiando con datos del año pasado
  if (week_inicio > week_termino) {
    year_inicio = year - 1
  } else {
    year_inicio = year
  }
  
  # Transformación de semana-año a fecha
  week_inicio <- week_inicio - 1
  
  if (week_inicio == 0) {
    week_inicio <- 53
    year_inicio <- year_inicio - 1
  } else {
    if (week_inicio < 10) {
      week_inicio <- paste0("0", week_inicio)
    }
  }
  inicio <- paste0(year_inicio, week_inicio, 1)
  dia_inicio <- as.Date(inicio, "%Y%W%u")
  
  
  if (week_termino == 53){
    year <- year + 1
    for(i in 1:7){
      
      termino <- paste0(year, "01", i)
      dia_termino <- as.Date(termino, "%Y%W%u")
      
      if (!(is.na(as.character(dia_termino)))) {
        break
      }
    }
  } else {
    if (week_termino < 10) {
      week_termino <- paste0("0", week_termino)
    }
    fin <- paste0(year, week_termino, 7)
    dia_termino <- as.Date(fin, "%Y%W%u")
  }
  
  
  .tmp_plot <- dfCentrifuga %>%
    filter(fecha >= dia_inicio,
           fecha < dia_termino) %>%
    mutate(horas = if_else(indice == 0, 0, horas),
           centrifuga = factor(centrifuga)) %>%
    group_by(centrifuga) %>%
    mutate(cum_horas = cumsum(horas)) %>%
    ggplot() +
    geom_line(aes(x=fecha, y=cum_horas, color = centrifuga), size =1) +
    labs(x="Fecha", y="Hora de operaci\u00F3n") +
    labs(title = "Horas totales de operaci\u00F3n por centr\u00EDfuga",
         subtitle = paste0("Periodo del ", dia_inicio, " al ", dia_termino))
  
  return(.tmp_plot)
}

#################################################################################################################
# PARAMETROS DE OPERACIÓN, CARGA, TORQUE, VR
#################################################################################################################


fig_operacion_cent <- function(dfCentrifuga, year, week, destino){
  
  destino_plot <- destino
  destino <- stringi::stri_trans_general(destino, id="Latin-ASCII")
  
  .tmp_df <- dfCentrifuga %>%
    drop_na(torque,vr) %>%
    filter(ano == year,
           sem == week,
           direccion == destino)
  
  if(nrow(.tmp_df) == 0){
    return(1)
  }
  
  .tmp_df <- .tmp_df %>%
    mutate(centrifuga = factor(centrifuga)) %>%
    group_by(centrifuga) %>%
    summarise(vr_mean = mean(vr),
              torque_mean = mean(torque),
              vr_sd = sd(vr),
              torque_sd = sd(torque)
              ) %>%
    ungroup(centrifuga)
  
  .tmp_plot <- .tmp_df %>%
    ggplot() +
    geom_errorbar(mapping=aes(x=torque_mean, ymin=vr_mean - vr_sd/2, ymax=vr_mean + vr_sd/2)) +
    geom_errorbarh(mapping=aes(y= vr_mean, xmin=torque_mean - torque_sd/2, xmax= torque_mean + torque_sd/2)) +
    geom_point(data=.tmp_df, aes(x=torque_mean, y=vr_mean, color=centrifuga), size=2) +
    labs(x="Torque Promedio (%)", y="VR Promedio (rpm)") +
    labs(title=paste0("Operaci\u00F3n centr\u00EDfugas ", destino_plot),
         subtitle= paste0("Semana ", week))
         
    return(.tmp_plot)
}

fig_box_carga <- function(dfCentrifuga, year, week, destino){
  # carga de operación
  
  destino_plot <- destino
  destino <- stringi::stri_trans_general(destino, id="Latin-ASCII")
  
  .tmp_plot <- dfCentrifuga %>%
    filter(direccion == destino,
           ano == year,
           sem == week) %>%
    mutate(centrifuga = factor(centrifuga)) %>%
    mutate(carga_horaria = flujoMS / horas) %>%
    ggplot() +
    geom_boxplot(aes(x=centrifuga, y=carga_horaria)) +
    labs(x="Centr\u00EDfuga", y="Carga horaria (kg/h") +
    labs(title=paste0("carga centr\u00EDfugas ", destino_plot),
         subtitle= paste0("Semana ", week)
    )
  
  return(.tmp_plot)
}

fig_box_caudal <- function(dfCentrifuga, year, week, destino){
  
  destino_plot <- destino
  destino <- stringi::stri_trans_general(destino, id="Latin-ASCII")
  
  # carga de operación
  .tmp_plot <- dfCentrifuga %>%
    filter(direccion == destino,
           ano == year,
           sem == week) %>%
    mutate(centrifuga = factor(centrifuga)) %>%
    mutate(caudal_horaria = caudalLodo / horas) %>%
    ggplot() +
    geom_boxplot(aes(x=centrifuga, y=caudal_horaria)) +
    labs(x="Centr\u00EDfuga", y="Caudal horario (m3/h") +
    labs(title=paste0("Caudal Centr\u00EDfugas ", destino_plot),
         subtitle= paste0("Semana ", week)
    )
  
  return(.tmp_plot)
}

#################################################################################################################
# FIGURAS AVISOS 
#################################################################################################################

fig_avisos_sem <- function(dfCentrifuga, n.ano, n.semana){
  
  n.ano <- as.character(n.ano)
  n.semana <- as.character(n.semana)
  
  .tmp_df <- dfCentrifuga %>%
    mutate(semana = factor(week(Fecha.de.aviso), ordered = TRUE),
           ano = factor(year(Fecha.de.aviso), ordered = TRUE)
    ) %>%
    filter(as.character(semana) %in% n.semana,
           as.character(ano) %in% n.ano) %>%
    group_by(semana, ano) %>%
    summarise(num.avisos= n()) %>%
    ungroup(semana, ano)
  
  .tmp_plot <- .tmp_df %>%
    ggplot() +
    geom_col(aes(x=semana, y=num.avisos, fill=ano), position="dodge2") +
    labs(x="Semana", y = "N\u00FAmero de Avisos")
  
  return(.tmp_plot)
}

fig_avisos_area <- function(dfCentrifuga, n.ano, n.semana){
  
  n.ano <- as.character(n.ano)
  n.semana <- as.character(n.semana)
  
  .tmp_df <- dfCentrifuga %>%
    mutate(semana = factor(week(Fecha.de.aviso), ordered = TRUE),
           area = factor(Pto.tbjo.resp., ordered = FALSE),
           ano = factor(year(Fecha.de.aviso), ordered = TRUE)
    ) %>%
    filter(as.character(semana) %in% n.semana,
           as.character(ano) %in% n.ano) %>%
    group_by(semana, ano, area) %>%
    summarise(num.avisos= n()) %>%
    ungroup(semana, ano, area)
  
  .tmp_plot <- .tmp_df %>%
    ggplot() +
    geom_col(mapping = aes(x=ano, y=num.avisos, fill=area), position = "fill") +
    labs(x = "A\u00F1o", y = "N\u00FAmero de avisos") +
    coord_flip()
  
  return(.tmp_plot)
}

fig_top_avisos <- function(dfCentrifuga, n.ano, n.semana, n.top){
  
  n.ano <- as.character(n.ano)
  n.semana <- as.character(n.semana)
  
  .tmp_df <- dfCentrifuga %>%
    mutate(ano = factor(year(Fecha.de.aviso), ordered = TRUE),
           mes = factor(month(Fecha.de.aviso), ordered = TRUE),
           sem = factor(isoweek(Fecha.de.aviso), ordered = TRUE)) %>%
    filter(as.character(ano) %in% n.ano,
           as.character(sem) %in% n.semana) %>%
    group_by(mes, ano, Equipo) %>%
    summarise(num.avisos = n()) %>%
    ungroup(Equipo, ano, mes) %>%
    group_by(mes) %>%
    top_n(n.top, num.avisos)
  
  .tmp_df %>%
    ggplot() +
    geom_col(aes(x=mes, y=num.avisos, fill=Equipo), position="dodge") +
    labs(x="Mes", y="N\u00FAmero de Avisos") +
    labs(paste0("Top ", n.top, " equipos por n\u00Fmero de aviso"))
  
}

#################################################################################################################
# POLIMERO
#################################################################################################################

fig_comparativa_pol <- function(df_comparacion, equipo, fecha_inicio, fecha_termino) {
  
  .tmp_plot <- df_comparacion %>%
    filter(fecha >= date(fecha_inicio),
           fecha <= date(fecha_termino),
           unidad == equipo) %>%
    ggplot() +
    geom_line(aes(fecha, concentracion, color=origen), size = 1) +
    labs(x="Fecha", y="Concentraci\u00F3n (g/L)") +
    labs(title = paste0("Concentraci\u00F3n lab/SCADA unidad ", equipo),
         subtitle = "Promedio movil 7 d\u00EDas")
  
  return(.tmp_plot)
}

fig_scada_stock <- function(dfscada, dfstock, proceso, year){
  
  proceso_plot <- proceso
  #proceso <- stringi::stri_trans_general(proceso_plot, id="Latin-ASCII")
  
  .tmp_stock <- return_stock_dia(dfstock, proceso_plot, year)
  
  dfscada %>%
    filter(indice == 1) %>%
    mutate(polvo = ratio * flujoMS / 1000) %>%
    group_by(fecha) %>%
    summarise(polvo = sum(polvo)) %>%
    filter(year(fecha) == year) %>%
    ggplot() +
    geom_line(aes(x=fecha, y=polvo, color="firebrick4"), size=1) +
    geom_segment(.tmp_stock, mapping=aes(x=fecha_inicio, xend=fecha_termino, y=consumo_dia, yend=consumo_dia, color="forestgreen"), size=1) +
    labs(x= "Fecha", y= "Consumo (kg/día)") +
    labs(title=paste0("Consumo diar\u00EDo pol\u00EDmero en polvo ", proceso_plot), subtitle="SCADA vs. Stock") +
    scale_color_manual(name ="Origen",values = c("firebrick4" = "firebrick4", "forestgreen" = "forestgreen"), 
                       labels =c("SCADA", "Stock"))
  
}

fig_objetivo_anual <- function(dfCent, dfStock, proceso, year, objetivo){
  
  proceso_plot <- proceso
  #proceso <- stringi::stri_trans_general(proceso_plot, id="Latin-ASCII")
  
  .tmp_carga <- dfCent %>%
    filter(ano == year) %>%
    group_by(fecha) %>%
    summarise(flujoMS = sum(flujoMS)) %>%
    ungroup(fecha) %>%
    mutate(cum_MS = cumsum(flujoMS))
  
  .tmp_stock <- return_stock_dia(dfStock, proc = proceso, year = year) %>%
    mutate(consumo_total = cumsum(consumo_rango))
  
  .tmp_plot <- inner_join(.tmp_carga, .tmp_stock, by=c("fecha" = "fecha_termino")) %>%
    mutate(ratio = consumo_total / cum_MS * 1000) %>%
    ggplot() +
    geom_line(aes(x=fecha, y=ratio, color="forestgreen"), size=1) +
    geom_hline(aes(yintercept=objetivo, color="firebrick"), size=1) +
    scale_color_manual(name="Ratio", values=c("forestgreen" = "forestgreen", "firebrick" = "firebrick"),
                       labels =c("Obj", "Stock")) +
    labs(x="Fecha", y="Ratio anual (kg/ton)") +
    labs(title=paste0("Evoluci\u00F3n ratio anual stock ", proceso_plot, " ", year))
  
 return(.tmp_plot)
}


fig_polimero_mes <- function(dfCent, dfStock, proceso, year, objetivo){
  
  proceso_plot <- proceso
  #proceso <- stringi:stri_trans_general(proceso_plot, id="Latin-ASCII")
  
  .tmp_mes_stock <- return_stock_dia(dfStock, proceso, year) %>%
    mutate(ano_mes = factor(paste0(year(fecha_inicio), mes))) %>%
    group_by(ano_mes) %>%
    summarise(stock = sum(consumo_rango) / 1000) %>%
    mutate(mes = factor(substring(ano_mes, 5, 5)))
  
  .tmp_mes_scada <- dfCent %>%
    mutate(ano_mes = factor(paste0(ano, month(fecha))),
           consumo = flujoMS * ratio / 1000 / 1000) %>%
    filter(ano == year) %>%
    group_by(ano_mes) %>%
    summarise(scada = sum(consumo)) %>%
    mutate(mes = factor(substring(ano_mes, 5, 5)))
  
  .tmp_plot <- inner_join(.tmp_mes_scada, .tmp_mes_stock, by=("ano_mes")) %>%
    gather(`scada`, `stock`, key="origen", value="consumo") %>%
    mutate(origen = factor(origen)) %>%
    ggplot +
    geom_col(aes(x=mes.x, y=consumo, fill=origen), position = "dodge2") +
    scale_x_discrete(breaks = c("1", "2", "3", "4", "5"), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo")) +
    labs(x="Mes", y="Consumo Pol\u00EDmero (ton/mes)") +
    labs(title = paste0("Comparaci\u00F3n mensual consumo pol\u00EDmero"),
         subtitle = proceso_plot)
  
  return(.tmp_plot)
  
} 

