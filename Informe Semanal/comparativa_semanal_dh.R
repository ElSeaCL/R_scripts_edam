#################################################################################################################
# SCRIPT FIGURAS ESPESAMIENTO
# Por: Sebastián Gonzalez
# 17-02-2020
#################################################################################################################

#################################################################################################################
# TODO
#################################################################################################################

# 1.- Separar la creación de figuras de la actualización del excel de clasificación.

setwd("T:/PROCESOS/18. Seguimientos/Deshidratacion/Comparativa Deshidratacion/semanal")

#################################################################################################################
# LIBRERIAS USUARIO
#################################################################################################################

#Funciones básicas
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/funciones/funciones_generales.R")
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/funciones/funciones_graficas.R")
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/funciones/funciones_data.R")

#################################################################################################################
# LIBRERIAS R
#################################################################################################################

# Lista de paquetes installados
cargar_librerias("stringr"    # Manipulación de strings
                 ,"ggplot2"    # Paquete con funciones para generar gráficos 
                 ,"dplyr"      # Herramientas para realizar operaciones en dataframes
                 ,"lubridate"  # Herramientas de fecha y hora
                 ,"tidyr"      # Organizador de data tabular
                 ,"openxlsx"   # Manipulación de excel
                 ,"stringi"    # quita los acentos
                 )

rm(cargar_librerias)

#################################################################################################################
# CARGAR DATOS ESPESAMIENTO
#################################################################################################################

# Se retiran las funciones que no se utilizaran
rm("return_df_esp", 
   "return_clasificacion_esp", 
   "return_df_cambi", 
   "return_df_pre_in", 
   "return_df_pre_out")

df_deshidratacion <- return_df_dh()
df_clasif_dh <- return_clasificacion_dh()
df_avisos_dh <- return_avisos_dh()

# Se remoueven los workbooks que no se van a modificar
rm("wb.deshidratacion")

# Se remoueven las funciones que no se utilizarán
# rm(list=setdiff(lsf.str(), c("resultado_medio", "resultado_negativo", "resultado_positivo",
#                              "CreaAsignaWD",
#                              "RangosOperacionEsp",
#                              "resultados"
#                              )))

# remueve ese return que se genera...
rm("return")

#################################################################################################################
# GRAFICAS
#################################################################################################################
#################################################################################################################
# Tema impresión ggplot
#################################################################################################################

# Parametros a utilizar como tema para ajustar las impresiónes de las figuras
# a los tamaños indicados para mostrar la información de manera correcta
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/temas/tema_impresion.R")

#################################################################################################################
# PARÁMETROS Y DATOS PREVIOS
#################################################################################################################

# Semana de la cual se rescatarán los datos
tipo <- "Deshidratacion"
semana_inicio <- 9
semana_termino <- 19
year <- 2020

# Lista de centrífugas a graficar
centrifugas <- c("A", "B", "C", "D", "E", "F")

# Se tabaja con una df temporal para limpiar los datos de los indices de marcha 0
.tmp_data <- df_deshidratacion %>% dplyr::filter(indice == 1) %>% drop_na(sequedad, tasaCaptura)

#################################################################################################################
# LOOP FIGURAS
#################################################################################################################

for (semana in semana_inicio:semana_termino) {
  print(semana)
  
  # Crea un directorio para almacenar las figuras
  path <- paste0("T:/PROCESOS/18. Seguimientos/Deshidratacion/Comparativa Deshidratacion/semanal/", year)
  asigna_wd(path, paste0(semana,"/",tipo))
  
  for (cent in centrifugas) {
    
    #################################################################################################################
    # FIGURAS CATEGORIZACIÓN
    #################################################################################################################
    
    # Figuras muestran los datos de 1 sola semana
    # Figura tasa captura vs. sequedad por centrífuga
    archivo_png <- paste(c("Semana", semana, " Cent. ", cent, " Deshidratación"), sep = "", collapse = "")
    temp_plot <- fig_capseq_sem(.tmp_data, "Deshidratación", cent, year, semana)
    ggsave(temp_plot,file=paste(c(archivo_png, ".png"), sep = "", collapse = ""),
           width = 11.5, height = 5.5, units = "cm", dpi=320)
    rm(temp_plot)
    
    # Se agregan los resultados de la clasificación al df
    resultado_semanal <- clasificador_semanal(.tmp_data, year, semana, cent)
    df_clasif_dh <- add_resultados(df_clasif_dh, resultado_semanal, year, semana, cent)
    rm(resultado_semanal)
    
    # Genera geom_bar con la clasificación de las muestras
    temp_plot <- fig_clasificacion(df_clasif_dh, year, semana, "Deshidratación")
    ggsave(temp_plot, file=paste0("categoria_semana",semana, ".png"), 
           width = 9, height = 9, units = "cm", dpi=320)
    rm(temp_plot)
    
  }
  
  #################################################################################################################
  # SERIES TIEMPO RESULTADOS
  #################################################################################################################
  
  # Intervalo propio de la serie de tiempo. Siempre toma como base
  # la semana actual y le resta un numero que dejamos fijo como 7.
  # En caso de que la diferencia de negativo, se pasa a la semana del
  # año anterior.
  .tmp_semana_inicio <- semana - 7
  if (.tmp_semana_inicio <= 0) {
    .tmp_semana_inicio <- 53 + .tmp_semana_inicio
  }
  
  # Base con los promedios calculados
  .tmp_prom <- return_prom_movil(.tmp_data, filter="Deshidratación")
  
  # figuras de ratio, sequedad y tasa de captura
  temp_plot <- fig_prom_movil(.tmp_prom, "ratio", year, .tmp_semana_inicio, semana)
  ggsave(temp_plot,file=paste(c("ratio.png"), sep = "", collapse = ""),
         width = 15, height = 7.0, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_prom_movil(.tmp_prom, "sequedad", year, .tmp_semana_inicio, semana)
  ggsave(temp_plot,file=paste(c("sequedad.png"), sep = "", collapse = ""),
         width = 15, height = 7.0, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_prom_movil(.tmp_prom, "Tasa de Captura", year, .tmp_semana_inicio, semana)
  ggsave(temp_plot,file=paste(c("tasa_captura.png"), sep = "", collapse = ""),
         width = 15, height = 7.0, units = "cm", dpi=320)
  rm(temp_plot)
  rm(.tmp_prom)
  
  #################################################################################################################
  # FIGURAS HORAS DE OPERACIÓN
  #################################################################################################################
  #TODO: IMPUTAR LAS HORAS DE OPERACIÓN, EXISTEN CASOS EN LOS QUE LAS HORAS NO SON REALES. VALORES
  #      POR SOBRE 24HRS O VALORES 0/- CON CAUDAL DE OPERACIÓN.
  
  # Horas de operación semanal
  temp_plot <- fig_horas_semana(df_deshidratacion, semana, year)
  ggsave(temp_plot, file=paste0("horas_op_semana",semana, ".png"), 
         width = 9, height = 9, units = "cm", dpi=320)
  rm(temp_plot)
  
  # Horas de operación acumulativas compredidas en el periodo indicado
  temp_plot <- fig_cum_horas(df_deshidratacion, year, .tmp_semana_inicio, semana)
  ggsave(temp_plot, file=paste0("horas_op_periodo",semana, ".png"), 
         width = 15, height = 9, units = "cm", dpi=320)
  rm(temp_plot)
  
  #################################################################################################################
  # PARAMETROS DE OPERACIÓN, CARGA, TORQUE, VR
  #################################################################################################################
  
  # vr vs. torque
  .tmp_plot <- fig_operacion_cent(.tmp_data, year, semana, "Deshidratación")
    # Se comprueba si se encontraron datos de vr vs. torque, de no se así se salta el guardado
  if (!(is.na(class(.tmp_plot)[2]))){
    ggsave(.tmp_plot, file=paste0("operación_semana",semana, ".png"), 
           width = 15, height = 7, units = "cm", dpi=320)
  }
  rm(.tmp_plot)
  
  # carga operación
  .tmp_plot <- fig_box_carga(.tmp_data, year, semana, "Deshidratación")
  ggsave(.tmp_plot, file=paste0("carga_semana",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  # Caudal Operación
  .tmp_plot <- fig_box_caudal(.tmp_data, year, semana, "Deshidratación")
  ggsave(.tmp_plot, file=paste0("caudal_semana",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  #################################################################################################################
  # COMPARATIVA CAUDALÍMETROS
  #################################################################################################################
  
  # Alimentación a digestión vs Caudal de alimentación
  
  #################################################################################################################
  # FIGURAS AVISOS 
  #################################################################################################################
  
  ## cantidad de avisos por semana del año
  .tmp_plot <- fig_avisos_sem(df_avisos_dh, (year - 1):year, (semana - 8):semana)
  ggsave(.tmp_plot, file=paste0("avisos_semana ",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  ## cantidad de avisos por área de tratamiento
  .tmp_plot<- fig_avisos_area(df_avisos_dh, (year - 1):year, (semana - 8):semana)
  ggsave(.tmp_plot, file=paste0("avisos_area",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  ## cantidad de avisos por tipo de equipo.
  .tmp_plot <- fig_top_avisos(df_avisos_dh, year, (semana - 8):semana, 3)
  ggsave(.tmp_plot, file=paste0("avisos_equipo",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  #################################################################################################################
  # CLASIFICACIÓN ANUAL
  #################################################################################################################
  
  # Figura datos clasificación anuales
  temp_plot <- fig_clasificacion_anual(df_clasif_dh, year = year, 1, semana, tipo_centrifuga = "Deshidratación")
  ggsave(temp_plot, file=paste0("clasificacion_a_la_fecha.png"), 
         width = 15, height = 9, units = "cm", dpi=320)
  rm(temp_plot)
  
}
 
#################################################################################################################
# AlMACENA DATOS RESULTADO
#################################################################################################################

# Se graba el excel con los datos
setwd("T:/PROCESOS/18. Seguimientos/Deshidratacion/src")
openxlsx::writeData(wb.resultados.deshidratacion, "Resultados", df_clasif_dh)
openxlsx::saveWorkbook(wb.resultados.deshidratacion, "categoria_resultados_esp.xlsx", overwrite = TRUE)
