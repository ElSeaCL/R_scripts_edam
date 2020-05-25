#################################################################################################################
# SCRIPT FIGURAS ESPESAMIENTO
# Por: Sebastián Gonzalez
# 17-02-2020
#################################################################################################################

#################################################################################################################
# TODO
#################################################################################################################

# 1.- Separar la creación de figuras de la actualización del excel de clasificación.

setwd("T:/PROCESOS/18. Seguimientos/Espesamiento/Comparativa Espesamiento/semanal")

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
                )

rm(cargar_librerias)

#################################################################################################################
# CARGAR DATOS ESPESAMIENTO
#################################################################################################################

# Se retiran las funciones que no se utilizaran
rm("return_df_dh", "return_clasificacion_dh")

df_espesamiento <- return_df_esp()
df_clasif_esp <- return_clasificacion_esp()
df_avisos_esp <- return_avisos_esp()
df_pre_in <- return_df_pre_in()
df_pre_out <- return_df_pre_out()
df_cent_in <- return_df_cent_in()
df_cambi <- return_df_cambi()

# Se remoueven los workbooks que no se van a modificar
rm("wb.espesamiento")

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
tipo <- "Espesamiento"
semana_inicio <- 20
semana_termino <- 21
year <- 2020

# Lista de centrífugas a graficar
centrifugas <- c("A", "B", "C", "D", "E", "F", "G", "H")

# Se tabaja con una df temporal para limpiar los datos de los indices de marcha 0
.tmp_esp <- df_espesamiento %>% dplyr::filter(indice == 1) %>% drop_na(sequedad, tasaCaptura)

#################################################################################################################
# LOOP FIGURAS
#################################################################################################################

for (semana in semana_inicio:semana_termino) {
  print(semana)
  
  # Crea un directorio para almacenar las figuras
  path <- paste0("T:/PROCESOS/18. Seguimientos/Espesamiento/Comparativa Espesamiento/semanal/figuras/", year)
  asigna_wd(path, paste0(semana))
  
  for (cent in centrifugas) {
    
#################################################################################################################
# FIGURAS CATEGORIZACIÓN
#################################################################################################################
    
    # Figura tasa captura vs. sequedad por centrífuga
    archivo_png <- paste(c("Semana", semana, " Cent. ", cent, " CAMBI"), sep = "", collapse = "")
    temp_plot <- fig_capseq_sem(.tmp_esp, "CAMBI", cent, year, semana)
    ggsave(temp_plot,file=paste(c(archivo_png, ".png"), sep = "", collapse = ""),
           width = 11.5, height = 5.5, units = "cm", dpi=320)
    rm(temp_plot)

    # Se agregan los resultados de la clasificación al df
    resultado_semanal <- clasificador_semanal(.tmp_esp, year, semana, cent)
    df_clasif_esp <- add_resultados(df_clasif_esp, resultado_semanal, year, semana, cent)
    rm(resultado_semanal)
    
    # Genera geom_bar con la clasificación de las muestras
    temp_plot <- fig_clasificacion(df_clasif_esp, year, semana, "Espesamiento")
    ggsave(temp_plot, file=paste0("categoria_semana",semana, ".png"), 
           width = 9, height = 9, units = "cm", dpi=320)
    rm(temp_plot)
    
      }
  
  #################################################################################################################
  # SERIES TIEMPO PRE-ESPESADORES
  #################################################################################################################
  
  # Intervalo propio de la serie de tiempo. Siempre toma como base
  # la semana actual y le resta un numero que dejamos fijo como 7.
  # En caso de que la diferencia de negativo, se pasa a la semana del
  # año anterior.
  .tmp_semana_inicio <- semana - 7
  if (.tmp_semana_inicio <= 0) {
    .tmp_semana_inicio <- 53 + .tmp_semana_inicio
  }
  
  # Base con los promedios moviles
  .tmp_prom <- return_prom_pre(df_cent_in)
  
  # figura comparativa de MS
  temp_plot <- fig_prom_in(.tmp_prom, "MS", year, .tmp_semana_inicio, semana_termino)
  ggsave(temp_plot,file=paste(c("MS.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  # Evolución del MV
  temp_plot <- fig_prom_in(.tmp_prom, "MV", year, .tmp_semana_inicio, semana_termino)
  ggsave(temp_plot,file=paste(c("MV.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  #################################################################################################################
  # SERIES TIEMPO RESULTADOS
  #################################################################################################################
  
  # Base con los promedios calculados
  .tmp_prom <- return_prom_movil(.tmp_esp, filter="CAMBI")
  
  # figuras de ratio, sequedad y tasa de captura
  temp_plot <- fig_prom_movil(.tmp_prom, "ratio", year, .tmp_semana_inicio, semana_termino)
  ggsave(temp_plot,file=paste(c("ratio.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_prom_movil(.tmp_prom, "sequedad", year, .tmp_semana_inicio, semana_termino)
  ggsave(temp_plot,file=paste(c("sequedad.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_prom_movil(.tmp_prom, "Tasa de Captura", year, .tmp_semana_inicio, semana_termino)
  ggsave(temp_plot,file=paste(c("tasa_captura.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  rm(.tmp_prom)
  
  #################################################################################################################
  # FIGURAS HORAS DE OPERACIÓN
  #################################################################################################################
  #TODO: IMPUTAR LAS HORAS DE OPERACIÓN, EXISTEN CASOS EN LOS QUE LAS HORAS NO SON REALES. VALORES
  #      POR SOBRE 24HRS O VALORES 0/- CON CAUDAL DE OPERACIÓN.
  
  # Horas de operación semanal
  temp_plot <- fig_horas_semana(df_espesamiento, semana, year)
  ggsave(temp_plot, file=paste0("horas_op_semana",semana, ".png"), 
         width = 9, height = 9, units = "cm", dpi=320)
  rm(temp_plot)
  
  # Horas de operación acumulativas compredidas en el periodo indicado
  temp_plot <- fig_cum_horas(df_espesamiento, year, .tmp_semana_inicio, semana)
  ggsave(temp_plot, file=paste0("horas_op_periodo",semana, ".png"), 
         width = 15, height = 9, units = "cm", dpi=320)
  rm(temp_plot)
  
  #################################################################################################################
  # PARAMETROS DE OPERACIÓN, CARGA, TORQUE, VR
  #################################################################################################################
  
  # vr vs. torque
  .tmp_plot <- fig_operacion_cent(.tmp_esp, year, semana, "CAMBI")
  # Se comprueba si se encontraron datos de vr vs. torque, de no se así se salta el guardado
  if (!(is.na(class(.tmp_plot)[2]))){
    ggsave(.tmp_plot, file=paste0("operación_semana",semana, ".png"), 
           width = 15, height = 7, units = "cm", dpi=320)
  }
  rm(.tmp_plot)
  
  # carga operación
  .tmp_plot <- fig_box_carga(.tmp_esp, year, semana, "CAMBI")
  ggsave(.tmp_plot, file=paste0("carga_semana",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  #################################################################################################################
  # COMPARATIVA CAUDALÍMETROS
  #################################################################################################################
  
  
  #################################################################################################################
  # FIGURAS AVISOS 
  #################################################################################################################
  
  ## cantidad de avisos por semana del año
  .tmp_plot <- fig_avisos_sem(df_avisos_esp, (year - 1):year, (semana - 8):semana)
  ggsave(.tmp_plot, file=paste0("avisos_semana ",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  ## cantidad de avisos por área de tratamiento
  .tmp_plot<- fig_avisos_area(df_avisos_esp, (year - 1):year, (semana - 8):semana)
  ggsave(.tmp_plot, file=paste0("avisos_area",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  ## cantidad de avisos por tipo de equipo.
  .tmp_plot <- fig_top_avisos(df_avisos_esp, year, (semana - 8):semana, 3)
  ggsave(.tmp_plot, file=paste0("avisos_equipo",semana, ".png"), 
         width = 15, height = 7, units = "cm", dpi=320)
  rm(.tmp_plot)
  
  #################################################################################################################
  # CLASIFICACIÓN ANUAL
  #################################################################################################################
  
  # Figura datos clasificación anuales
  temp_plot <- fig_clasificacion_anual(df_clasif_esp, year = year, 1, semana, tipo_centrifuga = "Espesamiento")
  ggsave(temp_plot, file=paste0("clasificacion_a_la_fecha.png"), 
         width = 15, height = 9, units = "cm", dpi=320)
  rm(temp_plot)
}

#################################################################################################################
# AlMACENA DATOS RESULTADO
#################################################################################################################

# Se graba el excel con los datos
setwd("T:/PROCESOS/18. Seguimientos/Espesamiento/src")
openxlsx::writeData(wb.resultados.espesamiento, "Resultados", df_clasif_esp)
openxlsx::saveWorkbook(wb.resultados.espesamiento, "categoria_resultados_esp.xlsx", overwrite = TRUE)


