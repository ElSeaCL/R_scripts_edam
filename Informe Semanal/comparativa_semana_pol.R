#################################################################################################################
# SCRIPT FIGURAS ESPESAMIENTO
# Por: Sebastián Gonzalez
# 17-02-2020
#################################################################################################################

#################################################################################################################
# TODO
#################################################################################################################

# 1.- Separar la creación de figuras de la actualización del excel de clasificación.

setwd("T:/PROCESOS/18. Seguimientos/Polimero/Comparativa polimero/semanal")

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

# Se retiran las funciones que no se utilizaran POR COMPLETAR
#rm("return_df_dh", "return_clasificacion_dh")

df_espesamiento <- return_df_esp()
df_deshidratacion <- return_df_dh()
df_pol_stock <- return_df_pol()
df_pol_scada <- return_pol_scada()
df_pol_lab <- return_pol_lab()

# Se remoueven los workbooks que no se van a modificar
rm("wb.espesamiento")

#################################################################################################################
# PARÁMETROS Y DATOS PREVIOS
#################################################################################################################

# Semana de la cual se rescatarán los datos
semana_inicio <- 19
semana_termino <- 20
year <- 2020

# Se tabaja con una df temporal para limpiar los datos de los indices de marcha 0
.tmp_esp <- df_espesamiento %>% dplyr::filter(indice == 1) %>% drop_na(sequedad, tasaCaptura)
.tmp_dh <- df_deshidratacion %>% dplyr::filter(indice == 1) %>% drop_na(sequedad, tasaCaptura)

.tmp_stock <- df_pol_stock 
.tmp_scada <- df_pol_scada 
.tmp_lab <- df_pol_lab %>% drop_na(concentracion)


#################################################################################################################
# LOOP FIGURAS
#################################################################################################################

for (semana in semana_inicio:semana_termino) {
  print(semana)
  
  # Crea un directorio para almacenar las figuras
  path <- paste0("T:/PROCESOS/18. Seguimientos/Polimero/Comparativa polimero/semanal/", year)
  asigna_wd(path, paste0(semana))
  
  #################################################################################################################
  # COMPARATIVA CONCETRACIÓN
  #################################################################################################################
  
  lista_unidades<-levels(.tmp_scada$unidad)
  df_comparacion <- return_comp_pol(.tmp_scada, .tmp_lab)
  
  semana_ini <- semana
  semana_term <- semana - 15
  
  if (semana_ini %in% 1:9){
    semana_ini <- paste0("0", as.character(semana_ini))
  }
  if (semana_term %in% 1:9){
    semana_term <- paste0("0", as.character(semana_term))
  }
  
  fin <- paste0(year, semana_ini, 7)
  inicio <- paste0(year, semana_term, 1)
  dia_termino <- as.Date(fin, "%Y%W%u")
  dia_inicio <- as.Date(inicio, "%Y%W%u")
  
  
  for (equipo in lista_unidades){
    print(equipo)
    .tmp_plot <- fig_comparativa_pol(df_comparacion, equipo, dia_inicio, dia_termino)
    ggsave(.tmp_plot, file=paste(c("comp_pol_,", equipo, ".png"), sep = "", collapse = ""),
           width = 12, height = 9.9, units = "cm", dpi=320)
    rm(.tmp_plot)
  }
  
  #################################################################################################################
  # STOCK VS. SCADA
  #################################################################################################################
  
  temp_plot <- fig_scada_stock(.tmp_esp, .tmp_stock, "Espesamiento", 2020)
  ggsave(temp_plot,file=paste(c("polimero_semana_esp.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_scada_stock(.tmp_dh, .tmp_stock, "Deshidratación", 2020)
  ggsave(temp_plot,file=paste(c("polimero_semanal_dh.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_polimero_mes(.tmp_esp, .tmp_stock, "Espesamiento", 2020)
  ggsave(temp_plot,file=paste(c("polimero_mensual_esp.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_polimero_mes(.tmp_dh, .tmp_stock, "Deshidratación", 2020)
  ggsave(temp_plot,file=paste(c("polimero_mensual_dh.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_objetivo_anual(.tmp_esp, .tmp_stock, "Espesamiento", 2020, 12)
  ggsave(temp_plot,file=paste(c("objetivo_pol_esp.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)
  
  temp_plot <- fig_objetivo_anual(.tmp_dh, .tmp_stock, "Deshidratación", 2020, 8.11)
  ggsave(temp_plot,file=paste(c("objetivo_pol_dh.png"), sep = "", collapse = ""),
         width = 15, height = 7, units = "cm", dpi=320)
  rm(temp_plot)

}
