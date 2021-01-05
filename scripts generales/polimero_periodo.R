#################################################################################################################
# Cálculos de ratio, sequdad y centrado por periodo de operación
# Por: Sebastián Gonzalez
# 17-02-2020
#################################################################################################################

# En este caso consideramos periodo al transcurso continuo de operación con un producto de polímero
# En caso de que se cambie de polímero se termina el periodo anterior y se comienza un nuevo periodo.

#################################################################################################################
# TODO
#################################################################################################################

# 1.- Separar la creación de figuras de la actualización del excel de clasificación.

setwd("T:/PROCESOS/18. Seguimientos/Espesamiento/Comparativa Espesamiento/semanal")
rm(list=ls())

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
                 ,"RColorBrewer"
)

rm(cargar_librerias)

#################################################################################################################
# CARGAR DATOS ESPESAMIENTO
#################################################################################################################

# Se retiran las funciones que no se utilizaran
rm("return_df_dh", "return_clasificacion_dh")

df_espesamiento <- return_df_esp(horas_imput=TRUE)
df_deshidratacion <- return_df_dh(horas_imput=TRUE)

# Se remoueven los workbooks que no se van a modificar
rm("wb.espesamiento")
rm("wb.deshidratacion")
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
year <- 2016

# Lista de centrífugas a graficar
centrifugas <- c("A", "B", "C", "D", "E", "F", "G", "H")

# Se tabaja con una df temporal para limpiar los datos de los indices de marcha 0
.tmp_esp <- df_espesamiento %>% dplyr::filter(indice == 1,
                                              ano == 2020) %>%
            drop_na(sequedad, tasaCaptura)


df_pruebas <- .tmp_esp %>%
  filter(direccion == "CAMBI") %>%
  group_by(fecha, polimero) %>%
  summarise(lodoMS = sum(flujoMS),
            poliMS = sum(poliMS),
            sequedad = sum(sequedad * flujoMS) / lodoMS,
            centrado = sum(centrado * flujoMS) / lodoMS) %>%
  mutate(ratio = poliMS * 1000 / lodoMS) %>%
  ungroup(fecha) %>%
  mutate(periodo = 1)

contador <- 1
for (x in 1:nrow(df_pruebas)){
  if (x != 1){
    if(df_pruebas$polimero[x] != df_pruebas$polimero[x - 1]){
      print("cambio de polimero")
      contador <- contador + 1
    }
  }
  df_pruebas$periodo[x] <- contador
}

df_pruebas %>%
  group_by(periodo, polimero) %>%
  summarise(lodoMS.tot = sum(lodoMS),
            poliMS.tot = sum(poliMS),
            ratio = poliMS.tot * 1000 / lodoMS.tot,
            seq.pon = sum(sequedad * lodoMS) / lodoMS.tot,
            cent.pon = sum(centrado * lodoMS) / lodoMS.tot)
View

########################################################
############ DESHIDRATACIÓN

.tmp_dh <- df_deshidratacion %>% dplyr::filter(indice == 1,
                                              ano == 2020) %>%
  drop_na(sequedad, tasaCaptura)

df_pruebas <- .tmp_dh %>%
  mutate(poliMS = flujoMS * ratio / 1000) %>%
  group_by(fecha, polimero) %>%
  summarise(lodoMS = sum(flujoMS),
            poliMS = sum(poliMS),
            sequedad = sum(sequedad * flujoMS) / lodoMS,
            centrado = sum(centrado * flujoMS) / lodoMS) %>%
  mutate(ratio = poliMS * 1000 / lodoMS) %>%
  ungroup(fecha) %>%
  mutate(periodo = 1)

contador <- 1
for (x in 1:nrow(df_pruebas)){
  if (x != 1){
    if(df_pruebas$polimero[x] != df_pruebas$polimero[x - 1]){
      print("cambio de polimero")
      contador <- contador + 1
    }
  }
  df_pruebas$periodo[x] <- contador
}

df_pruebas %>%
  group_by(periodo, polimero) %>%
  summarise(lodoMS.tot = sum(lodoMS),
            poliMS.tot = sum(poliMS),
            ratio = poliMS.tot * 1000 / lodoMS.tot,
            seq.pon = sum(sequedad * lodoMS) / lodoMS.tot,
            cent.pon = sum(centrado * lodoMS) / lodoMS.tot)
