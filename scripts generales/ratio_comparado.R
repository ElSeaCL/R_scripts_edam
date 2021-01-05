#################################################################################################################
# SCRIPT COMPARATIVO RATIOS ENTRE AÑOS 
# Por: Sebastián Gonzalez 
# 17-02-2020
#################################################################################################################
# Calculo de los ratios reales de polímero (uso de planilla de stock) en espesamiento 
# y deshidratación. Para el caso de espesamiento se realiza un calculo estimativo
# del ratio unicamente tomando en cuenta el polímero por CAMBI

rm(list = ls())
#################################################################################################################
# TODO
#################################################################################################################

# 1.- Separar la creación de figuras de la actualización del excel de clasificación.

setwd("T:/PROCESOS/2. Comisiones/Polímero/Presentación/imagenes")

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
                 ,"stringi"     # Manipulacion de strings de caracteres
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
semana_inicio <- 36
semana_termino <- 36
year_inicio <- 2018
year_termino <- 2020

# Se tabaja con una df temporal para limpiar los datos de los indices de marcha 0
# OJO!!! CAMBIAR EL DROP_NA YA QUE PUEDE ELIMINAR DATOS DE CARGA REALES
# CAMBIAR POR UN PROM ULTIMOS DÏAS
.tmp_esp <- df_espesamiento %>% dplyr::filter(indice == 1,
                                              fecha <= as.Date(paste0(year_termino, semana_termino, 7), "%Y%W%u")) %>%
  mutate(sequedad = if_else(is.na(sequedad), (lag(sequedad, 1) +
                                              lag(sequedad, 2) +
                                              lag(sequedad, 3))/3,
                                              sequedad),
         tasaCaptura = if_else(is.na(tasaCaptura), (lag(tasaCaptura, 1) +
                                                    lag(tasaCaptura, 2) +
                                                    lag(tasaCaptura, 3))/3,
                                                    tasaCaptura)
         )
  
.tmp_dh <- df_deshidratacion %>% dplyr::filter(indice == 1,
                                               fecha <= as.Date(paste0(year_termino, semana_termino, 7), "%Y%W%u")) %>%
  mutate(sequedad = if_else(is.na(sequedad), (lag(sequedad, 1) + lag(sequedad,2) + lag(sequedad, 3))/3, sequedad),
         tasaCaptura = if_else(is.na(tasaCaptura), (lag(tasaCaptura, 1) + lag(tasaCaptura,2) + lag(tasaCaptura, 3))/3, tasaCaptura)
         )

.tmp_stock <- df_pol_stock 
.tmp_scada <- df_pol_scada 
.tmp_lab <- df_pol_lab %>% drop_na(concentracion)

#################################################################################################################
# FIGURAS
#################################################################################################################

# ESPESAMIENTO
# Se filtran los datos seleccionando desde un año de interes y se suman los consumos
# por destino, CAMBI u otros (690, 1690, ...)

.tmp_esp_direccion <- .tmp_esp %>%
  filter(ano >= year_inicio) %>%
  group_by(ano, mes, direccion) %>%
  summarise(flujoMS = sum(flujoMS),
            poliMS = sum(poliMS)) %>%
  ungroup(ano, mes)

# Se filtran los datos seleccionando un año de interes como fue en el caso anterior 
# solo que en este caso no se agrupan por destino.

.tmp_esp_tot <- .tmp_esp %>%
  filter(ano >= year_inicio) %>%
  group_by(ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            poliMS = sum(poliMS)) %>%
  ungroup(ano)

# Se juntan los dos df anteriores y se calcula la proporción de polímero que va a CAMBI
.tmp_prop_pol_cambi <- inner_join(.tmp_esp_direccion, .tmp_esp_tot, by=c("ano", "mes"), suffix = c(".par", ".tot")) %>%
  mutate(polimero.prop = poliMS.par / poliMS.tot) %>%
  filter(direccion == "CAMBI")

# Se calcula el polímero mensual
.tmp_stock_esp <- return_stock_dia(.tmp_stock, "Espesamiento") %>%
  mutate(ano = year(fecha_inicio),
         mes = month(fecha_inicio)) %>%
  group_by(ano, mes) %>%
  summarise(pol.total = sum(consumo_rango))

.tmp_plot <- inner_join(.tmp_prop_pol_cambi, .tmp_stock_esp, by=c("ano", "mes")) %>%
  mutate(ratio.cambi = pol.total * polimero.prop * 1000 / flujoMS.par) %>%
  select(ano, mes, ratio.cambi) %>%
  mutate(ano = as.factor(ano)) %>%
  ggplot() +
  geom_line(aes(x=mes, y=ratio.cambi, color=ano), size = 1) +
  labs(title="Ratio Polímero Stock Estimado", subtitle = "CAMBI",
       x= "Mes",
       y= "Ratio (Kg/ton)") +
  scale_color_discrete(name = "Año") +
  scale_x_continuous(breaks = 1:12)

ggsave(.tmp_plot,file=paste(c("ratio_cambi.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)

# DESHIDRATACION
# Se filtran inicialmente los datos, se calcula el polímero en polvo dosificado 
# y se seleccionan los años de interes para finalmente agrupar los datos por año y mes.
.tmp_dh_tot <- .tmp_dh %>%
  filter(indice == 1) %>%
  mutate(mes = month(fecha),
         poliMS = ratio * flujoMS / 1000) %>%
  filter(ano >= year_inicio) %>%
  group_by(ano, mes) %>%
  summarise(flujoMS = sum(flujoMS))


.tmp_stock_dh <- return_stock_dia(.tmp_stock, "Deshidratación") %>%
  filter(year(fecha_inicio) >= year_inicio) %>%
  mutate(ano = year(fecha_inicio),
         mes = month(fecha_inicio)) %>%
  group_by(ano, mes) %>%
  summarise(pol.total = sum(consumo_rango))

.tmp_plot <- inner_join(.tmp_dh_tot, .tmp_stock_dh, by=c("ano", "mes")) %>%
  mutate(ratio.cambi = pol.total * 1000 / flujoMS) %>%
  select(ano, mes, ratio.cambi) %>%
  mutate(ano = as.factor(ano)) %>%
  ggplot() +
  geom_line(aes(x=mes, y=ratio.cambi, color=ano), size = 1) +
  labs(title="Ratio Polímero Stock", subtitle = "Deshidratación",
       x= "Mes",
       y= "Ratio (Kg/ton)") +
  scale_color_discrete(name = "Año") +
  scale_x_continuous(breaks = 1:12)

ggsave(.tmp_plot,file=paste(c("ratio_dh.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)

.tmp_esp %>%
  filter(indice == 1
         , ano == 2020
         , direccion == "CAMBI"
#         , centrifuga== "E"
         ) %>%
  group_by(centrifuga) %>%
  summarise(seq.prom = mean(sequedad))
  
.tmp_esp %>%
  filter(indice == 1
         , ano == 2020
         , direccion == "CAMBI"
         , centrifuga== "H"
         , sequedad > 10
         #, sequedad < 17
         , fecha >= as.Date("2020-06-01")
         ) %>%
  ggplot(aes(x=vr, y=sequedad)) +
  geom_point() +
  geom_smooth(method='lm')
  
