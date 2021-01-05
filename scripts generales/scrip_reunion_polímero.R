#################################################################################################################
# INFORME SEMANAL ESPESAMIENTO
# Por: Sebastián Gonzalez
# 07-04-2020
#################################################################################################################

# TODO: Ver si se puede rescatar algo par ser utilizado de manera más general.


setwd("T:/PROCESOS/18. Seguimientos/Polimero/reunión polímero")

#################################################################################################################
# LIBRERIAS USUARIO
#################################################################################################################

#Funciones básicas, muchas de estas son necesarias para algunas de las gráficas
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/funciones/funciones_generales.R")

# Diferentes funciones para graficar
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/funciones/funciones_graficas.R")

# funciones que retornan los distintos data frames
source("T:/PROCESOS/18. Seguimientos/Varios/R scripts/funciones/funciones_data.R")


#################################################################################################################
# LIBRERIAS R
#################################################################################################################

# Lista de paquetes installados
cargar_librerias("stringr"    # Manipulación de strings
                 ,"ggplot2"    # Paquete con funciones para generar gráficos 
                 ,"plotly"     # Gráficas interactivas gracias a plotly.js
                 ,"dplyr"      # Herramientas para realizar operaciones en dataframes
                 ,"lubridate"  # Herramientas de fecha y hora
                 ,"tidyr"      # Organizador de data tabular
                 ,"openxlsx"   # Manipulación de excel
                 ,"stringi"    # quita los acentos
                 ,"RColorBrewer"
                 , "zoo"       # Infraestructura para series de tiempo regulares e irregulares
)

rm(cargar_librerias)

#################################################################################################################
# CARGAR DATOS ESPESAMIENTO
#################################################################################################################

# Base de espesamiento proveniente de Comparativo espesamiento 2020
df_espesamiento <- return_df_esp(horas_imput = TRUE, missing_sample_input = TRUE)

# Base de deshidratación proveniente de Comparativo deshidratacion 2020
df_deshidratacion <- return_df_dh(horas_imput=TRUE)

# Bases de polímero proveniente de Comparativo polimero 2020
## polímero según stock
df_pol_stock <- return_df_pol()
## polímero según scada
df_pol_scada <- return_pol_scada()
## contraste concentraciones en unidades
df_pol_lab <- return_pol_lab()

# Base con loss datos de lodo entrada a espesamiento
df_cent_in <- return_df_cent_in()

# Base con datos de salida pre-epesadores
df_pre_out <- return_df_pre_out()

# Se remoueven los workbooks que no se van a modificar
rm("wb.deshidratacion",
   "wb.espesamiento")

rm("return")

#################################################################################################################
# PARÁMETROS Y DATOS PREVIOS
#################################################################################################################

# Semana de la cual se rescatarán los datos
tipo <- "Espesamiento"
semana_inicio <- 1
semana_termino <- 52
year <- 2020

# Lista de centrífugas a graficar
centrifugas <- c("A", "B", "C", "D", "E", "F", "G", "H")

#################################################################################################################
# LIMPIEZA PREVIA
#################################################################################################################

# Se tabaja con una df temporal para limpiar los datos de los indices de marcha 0
df_espesamiento <- df_espesamiento %>% dplyr::filter(indice == 1 #,
                                              #sem <= semana_termino,
                                              #direccion == "CAMBI"
                                              ) %>%
  mutate(sequedad = if_else(is.na(sequedad), (lag(sequedad, 1) + lag(sequedad,2) + lag(sequedad, 3))/3, sequedad),
         tasaCaptura = if_else(is.na(tasaCaptura), (lag(tasaCaptura, 1) + lag(tasaCaptura,2) + lag(tasaCaptura, 3))/3, tasaCaptura)
         )

df_deshidratacion <- df_deshidratacion %>% dplyr::filter(indice == 1#,
                                               #sem <= semana_termino
                                               ) %>% 
  mutate(sequedad = if_else(is.na(sequedad), (lag(sequedad, 1) + lag(sequedad,2) + lag(sequedad, 3))/3, sequedad),
         tasaCaptura = if_else(is.na(tasaCaptura), (lag(tasaCaptura, 1) + lag(tasaCaptura,2) + lag(tasaCaptura, 3))/3, tasaCaptura)
         )

setwd("T:/PROCESOS/18. Seguimientos/Polimero/reunión polímero")

#################################################################################################################
# FIGURAS
#################################################################################################################
#################################################################################################################
# SERIES TIEMPO PRE-ESPESADORES
#################################################################################################################

# Base con los promedios moviles
.tmp_prom <- return_prom_pre(df_cent_in)

# figura comparativa de MS
temp_plot <- fig_prom_in(.tmp_prom, "MS", c(2019,2020), scada = FALSE, 1, 52)
ggsave(temp_plot,file=paste(c("MS.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)
rm(temp_plot)

# Evolución del MV
temp_plot <- fig_prom_in(.tmp_prom, "MV", c(2019, 2020), scada = FALSE, 1, 52)
ggsave(temp_plot,file=paste(c("MV.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)
rm(temp_plot)

# Otra opción de ver lo mismo solo que como una sola serie
temp_plot <- .tmp_prom %>%
  ggplot(aes(x = fecha, y = MSlab)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Concentraci\u00F3n Lodo Pre-Esp a Centr\u00EDfugas",
      x = "Fecha",
      y = "MS (g/L)") +
  scale_x_date(breaks = "4 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank())
ggsave(temp_plot,file=paste(c("MS_all.png"), sep = "", collapse = ""),
       width = 24, height = 7, units = "cm", dpi=320)
rm(temp_plot)

temp_plot <- .tmp_prom %>%
  filter(MVlab > 0.75) %>%
  ggplot(aes(x = fecha, y = MVlab)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Concentraci\u00F3n Lodo Pre-Esp a Centr\u00EDfugas",
       x = "Fecha",
       y = "MV (g/L)") +
  scale_x_date(breaks = "4 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank())
ggsave(temp_plot,file=paste(c("MV_all.png"), sep = "", collapse = ""),
       width = 24, height = 7, units = "cm", dpi=320)
rm(temp_plot)


#################################################################################################################
# MANTOS
#################################################################################################################

## Se comienza 
datosMantos <- df_pre_out %>%
  filter(indice_marcha == 1, fecha < today()) %>%
  group_by(fecha) %>%
  summarise(total.mantos = sum(manto)) 

for (i in 1:dim(datosMantos)[1]){
  if (is.na(datosMantos$total.mantos)) {
    datosMantos$total.mantos[i] = sum(datosMantos$total.mantos[i-1],
                                      datosMantos$total.mantos[i-2],
                                      datosMantos$total.mantos[i-3])/3 
  }
}

tmp.mantos <- datosMantos %>%
  mutate(year = factor(year(fecha))) %>%
  mutate(total.mantos.7d = (lag(total.mantos,0) +
                       lag(total.mantos,1) +
                       lag(total.mantos,2) +
                       lag(total.mantos,3) +
                       lag(total.mantos,4) +
                       lag(total.mantos,5) +
                       lag(total.mantos,6))/7) 

plot_mantos <- tmp.mantos %>%
  ggplot() +
  geom_line(aes(x = as.Date(paste("2020", strftime(fecha, format = "%m-%d"), sep = "-")), 
                y = total.mantos.7d,
                color = year),
            size = 1) +
  scale_color_discrete(name = "Año") +
  scale_x_date(name = "Fecha", breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(name = "Altura Manto (m)") +
  labs(title = "Suma de mantos pre-espesadores",
       subtitle = "Promedio movil")

tmp.mantos %>%
  ggplot(aes(x = fecha, y = total.mantos)) +
  geom_point() +
  geom_smooth()

ggsave(pre.mantos, file="pre_mantos.png", width = 25, height = 12, units = "cm", dpi=320)

### Resultados generales Sequedad, Tasa de Captura y Ratios según SCADA


# Para el caso del ratio conviene usar los datos de stock
consumo_semana_stock <- return_stock_dia(df_pol_stock, "Espesamiento")

consumo_mes_esp <- consumo_semana %>%
  filter(year(fecha_inicio) >= 2018) %>%
  mutate(ano = as.factor(year(fecha_inicio))) %>%
  group_by(mes, ano) %>%
  summarise(consumo_mes = sum(consumo_rango)) %>%
  ungroup() %>%
  mutate(mes_ano = as.yearmon(as.Date(paste(ano, mes, "01", sep = "-")))) %>%
  select(mes_ano, consumo_mes)
  
consumo_mes_scada <- df_espesamiento %>%
  group_by(mes, ano, direccion) %>%
  summarise(lodoMS = sum(flujoMS),
            polimeroMS = sum(poliMS)) %>%
  ungroup()

consum_mes_portipo <- consumo_mes_scada %>%
  group_by(mes, ano) %>%
  summarise(lodoMS = sum(lodoMS),
            polimeroMS = sum(polimeroMS)) %>%
  ungroup()

right_join(consumo_mes_scada, consum_mes_portipo, by = c("mes", "ano")) %>%
  mutate(porc_pol = lodoMS.x / lodoMS.y,
         porc_lodo = polimeroMS.x/polimeroMS.y) %>%
  select(mes, ano, direccion, porc_pol, porc_lodo, lodoMS.y) %>%
  mutate(mes_ano = as.yearmon(as.Date(paste(ano, mes, "01", sep = "-")))) %>%
  inner_join(consumo_mes_esp, by = c("mes_ano")) %>%
  select(mes_ano, direccion, porc_lodo, porc_pol, lodoMS.y, consumo_mes) %>%
  filter(direccion == "CAMBI") %>%
  mutate(ratio = (consumo_mes * porc_pol * 1000)/(lodoMS.y * porc_lodo)) %>%
  select(mes_ano, ratio) %>%
  ggplot(aes(x = mes_ano, y = ratio)) +
  geom_line()

# Comparación del consumo por año
temp_plot <- consumo_semana %>%
  filter(year(fecha_inicio) >= 2018) %>%
  mutate(ano = as.factor(year(fecha_inicio))) %>%
  group_by(mes, ano) %>%
  summarise(consumo_mes = sum(consumo_rango)/1000) %>%
  ggplot(aes(x = factor(mes), y = consumo_mes)) +
  geom_col(aes(fill = ano), position = "dodge") +
  scale_x_discrete(name = "Mes", breaks = c(1, 3, 5, 7, 9, 11), 
                   labels = c("ene.", "mar.", "may.", "jul.", "sep.", "nov.")) +
  scale_y_continuous(name = "Consumo (ton/mes)") +
  scale_fill_discrete(name = "A\u00F1o") +
  labs(title="Consumos mensual de polímero por stock",
       subtitle = "Espesamiento")
ggsave(temp_plot,file=paste(c("consumo_stock_esp.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)
rm(temp_plot)



# Base para los promedios móviles
.tmp_prom <- return_prom_movil(df_espesamiento, filter="CAMBI", group_pol = TRUE)

fig_prom_movil(.tmp_prom, "ratio", c(2018, 2019,2020), 1, 52, show_pol = TRUE)

library(zoo)
ratio_mensual <- df_espesamiento %>%
  filter(direccion == "CAMBI",
         ratio > 5, ratio < 25,
         ano >= 2018) %>%
  group_by(month(fecha), ano) %>%
  summarise(ratio = sum(poliMS) * 1000 / sum(flujoMS)) %>%
  ungroup() %>%
  mutate(ano_mes = as.yearmon(as.Date(paste(ano, `month(fecha)`, "01", sep = "-")))) %>%
  select(ano_mes, ratio)

ratio_ts <- ts(ratio_mensual$ratio, frequency = 12, start = 2018)

ratio_ts %>%
  ggseasonplot() +
  
  
 
plot(decompose(ratio_ts))
 
####################################################################
# COMPARATIVA RATIOS
####################################################################
# SE GRAFICAN LOS RATIOS MENSUALES DE ESPESAMIENTO SEGÚN SCADA
# Base con los promedios calculados
# común para las comparativas de ratio, sequedad y tasa de captura
.tmp_prom <- return_prom_movil(df_espesamiento, filter="CAMBI", group_pol = TRUE)

#listado de polímeros 2019-2020
lista_polimeros <- levels(factor(.tmp_prom$polimero[which(year(.tmp_prom$fecha) >= 2019)]))

# creamos un vector de colores a esta vector
poli_rainbow <- rainbow(length(lista_polimeros))
names(ex1) <- lista_polimeros

temp_plot_2019 <- fig_prom_movil(.tmp_prom, "ratio", c(2019), 1, 44, show_pol = TRUE) 
ggsave(temp_plot_2019,file=paste(c("ratio_2019.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2019)

temp_plot_2020 <- fig_prom_movil(.tmp_prom, "ratio", 2020, 32, 44, show_pol = TRUE) 
ggsave(temp_plot_2020,file=paste(c("ratio_2020_zoom.png"), sep = "", collapse = ""),
       width = 11, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2020)

tendencia_ratio <- .tmp_prom %>%
  filter(year(fecha) >= 2018) %>%
  ggplot(aes(x = fecha, y = ratio_mean)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(name = "Ratio (kg/ton)", limits = c(5, 25)) +
  scale_x_date(breaks = "4 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank()) +
  labs(title = "Ratio ponderado diario",
       subtitle = "Espesamiento")
ggsave(tendencia_ratio,file=paste(c("ratio_tendencia.png"), sep = "", collapse = ""),
       width = 23, height = 10, units = "cm", dpi=320)
rm(tendencia_ratio)

# RATIO STOCK
## Se calcula la proporción de polímero usado por CAMBI según scada

consumo_stock_tipo <- df_espesamiento %>%
  filter(ano >= 2016,
         poliMS > 0) %>%
  group_by(direccion,ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            poliMS = sum(poliMS)) %>%
  ungroup()

consumo_stock <- consumo_stock_tipo %>%
  group_by(ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            poliMS = sum(poliMS)) %>%
  ungroup()



consumo_cambi_scada <- right_join(consumo_stock_tipo, consumo_stock, by = c("ano", "mes"), suffix = c("", ".total")) %>%
  mutate(prop_polimero = poliMS / poliMS.total) %>%
  filter(direccion == "CAMBI") %>%
  select(ano, mes, flujoMS.total, prop_polimero)

.tmp_stock_2018 <- return_stock_dia(df_pol_stock, "Espesamiento", 2018)
.tmp_stock_2019 <- return_stock_dia(df_pol_stock, "Espesamiento", 2019)
.tmp_stock_2020 <- return_stock_dia(df_pol_stock, "Espesamiento", 2020)
.tmp_stock <- rbind(.tmp_stock_2018, .tmp_stock_2019, .tmp_stock_2020)

consumo_stock <- .tmp_stock %>%
  mutate(ano = year(fecha_inicio)) %>%
  group_by(ano, mes) %>%
  summarise(poliMS.stock = sum(consumo_rango)) %>%
  ungroup()

ratios_stock_cambi <- inner_join(consumo_stock, consumo_cambi_scada, by = c("ano", "mes")) %>%
  mutate(ratio_cambi = poliMS.stock * prop_polimero * 1000 / flujoMS.total,
         ano_mes = as.yearmon(paste(ano, mes, "01", sep = "-"))) %>%
  select(ano_mes, ano, mes, ratio_cambi)

ratio_stock_cambi <- ratios_stock_cambi %>%
  ggplot(aes(x = ano_mes, y = ratio_cambi)) +
  geom_line(size = 1) + 
  scale_x_yearmon(breaks = ratios_stock_cambi$ano_mes[seq(1, 36, 3)], format = "%b-%Y") +
  scale_y_continuous(limits = c(5, 25)) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ratio (kg/ton)",
       title = "Ratio Stock",
       subtitle = "Espesamiento CAMBI")
ggsave(ratio_stock_cambi,file=paste(c("ratio_stock_cambi.png"), sep = "", collapse = ""),
       width = 23, height = 10, units = "cm", dpi=320)
rm(ratio_stock_cambi)


# ----------------------------------------------------------------------------------------------------------------------#

####################################################################
# COMPARATIVA SEQUEDAD
####################################################################

temp_plot_2019 <- fig_prom_movil(.tmp_prom, "sequedad", 2019, 1, 44, show_pol = TRUE) 
ggsave(temp_plot_2019,file=paste(c("sequedad_2019.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2019)

temp_plot_2020 <- fig_prom_movil(.tmp_prom, "sequedad", 2020, 1, 44, show_pol = TRUE) 
ggsave(temp_plot_2020,file=paste(c("sequedad_2020.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2020)

resultados_mes_esp <- df_espesamiento %>%
  filter(direccion == "CAMBI",
         ano >= 2018) %>%
  mutate(sequedad_por_carga = sequedad * flujoMS,
         tasacaptura_por_carga = tasaCaptura * flujoMS) %>%
  group_by(ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            sequedad_por_carga = sum(sequedad_por_carga),
            tasacaptura_por_carga = sum(tasacaptura_por_carga)) %>%
  ungroup() %>%
  mutate(sequedad = sequedad_por_carga / flujoMS,
         tasaCaptura = tasacaptura_por_carga / flujoMS) %>%
  select(ano, mes, sequedad, tasaCaptura)

sequedad_mes_esp <- resultados_mes_esp %>%
  mutate(ano = factor(ano)) %>%
  ggplot(aes(x = mes, y = sequedad, color = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12, labels = 
                       c("Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.", 
                         "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic.")) +
  scale_color_discrete(name = "Año") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Sequedad (%)",
       title = "Sequedad Mensual",
       subtitle = "Espesamiento CAMBI")
ggsave(sequedad_mes_esp, file=paste(c("sequedad_mes.png"), sep = "", collapse = ""),
       width = 23, height = 10.0, units = "cm", dpi=320)
rm(sequedad_mes_esp)

####################################################################
# COMPARATIVA TASA DE CAPTURA
####################################################################

temp_plot_2019 <- fig_prom_movil(.tmp_prom, "tasa de captura", 2019, 1, 44, show_pol = TRUE) 
ggsave(temp_plot_2019,file=paste(c("tdc_2019.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2019)

temp_plot_2020 <- fig_prom_movil(.tmp_prom, "tasa de captura", 2020, 1, 44, show_pol = TRUE) 
ggsave(temp_plot_2020,file=paste(c("tdc_2020.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2020)

tdc_mes_esp <- resultados_mes_esp %>%
  mutate(ano = factor(ano)) %>%
  ggplot(aes(x = mes, y = tasaCaptura, color = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12, labels = 
                       c("Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.", 
                         "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic.")) +
  scale_color_discrete(name = "Año") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Tasa de Captura (%)",
       title = "Tasa de Captura Mensual",
       subtitle = "Espesamiento CAMBI")
ggsave(tdc_mes_esp, file=paste(c("tdc_mes.png"), sep = "", collapse = ""),
       width = 23, height = 10.0, units = "cm", dpi=320)
rm(tdc_mes_esp)



# Figura resumen resultados muestras espesamiento
datosResultadosEsp %>%
  group_by(ano, semana, resultado) %>%
  summarise(total = sum(total)) %>%
  mutate(resultado = factor(resultado)) %>%
  filter(ano == 2020) %>%
  ggplot() +
  geom_point(aes(x=semana, y= total, color=resultado), size = 3) +
  labs(x="Semana", y = "Casos Totales") +
  scale_color_discrete(name = "Resultados", labels = c("Medio", "Negativo", "Positivo"))


df_espesamiento %>%
  filter(direccion == "CAMBI",
         ano >= 2018,
         ratio < 30 & ratio > 1) %>%
  ggplot(aes(x = fecha, y = ratio)) +
  geom_point() +
  geom_smooth() +
  scale_x_date(breaks = "4 months", date_labels = "%b-%Y")
  
ts_ratio <- df_espesamiento %>%
  filter(direccion == "CAMBI",
         ano >= 2018,
         ratio < 30 & ratio > 1) %>%
  select(fecha, ratio)


####################################################################
# RESULTADOS POR CENTRÍFUGA ESPESAMIENTO
####################################################################

# PARAMETROS FILTRO
# Sefijan los filtros para descartar resultados extremos que pueden ser errados
ratio_min <- 4
ratio_max <- 25
sequedad_min <- 5
sequedad_max <- 25
tasa_captura_min <- 50
tasa_captura_max <- 100

df_espesamiento <- df_espesamiento %>%
  filter(ano == 2020,
         direccion == "CAMBI",
         between(ratio, ratio_min, ratio_max),
         between(sequedad, sequedad_min, sequedad_max),
         between(tasaCaptura, tasa_captura_min, tasa_captura_max)
         )

df_espesamiento_eq <- df_espesamiento %>%
  filter(mes == 10) %>%
  mutate(ratio_carga = ratio * flujoMS,
         seq_carga = sequedad * flujoMS,
         tdc_carga = tasaCaptura * flujoMS,
         centrifuga = as.factor(centrifuga),
         mes = as.factor(mes)) %>%
  group_by(centrifuga) %>%
    summarise(ratio_carga = sum(ratio_carga),
              seq_carga = sum(seq_carga),
              tdc_carga = sum(tdc_carga),
              flujoMS = sum(flujoMS)) %>%
    mutate(ratio = ratio_carga / flujoMS,
           sequedad = seq_carga / flujoMS,
           tasaCaptura = tdc_carga / flujoMS) %>%
  mutate(ratio_eq = (ratio - 12)/12 * 100,
         sequedad_eq = (sequedad - 16.5)/16 * 100,
         tasaCaptura_eq = (tasaCaptura - 95)/95 * 100) %>%
  select(centrifuga, ratio, sequedad, tasaCaptura, ratio_eq, sequedad_eq, tasaCaptura_eq)

df_esp <- df_espesamiento_eq %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado") %>%
  select(centrifuga, parametro, resultado)

df_esp_eq <- df_espesamiento_eq %>%
  select(centrifuga, ratio_eq, sequedad_eq, tasaCaptura_eq) %>%
  mutate(ratio = ratio_eq,
         sequedad = sequedad_eq,
         tasaCaptura = tasaCaptura_eq) %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado_eq") %>%
  select(centrifuga, parametro, resultado_eq)

df_esp_total <- inner_join(df_esp, df_esp_eq, by = c("centrifuga", "parametro"))

tabla_resultados_octubre <- df_esp_total %>%
  ggplot(aes(x = parametro, y = centrifuga, fill = resultado_eq)) +
  geom_tile() +
  geom_text(aes(label = round(resultado, 1))) +
  scale_fill_gradient2(low = "Red", mid = "Green", high = "Red") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Ratio (Kg/ton)", "Sequedad (%)", "Tasa de Captura (%)")) +
  labs(title = "Resultados promedio por centrífuga espesamiento",
       subtitle = "Octubre",
       x = element_blank(),
       y = "Centrífuga")
ggsave(tabla_resultados_octubre,file=paste(c("tabla_resultados_esp.png"), sep = "", collapse = ""),
       width = 15, height = 15, units = "cm", dpi=320)
rm(temp_plot_2019)
  

####################################################################
# RESULTADOS TRIMESTRE POR POLÍMERO
####################################################################
  
df_espesamiento_eq <- df_espesamiento %>%
  filter(between(mes, 8, 10)) %>%
  mutate(ratio_carga = ratio * flujoMS,
         seq_carga = sequedad * flujoMS,
         tdc_carga = tasaCaptura * flujoMS,
         centrifuga = as.factor(centrifuga),
         mes = as.factor(mes)) %>%
  group_by(polimero) %>%
  summarise(ratio_carga = sum(ratio_carga),
            seq_carga = sum(seq_carga),
            tdc_carga = sum(tdc_carga),
            flujoMS = sum(flujoMS)) %>%
  mutate(ratio = ratio_carga / flujoMS,
         sequedad = seq_carga / flujoMS,
         tasaCaptura = tdc_carga / flujoMS) %>%
  mutate(ratio_eq = (ratio - 12)/12 * 100,
         sequedad_eq = (sequedad - 16.5)/16 * 100,
         tasaCaptura_eq = (tasaCaptura - 95)/95 * 100) %>%
  select(polimero, ratio, sequedad, tasaCaptura, ratio_eq, sequedad_eq, tasaCaptura_eq)

df_esp <- df_espesamiento_eq %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado") %>%
  select(polimero, parametro, resultado)

df_esp_eq <- df_espesamiento_eq %>%
  select(polimero, ratio_eq, sequedad_eq, tasaCaptura_eq) %>%
  mutate(ratio = ratio_eq,
         sequedad = sequedad_eq,
         tasaCaptura = tasaCaptura_eq) %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado_eq") %>%
  select(polimero, parametro, resultado_eq)

df_esp_total <- inner_join(df_esp, df_esp_eq, by = c("polimero", "parametro"))

tabla_resultados_octubre <- df_esp_total %>%
  ggplot(aes(x = parametro, y = polimero, fill = resultado_eq)) +
  geom_tile() +
  geom_text(aes(label = round(resultado, 1))) +
  scale_fill_gradient2(low = "Red", mid = "Green", high = "Red") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Ratio (Kg/ton)", "Sequedad (%)", "Tasa de Captura (%)")) +
  labs(title = "Resultados promedio por polímero espesamiento",
       subtitle = "Agosto - Octubre",
       x = element_blank(),
       y = "Polímero")
ggsave(tabla_resultados_octubre,file=paste(c("tabla_resultados_esp_pol.png"), sep = "", collapse = ""),
       width = 15, height = 15, units = "cm", dpi=320)
rm(temp_plot_2019)

##################33
# PRUEBA
###################

install.packages("cowplot")
library(cowplot)
temp_plot_2020_ratio <- fig_prom_movil(.tmp_prom, "ratio", 2020, 1, 44, show_pol = TRUE) +
  theme(title = element_blank(),
        axis.title.y = element_text("Ratio"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
  
temp_plot_2020_seq_ <- fig_prom_movil(.tmp_prom, "sequedad", 2020, 1, 44, show_pol = TRUE) +
  theme(title = element_blank(),
        axis.title.y = element_text("Ratio"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

temp_plot_2020_tc <- fig_prom_movil(.tmp_prom, "Tasa de Captura", 2020, 1, 44, show_pol = TRUE)+
  theme(title = element_blank(),
        axis.title.y = element_text("Ratio"),
        axis.title.x = element_blank(),
        legend.position = "bottom")

resumen_esp <- cowplot::plot_grid(temp_plot_2020_ratio, temp_plot_2020_seq_, temp_plot_2020_tc, 
                   align = "v",
                   ncol = 1,
                   rel_heights = c(0.3, 0.3, 0.4))

ggsave(resumen_esp,file=paste(c("resumen_resultados_esp.png"), sep = "", collapse = ""),
       width = 23, height = 13, units = "cm", dpi=320)

#################################################################################################################
# DESHIDRATACION
#################################################################################################################
####################################################################
# LODO DE ENTRADA DESHIDRATACION
####################################################################

return_prom_dig <- function(df_dig, filter=NULL){
  
  .tmp_dig <- df_dig %>%
    group_by(fecha) %>%
    summarise(concMS = mean(concMS),
              concMV = mean(concMV)) %>%
    mutate(MS_prom = (lag(concMS,0)+lag(concMS,1)+lag(concMS,2)+lag(concMS,3)+lag(concMS,4)+lag(concMS,5)+lag(concMS,6))/7,
           MV_prom = (lag(concMV,0)+lag(concMV,1)+lag(concMV,2)+lag(concMV,3)+lag(concMV,4)+lag(concMV,5)+lag(concMV,6))*100/7
    )
  
  return(.tmp_dig)
}

.tmp_dig <- return_prom_dig(df_deshidratacion)

temp_plot <- fig_prom_dig(.tmp_dig, "MS", c(2019,2020), 1, 45)
ggsave(temp_plot,file=paste(c("MS_dig.png"), sep = "", collapse = ""),
       width = 15, height = 7.0, units = "cm", dpi=320)
rm(temp_plot)

temp_plot <- fig_prom_dig(.tmp_dig, "MV",c(2019,2020), 1, 45)
ggsave(temp_plot,file=paste(c("MV_dig.png"), sep = "", collapse = ""),
       width = 15, height = 7.0, units = "cm", dpi=320)
rm(temp_plot)

####################################################################
# COMPARATIVA RATIOS
####################################################################
# SE GRAFICAN LOS RATIOS MENSUALES DE ESPESAMIENTO SEGÚN SCADA
# Base con los promedios calculados
# común para las comparativas de ratio, sequedad y tasa de captura
.tmp_prom <- return_prom_movil(df_deshidratacion, filter="Deshidratacion", group_pol = TRUE)

#listado de polímeros 2019-2020
lista_polimeros <- levels(factor(.tmp_prom$polimero[which(year(.tmp_prom$fecha) >= 2019)]))

# creamos un vector de colores a esta vector
poli_rainbow <- rainbow(length(lista_polimeros))
names(ex1) <- lista_polimeros

temp_plot_2019 <- fig_prom_movil(.tmp_prom, "ratio", 2019, 1, 45, show_pol = TRUE) 
ggsave(temp_plot_2019,file=paste(c("ratio_2019_dh.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2019)

temp_plot_2020 <- fig_prom_movil(.tmp_prom, "ratio", 2020, 1, 45, show_pol = TRUE) 
ggsave(temp_plot_2020,file=paste(c("ratio_2020_dh.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2020)

tendencia_ratio <- .tmp_prom %>%
  filter(year(fecha) >= 2018) %>%
  ggplot(aes(x = fecha, y = ratio_mean)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(name = "Ratio (kg/ton)", limits = c(5, 15)) +
  scale_x_date(breaks = "4 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank()) +
  labs(title = "Ratio ponderado diario",
       subtitle = "Deshidrataci\u00F3n")
ggsave(tendencia_ratio,file=paste(c("ratio_tendencia_dh.png"), sep = "", collapse = ""),
       width = 23, height = 10, units = "cm", dpi=320)
rm(tendencia_ratio)

# RATIO STOCK
## Se calcula la proporción de polímero usado por CAMBI según scada

df_deshidratacion <- df_deshidratacion %>%
  mutate(poliMS = ratio * flujoMS / 1000,
         mes = month(fecha))

consumo_stock_tipo <- df_deshidratacion %>%
  filter(ano >= 2018,
         poliMS > 0) %>%
  group_by(direccion,ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            poliMS = sum(poliMS)) %>%
  ungroup()

consumo_stock <- consumo_stock_tipo %>%
  group_by(ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            poliMS = sum(poliMS)) %>%
  ungroup()

consumo_cambi_scada <- right_join(consumo_stock_tipo, consumo_stock, by = c("ano", "mes"), suffix = c("", ".total")) %>%
  mutate(prop_polimero = poliMS / poliMS.total) %>%
  filter(direccion == "Deshidratacion") %>%
  select(ano, mes, flujoMS.total, prop_polimero)

.tmp_stock_2018 <- return_stock_dia(df_pol_stock, "Deshidratación", 2018)
.tmp_stock_2019 <- return_stock_dia(df_pol_stock, "Deshidratación", 2019)
.tmp_stock_2020 <- return_stock_dia(df_pol_stock, "Deshidratación", 2020)
.tmp_stock <- rbind(.tmp_stock_2018, .tmp_stock_2019, .tmp_stock_2020)

consumo_stock <- .tmp_stock %>%
  mutate(ano = year(fecha_inicio)) %>%
  group_by(ano, mes) %>%
  summarise(poliMS.stock = sum(consumo_rango)) %>%
  ungroup()

ratios_stock_cambi <- inner_join(consumo_stock, consumo_cambi_scada, by = c("ano", "mes")) %>%
  mutate(ratio_cambi = poliMS.stock * prop_polimero * 1000 / flujoMS.total,
         ano_mes = as.yearmon(paste(ano, mes, "01", sep = "-"))) %>%
  select(ano_mes, ano, mes, ratio_cambi)

ratio_stock_cambi <- ratios_stock_cambi %>%
  ggplot(aes(x = ano_mes, y = ratio_cambi)) +
  geom_line(size = 1) + 
  scale_x_yearmon(breaks = ratios_stock_cambi$ano_mes[seq(1, 36, 3)], format = "%b-%Y") +
  scale_y_continuous(limits = c(5, 15)) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ratio (kg/ton)",
       title = "Ratio Stock",
       subtitle = "Deshidrataci\u00F3n")
ggsave(ratio_stock_cambi,file=paste(c("ratio_stock_dh.png"), sep = "", collapse = ""),
       width = 23, height = 10, units = "cm", dpi=320)
rm(ratio_stock_cambi)

####################################################################
# COMPARATIVA SEQUEDAD
####################################################################

.tmp_prom <- .tmp_prom %>%
  drop_na(ratio_mean, seq_mean, tc_mean)

temp_plot_2019 <- fig_prom_movil(.tmp_prom, "sequedad", 2019, 1, 45, show_pol = TRUE) 
ggsave(temp_plot_2019,file=paste(c("sequedad_2019_dh.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2019)

temp_plot_2020 <- fig_prom_movil(.tmp_prom, "sequedad", 2020, 1, 44, show_pol = TRUE) 
ggsave(temp_plot_2020,file=paste(c("sequedad_2020_dh.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2020)

resultados_mes_esp <- df_deshidratacion %>%
  filter(direccion == "Deshidratacion",
         ano >= 2018) %>%
  mutate(sequedad_por_carga = sequedad * flujoMS,
         tasacaptura_por_carga = tasaCaptura * flujoMS) %>%
  group_by(ano, mes) %>%
  summarise(flujoMS = sum(flujoMS),
            sequedad_por_carga = sum(sequedad_por_carga),
            tasacaptura_por_carga = sum(tasacaptura_por_carga)) %>%
  ungroup() %>%
  mutate(sequedad = sequedad_por_carga / flujoMS,
         tasaCaptura = tasacaptura_por_carga / flujoMS) %>%
  select(ano, mes, sequedad, tasaCaptura)

sequedad_mes_esp <- resultados_mes_esp %>%
  mutate(ano = factor(ano)) %>%
  ggplot(aes(x = mes, y = sequedad, color = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12, labels = 
                       c("Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.", 
                         "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic.")) +
  scale_color_discrete(name = "Año") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Sequedad (%)",
       title = "Sequedad Mensual",
       subtitle = "Deshidrataci\u00F3n")
ggsave(sequedad_mes_esp, file=paste(c("sequedad_mes_dh.png"), sep = "", collapse = ""),
       width = 23, height = 10.0, units = "cm", dpi=320)
rm(sequedad_mes_esp)






####################################################################
# COMPARATIVA TASA DE CAPTURA
####################################################################

temp_plot_2019 <- fig_prom_movil(.tmp_prom, "tasa de captura", 2019, 1, 45, show_pol = TRUE) 
ggsave(temp_plot_2019,file=paste(c("tdc_2019_dh.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2019)

temp_plot_2020 <- fig_prom_movil(.tmp_prom, "tasa de captura", 2020, 1, 45, show_pol = TRUE) 
ggsave(temp_plot_2020,file=paste(c("tdc_2020_dh.png"), sep = "", collapse = ""),
       width = 23, height = 7.0, units = "cm", dpi=320)
rm(temp_plot_2020)

tdc_mes_esp <- resultados_mes_esp %>%
  mutate(ano = factor(ano)) %>%
  ggplot(aes(x = mes, y = tasaCaptura, color = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12, labels = 
                       c("Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.", 
                         "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic.")) +
  scale_color_discrete(name = "Año") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Tasa de Captura (%)",
       title = "Tasa de Captura Mensual",
       subtitle = "Deshidrataci\u00F3n")
ggsave(tdc_mes_esp, file=paste(c("tdc_mes_dh.png"), sep = "", collapse = ""),
       width = 23, height = 10.0, units = "cm", dpi=320)
rm(tdc_mes_esp)



# Figura resumen resultados muestras espesamiento
datosResultadosEsp %>%
  group_by(ano, semana, resultado) %>%
  summarise(total = sum(total)) %>%
  mutate(resultado = factor(resultado)) %>%
  filter(ano == 2020) %>%
  ggplot() +
  geom_point(aes(x=semana, y= total, color=resultado), size = 3) +
  labs(x="Semana", y = "Casos Totales") +
  scale_color_discrete(name = "Resultados", labels = c("Medio", "Negativo", "Positivo"))


df_espesamiento %>%
  filter(direccion == "CAMBI",
         ano >= 2018,
         ratio < 30 & ratio > 1) %>%
  ggplot(aes(x = fecha, y = ratio)) +
  geom_point() +
  geom_smooth() +
  scale_x_date(breaks = "4 months", date_labels = "%b-%Y")

ts_ratio <- df_espesamiento %>%
  filter(direccion == "CAMBI",
         ano >= 2018,
         ratio < 30 & ratio > 1) %>%
  select(fecha, ratio)


##################33
# PRUEBA
###################

install.packages("cowplot")
library(cowplot)

temp_plot_2020_ratio <- fig_prom_movil(.tmp_prom, "ratio", 2020, 1, 44, show_pol = TRUE) +
  theme(title = element_blank(),
        axis.title.y = element_text("Ratio"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

temp_plot_2020_seq_ <- fig_prom_movil(.tmp_prom, "sequedad", 2020, 1, 44, show_pol = TRUE) +
  theme(title = element_blank(),
        axis.title.y = element_text("Ratio"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

temp_plot_2020_tc <- fig_prom_movil(.tmp_prom, "Tasa de Captura", 2020, 1, 44, show_pol = TRUE)+
  theme(title = element_blank(),
        axis.title.y = element_text("Ratio"),
        axis.title.x = element_blank(),
        legend.position = "bottom")

resumen_dh <- cowplot::plot_grid(temp_plot_2020_ratio, temp_plot_2020_seq_, temp_plot_2020_tc, 
                                  align = "v",
                                  ncol = 1,
                                  rel_heights = c(0.225, 0.225, 0.45))

ggsave(resumen_dh,file=paste(c("resumen_resultados_dh.png"), sep = "", collapse = ""),
       width = 23, height = 13, units = "cm", dpi=320)


####################################################################
# RESULTADOS POR CENTRÍFUGA DESHIDRATACIÓN
####################################################################

# PARAMETROS FILTRO
# Sefijan los filtros para descartar resultados extremos que pueden ser errados
ratio_min <- 3
ratio_max <- 12
sequedad_min <- 20
sequedad_max <- 35
tasa_captura_min <- 50
tasa_captura_max <- 100

df_deshidratacion <- df_deshidratacion %>%
  mutate(mes = month(fecha))
  filter(ano == 2020,
         between(ratio, ratio_min, ratio_max),
         between(sequedad, sequedad_min, sequedad_max),
         between(tasaCaptura, tasa_captura_min, tasa_captura_max)
  )

df_deshidratacion_eq <- df_deshidratacion %>%
  filter(mes == 10) %>%
  mutate(ratio_carga = ratio * flujoMS,
         seq_carga = sequedad * flujoMS,
         tdc_carga = tasaCaptura * flujoMS,
         centrifuga = as.factor(centrifuga),
         mes = as.factor(mes)) %>%
  group_by(centrifuga) %>%
  summarise(ratio_carga = sum(ratio_carga),
            seq_carga = sum(seq_carga),
            tdc_carga = sum(tdc_carga),
            flujoMS = sum(flujoMS)) %>%
  mutate(ratio = ratio_carga / flujoMS,
         sequedad = seq_carga / flujoMS,
         tasaCaptura = tdc_carga / flujoMS) %>%
  mutate(ratio_eq = (ratio - 7)/8 * 100,
         sequedad_eq = (sequedad - 31)/27.5 * 100,
         tasaCaptura_eq = (tasaCaptura - 97)/95 * 100) %>%
  select(centrifuga, ratio, sequedad, tasaCaptura, ratio_eq, sequedad_eq, tasaCaptura_eq)

df_dh <- df_deshidratacion_eq %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado") %>%
  select(centrifuga, parametro, resultado)

df_dh_eq <- df_deshidratacion_eq %>%
  select(centrifuga, ratio_eq, sequedad_eq, tasaCaptura_eq) %>%
  mutate(ratio = ratio_eq,
         sequedad = sequedad_eq,
         tasaCaptura = tasaCaptura_eq) %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado_eq") %>%
  select(centrifuga, parametro, resultado_eq)

df_dh_total <- inner_join(df_dh, df_dh_eq, by = c("centrifuga", "parametro"))

tabla_resultados_octubre_dh <- df_dh_total %>%
  ggplot(aes(x = parametro, y = centrifuga, fill = resultado_eq)) +
  geom_tile() +
  geom_text(aes(label = round(resultado, 1))) +
  scale_fill_gradient2(low = "Red", mid = "Green", high = "Red") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Ratio (Kg/ton)", "Sequedad (%)", "Tasa de Captura (%)")) +
  labs(title = "Resultados promedio por centrífuga deshidratación",
       subtitle = "Octubre",
       x = element_blank(),
       y = "Centrífuga")
ggsave(tabla_resultados_octubre_dh,file=paste(c("tabla_resultados_dh.png"), sep = "", collapse = ""),
       width = 15, height = 15, units = "cm", dpi=320)
rm(temp_plot_2019)


####################################################################
# RESULTADOS POR POLIMERO DESHIDRATACIÓN
####################################################################

df_deshidratacion_eq <- df_deshidratacion %>%
  filter(between(mes, 8, 10)) %>%
  mutate(ratio_carga = ratio * flujoMS,
         seq_carga = sequedad * flujoMS,
         tdc_carga = tasaCaptura * flujoMS,
         polimero = as.factor(polimero),
         mes = as.factor(mes)) %>%
  group_by(polimero) %>%
  summarise(ratio_carga = sum(ratio_carga),
            seq_carga = sum(seq_carga),
            tdc_carga = sum(tdc_carga),
            flujoMS = sum(flujoMS)) %>%
  mutate(ratio = ratio_carga / flujoMS,
         sequedad = seq_carga / flujoMS,
         tasaCaptura = tdc_carga / flujoMS) %>%
  mutate(ratio_eq = (ratio - 7)/8 * 100,
         sequedad_eq = (sequedad - 27.5)/27.5 * 100,
         tasaCaptura_eq = (tasaCaptura - 97)/95 * 100) %>%
  select(polimero, ratio, sequedad, tasaCaptura, ratio_eq, sequedad_eq, tasaCaptura_eq)

df_dh <- df_deshidratacion_eq %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado") %>%
  select(polimero, parametro, resultado)

df_dh_eq <- df_deshidratacion_eq %>%
  select(polimero, ratio_eq, sequedad_eq, tasaCaptura_eq) %>%
  mutate(ratio = ratio_eq,
         sequedad = sequedad_eq,
         tasaCaptura = tasaCaptura_eq) %>%
  gather(`ratio`, `sequedad`, `tasaCaptura`, key = "parametro", value = "resultado_eq") %>%
  select(polimero, parametro, resultado_eq)

df_dh_total <- inner_join(df_dh, df_dh_eq, by = c("polimero", "parametro"))

tabla_resultados_octubre_dh <- df_dh_total %>%
  ggplot(aes(x = parametro, y = polimero, fill = resultado_eq)) +
  geom_tile() +
  geom_text(aes(label = round(resultado, 1))) +
  scale_fill_gradient2(low = "Red", mid = "Green", high = "Red") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Ratio (Kg/ton)", "Sequedad (%)", "Tasa de Captura (%)")) +
  labs(title = "Resultados promedio por polimero deshidratación",
       subtitle = "Agosto - Octubre",
       x = element_blank(),
       y = "Polímero")
ggsave(tabla_resultados_octubre_dh,file=paste(c("tabla_resultados_dh_pol.png"), sep = "", collapse = ""),
       width = 15, height = 15, units = "cm", dpi=320)
rm(temp_plot_2019)


######










# ----------------------------------------------------------------------------------------------------------- #

# Lodo Digerido
# MS
digerido.ms <- datosDH %>% 
  filter(marcha == 1,
         fecha < date("2020-04-19")
         ) %>%
  mutate(MS = flujoMS/caudalLodo) %>%
  mutate(prom.MS.7d = (lag(MS, 0) +
                            lag(MS, 1) +
                            lag(MS, 2) +
                            lag(MS, 3) +
                            lag(MS, 4) +
                            lag(MS, 5) +
                            lag(MS, 6))/7) %>%
  group_by(fecha) %>%
  summarise(MS = mean(MS),
            prom.MS.7d = mean(prom.MS.7d)) %>%
  filter(year(fecha) == 2020) %>%
  ggplot() +
  geom_line(aes(x=fecha, y=prom.MS.7d), size=1, color="wheat4") +
  geom_point(aes(x=fecha, y=MS)) +
  labs(x= "Fecha", y = "MS Lodo Digerido (g/L)") +
  scale_y_continuous(limits = c(25,38))

ggsave(digerido.ms, file="digerido_ms.png", width = 30, height = 9, units = "cm", dpi=320)
  
# MV
digerido.mv <- datosDH %>% 
  filter(marcha == 1,
         fecha < date("2020-04-19")
  ) %>%
  mutate(LDMV = LDMV * 100) %>%
  mutate(prom.MV.7d = (lag(LDMV, 0) +
                         lag(LDMV, 1) +
                         lag(LDMV, 2) +
                         lag(LDMV, 3) +
                         lag(LDMV, 4) +
                         lag(LDMV, 5) +
                         lag(LDMV, 6))/7) %>%
  group_by(fecha) %>%
  summarise(LDMV = mean(LDMV),
            prom.MV.7d = mean(prom.MV.7d)) %>%
  filter(year(fecha) == 2020) %>%
  ggplot() +
  geom_line(aes(x=fecha, y=prom.MV.7d), size=1, color="springgreen4") +
  geom_point(aes(x=fecha, y=LDMV)) +
  labs(x= "Fecha", y = "MV Lodo Digerido (%)") +
  scale_y_continuous(limits = c(60,75))

ggsave(digerido.mv, file="digerido_mv.png", width = 30, height = 9, units = "cm", dpi=320)

### Resultados generales Sequedad, Tasa de Captura y Ratios según SCADA
# Base para los promedios móviles

datosImpDH <- datosDH %>%
  filter(fecha < date("2020-04-19"),
         marcha == 1)

for(i in 1:dim(datosInfDH)[1]){
  if(is.na(datosInfDH$sequedad[i])){
    datosInfDH$sequedad[i] <- sum(
      datosInfDH$sequedad[i-1],
      datosInfDH$sequedad[i-2],
      datosInfDH$sequedad[i-3]
    )/3
  }
  if(is.na(datosInfDH$tasaCaptura[i])){
    datosInfDH$tasaCaptura[i] <- sum(
      datosInfDH$tasaCaptura[i-1],
      datosInfDH$tasaCaptura[i-2],
      datosInfDH$tasaCaptura[i-3]
    )/3
  }
}

df.prommobil.dh <- datosImpDH %>%
  mutate(ratio.dia = ratio) %>%
  group_by(fecha) %>%
  summarise(polimero.Total = sum(ratio.dia * flujoMS / 1000),
            sequedad.Total = sum(sequedad * flujoMS),
            tasaCap.Total = sum(tasaCaptura * flujoMS),
            lodo.Total = sum(flujoMS)) %>%
  mutate(ratio.dia = polimero.Total*1000/lodo.Total) %>%
  mutate(prom.ratio.7d = (lag(ratio.dia, 0) +
                            lag(ratio.dia, 1) +
                            lag(ratio.dia, 2) +
                            lag(ratio.dia, 3) +
                            lag(ratio.dia, 4) +
                            lag(ratio.dia, 5) +
                            lag(ratio.dia, 6))/7) %>%
  mutate(sequedad.dia = sequedad.Total/lodo.Total) %>%
  mutate(sum.sequedad.7d = (lag(sequedad.Total, 0) +
                              lag(sequedad.Total, 1) +
                              lag(sequedad.Total, 2) +
                              lag(sequedad.Total, 3) +
                              lag(sequedad.Total, 4) +
                              lag(sequedad.Total, 5) +
                              lag(sequedad.Total, 6))) %>%
  mutate(tasaCap.dia = tasaCap.Total/lodo.Total) %>%
  mutate(sum.tasaCap.7d = (lag(tasaCap.Total, 0) +
                             lag(tasaCap.Total, 1) +
                             lag(tasaCap.Total, 2) +
                             lag(tasaCap.Total, 3) +
                             lag(tasaCap.Total, 4) +
                             lag(tasaCap.Total, 5) +
                             lag(tasaCap.Total, 6))) %>%
  mutate(sum.lodo.7d = (lag(lodo.Total, 0) +
                          lag(lodo.Total, 1) +
                          lag(lodo.Total, 2) +
                          lag(lodo.Total, 3) +
                          lag(lodo.Total, 4) +
                          lag(lodo.Total, 5) +
                          lag(lodo.Total, 6))) %>%
  mutate(prom.sequedad.7d = sum.sequedad.7d / sum.lodo.7d,
         prom.tasaCap.7d = sum.tasaCap.7d / sum.lodo.7d) %>%
  filter(year(fecha) == 2020)

# Figura Promedio Movil Sequedad
sequedad.dh <- df.prommobil.dh %>%
  ggplot() +
  geom_line(aes(x=fecha, y=prom.sequedad.7d), color ="Blue", size = 1) +
  geom_point(aes(x=fecha, y=sequedad.dia)) +
  geom_segment(y = 28, yend = 28, x = date("2020-01-01"), xend = date("2020-04-18"), color = "red") +
  labs(x = "Fecha", y = "Sequedad DH. (%)")

ggsave(sequedad.dh, file="sequedad_dh.png", width = 30, height = 6, units = "cm", dpi=320)

# Figura Promedio Movil Tasa de Captura
tasa.cap.dh <- df.prommobil.dh %>%
  ggplot() +
  geom_line(aes(fecha, prom.tasaCap.7d), color ="Blue", size = 1) +
  geom_segment(y = 95, yend = 95, x = date("2020-01-01"), xend = date("2020-04-18"), color = "red") +
  geom_point(aes(fecha,tasaCap.dia)) +
  labs(x = "Fecha", y = "Tasa de Captura DH. (%)") +
  scale_y_continuous(limits = c(93,100))

ggsave(tasa.cap.dh, file="tasa_cap_dh.png", width = 30, height = 6, units = "cm", dpi=320)

# Figura Promedio Movil Ratio
ratio.dh <- df.prommobil.dh %>%
  ggplot() +
  geom_line(aes(fecha, prom.ratio.7d), color ="Blue", size = 1) +
  geom_point(aes(fecha, ratio.dia)) +
  geom_segment(y = 8.4, yend = 8.4, x = date("2020-01-01"), xend = date("2020-04-18"), color = "red") +
  labs(x = "Fecha", y = "Ratio DH. (kg/ton)") +
  scale_y_continuous(limits = c(3,13))

ggsave(ratio.dh, file="ratio_dh.png", width = 30, height = 6, units = "cm", dpi=320)



#################################################################################################################
# POLIMERO
#################################################################################################################


openxlsx::read.xlsx()

resumen_anual <- df_deshidratacion %>%
  filter(!is.na(ratio),
         !is.na(sequedad),
         !is.na(tasaCaptura),
         caudalLodo > 0,
         ratio > 0,
         sequedad > 0,
         tasaCaptura > 0) %>%
  mutate(poliMS = ratio * flujoMS / 1000,
         seq_por_lodo = sequedad * flujoMS,
         tc_por_lodo = tasaCaptura * flujoMS) %>%
  group_by(ano, polimero) %>%
  summarise(flujoMS = sum(flujoMS),
            caudalLodo = sum(caudalLodo), 
            poliMS = sum(poliMS),
            seq_por_lodo = sum(seq_por_lodo),
            tc_por_lodo = sum(tc_por_lodo)) %>%
  ungroup() %>%
  mutate(ratio = poliMS * 1000 / flujoMS,
         sequedad = seq_por_lodo / flujoMS,
         tasaCaptura = tc_por_lodo / flujoMS,
         polimero = factor(polimero)) %>%
  select(ano, polimero, ratio, sequedad, tasaCaptura) %>%
  filter(polimero != "-")

#%>%
  filter(polimero == "MATFLOC 8133")

write.xlsx(resumen_anual, sheetName = "algoalgo", "resumen_anual2.xlsx")

View(resumen)

plot(resumen$tasaCaptura)


###################################################################################
# TABLA COSTOS ESPECIFICOS
###################################################################################

precio_polimeros <- data.frame(matrix(c("SNF 4530", "DP 6554", "C 1198", "ZETAG 8165", "MATFLOC 8133", 
                    2.68, 2.96, 3.1, 3.36, 3.04),
                  nrow = 5), stringsAsFactors = FALSE)
names(precio_polimeros) <- c("polimero", "precio")
precio_polimeros$polimero <- as.factor(precio_polimeros$polimero)
precio_polimeros$precio <- as.numeric(precio_polimeros$precio)

# ESPESAMIENTO
df_espesamiento <- df_espesamiento %>%
  mutate(direccion = as.factor(direccion)) %>%
  filter(direccion == "CAMBI") %>%
  filter(ratio > 5,
         sequedad > 10,
         tasaCaptura > 50) %>%
  mutate(semestre = ceiling(mes/3))

db_precios_esp <- inner_join(df_espesamiento, precio_polimeros, by = "polimero") %>%
  mutate(costo.especifico = ratio * precio / (tasaCaptura / 100)) %>%
  select(fecha, semestre, polimero, ratio, sequedad, tasaCaptura, precio, costo.especifico)

costo.esp.esp <- db_precios_esp %>%
  filter(year(fecha) == 2020) %>%
  mutate(semestre = as.factor(semestre),
         polimero = as.factor(polimero)) %>%
  group_by(semestre, polimero) %>%
  summarise(ratio = mean(ratio),
            sequedad = mean(sequedad),
            tasaCaptura = mean(tasaCaptura),
            costo.especifico = mean(costo.especifico)) %>%
  ggplot(aes(x = semestre, y = costo.especifico, fill = polimero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "Costo Especifico (USD/ton)",
                     limits = c(30,65),
                     oob = scales::squish,
                     breaks = seq(30, 65, 5)) +
  labs(title = "Costos polímero por semestre",
       subtitle = "Espesamiento")
ggsave(costo.esp.esp, file="costo_esp_esp.png", width = 13, height = 10, units = "cm", dpi=320)

seq.esp.esp <- db_precios_esp %>%
  filter(year(fecha) == 2020) %>%
  mutate(semestre = as.factor(semestre),
         polimero = as.factor(polimero)) %>%
  group_by(semestre, polimero) %>%
  summarise(ratio = mean(ratio),
            sequedad = mean(sequedad),
            tasaCaptura = mean(tasaCaptura),
            costo.especifico = mean(costo.especifico)) %>%
  ggplot(aes(x = semestre, y = sequedad, fill = polimero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "Sequedad (%)",
                     limits = c(12,16),
                     oob = scales::squish,
                     breaks = seq(12, 16, 2)) +
  labs(title = "Sequedad por polímero y semestre",
       subtitle = "Espesamiento")
ggsave(seq.esp.esp, file="seq_esp_esp.png", width = 13, height = 10, units = "cm", dpi=320)


# DESHIDRATACION
df_deshidratacion <- df_deshidratacion %>%
  mutate(direccion = as.factor(direccion),
         mes = month(fecha)) %>%
  filter(direccion == "Deshidratacion") %>%
  filter(ratio > 5,
         sequedad > 20,
         tasaCaptura > 50) %>%
  mutate(semestre = ceiling(mes/3))

db_precios_dh <- inner_join(df_deshidratacion, precio_polimeros, by = "polimero") %>%
  mutate(costo.especifico = ratio * precio / (tasaCaptura / 100)) %>%
  select(fecha, semestre, polimero, ratio, sequedad, tasaCaptura, precio, costo.especifico)

costo.esp.dh <- db_precios_dh %>%
  filter(year(fecha) == 2020) %>%
  mutate(semestre = as.factor(semestre),
         polimero = as.factor(polimero)) %>%
  group_by(semestre, polimero) %>%
  summarise(ratio = mean(ratio),
            sequedad = mean(sequedad),
            tasaCaptura = mean(tasaCaptura),
            costo.especifico = mean(costo.especifico)) %>%
  ggplot(aes(x = semestre, y = costo.especifico, fill = polimero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "Costo Especifico (USD/ton)",
                     limits = c(20,30),
                     oob = scales::squish,
                     breaks = seq(20, 32, 2)) +
  labs(title = "Costos polímero por semestre",
       subtitle = "Deshidratación")
ggsave(costo.esp.dh, file="costo_esp_dh.png", width = 13, height = 10, units = "cm", dpi=320)

seq.esp.dh <- db_precios_dh %>%
  filter(year(fecha) == 2020) %>%
  mutate(semestre = as.factor(semestre),
         polimero = as.factor(polimero)) %>%
  group_by(semestre, polimero) %>%
  summarise(ratio = mean(ratio),
            sequedad = mean(sequedad),
            tasaCaptura = mean(tasaCaptura),
            costo.especifico = mean(costo.especifico)) %>%
  ggplot(aes(x = semestre, y = sequedad, fill = polimero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "Sequedad (%)",
                     limits = c(24,30),
                     oob = scales::squish,
                     breaks = seq(20, 32, 2)) +
  labs(title = "Sequedad por polímero por semestre",
       subtitle = "Deshidratación")
ggsave(seq.esp.dh, file="seq_esp_dh.png", width = 13, height = 10, units = "cm", dpi=320)


df_pol_stock %>%
  filter(proceso == "Espesamiento",
         year(fecha_inicio) == 2020) %>%
  mutate(rango_fecha = as.numeric(fecha_termino - fecha_inicio + 1)) %>%
  mutate(dia_mes = day(fecha_inicio)) %>%
  group_by(fecha_inicio, fecha_termino, polimero) %>%
  summarise(consumo = sum(cantidad),
            rango_fecha = mean(rango_fecha)) %>%
  mutate(mes = month(fecha_inicio),
         dia_mes = day(fecha_inicio)) %>%
  ungroup(fecha_inicio) %>%
  group_by(mes) %>%
  mutate(consumo_rango = if_else(dia_mes == 1, consumo, lag(consumo, 0) - lag(consumo, 1)),
         consumo_dia = consumo_rango / rango_fecha) %>%
  #filter(year(fecha_inicio) == year) %>%
  ungroup(mes) %>%
  group_by(mes, polimero) %>%
  summarise(consumo = sum(consumo_rango))

if (!is.na(year)){
  .tmp_stock_dia <- .tmp_stock_dia %>%
    filter(year(fecha_inicio) == year)
}
