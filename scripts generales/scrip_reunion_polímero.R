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

#Funciones básicas
source("T:/PROCESOS/18. Seguimientos/Varios/funciones.R")

#################################################################################################################
# LIBRERIAS R
#################################################################################################################

# Lista de paquetes installados
paquetes_utilizar("stringr"     # Manipulación de strings
                  ,"ggplot2"    # Paquete con funciones para generar gráficos 
                  ,"dplyr"      # Herramientas para realizar operaciones en dataframes
                  ,"lubridate"  # Herramientas de fecha y hora
                  ,"tidyr"
)
rm(paquetes_utilizar)

#################################################################################################################
# CARGAR DATOS ESPESAMIENTO
#################################################################################################################

# Se cargan las funciones para cargar los distintos dataframe disponibles en el sistema
source("T:/PROCESOS/18. Seguimientos/Varios/fun_data.R")

datosEsp <- LoadDFCentrifugasEsp()
datosResultadosEsp <- LoadDFClasificacionEsp()
datosDH <- LoadDFCentrifugasDH()
datosResultadosDH <- LoadDFClasificacionDH()
datosInPre <- LoadDFIngresoPre()
datosOutPre <- LoadDFSalidaPre()

# Se remoueven los workbooks que no se van a modificar
rm("wb.espesamiento", "wb.deshidratacion")

# Se remoueven las funciones que no se utilizarán
rm(list=setdiff(lsf.str(), c("resultado_medio", "resultado_negativo", "resultado_positivo",
                             "CreaAsignaWD",
                             "RangosOperacionEsp",
                             "resultado")))

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
source("T:/PROCESOS/18. Seguimientos/Varios/tema_impresion.R")

#################################################################################################################
# ESPESAMIENTO
#################################################################################################################

# Base inicial. Se filtra por disponibilidad y dirección
datosEspCambi <- datosEsp %>%
  filter(indice == 1,
         direccion == "CAMBI")

# Imputan los valores faltantes como el promedio de los 3 últimos valores
for(i in 1:dim(datosEspCambi)[1]){
  if(is.na(datosEspCambi$sequedad[i])){
    datosEspCambi$sequedad[i] <- sum(
      datosEspCambi$sequedad[i-1],
      datosEspCambi$sequedad[i-2],
      datosEspCambi$sequedad[i-3]
    )/3
  }
  if(is.na(datosEspCambi$tasaCaptura[i])){
    datosEspCambi$tasaCaptura[i] <- sum(
      datosEspCambi$tasaCaptura[i-1],
      datosEspCambi$tasaCaptura[i-2],
      datosEspCambi$tasaCaptura[i-3]
    )/3
  }
}

# Datos del lodo de estrada:
# MS
datosEspCambi %>%
  group_by(fecha) %>%
  summarise(concMS = mean(concMS),
            concMV = mean(concMV)) %>%
  mutate(prom.MS.7d = (lag(concMS, 0) +
                         lag(concMS, 1) +
                         lag(concMS, 2) +
                         lag(concMS, 3) +
                         lag(concMS, 4) +
                         lag(concMS, 5) +
                         lag(concMS, 6))/7) %>%
  mutate(prom.MV.7d = (lag(concMV, 0) +
                         lag(concMV, 1) +
                         lag(concMV, 2) +
                         lag(concMV, 3) +
                         lag(concMV, 4) +
                         lag(concMV, 5) +
                         lag(concMV, 6))/7) %>%
  filter(year(fecha) == 2020) %>%
  ggplot() +
  geom_line(aes(x = fecha, y = prom.MS.7d), size = 1, color = "orangered4") +
  labs(x = "Fecha", y = "Promedio movil MS (g/L)")

# MV
pre.esp.mv <- datosEspCambi %>%
  group_by(fecha) %>%
  summarise(concMS = mean(concMS),
            concMV = mean(concMV)) %>%
  mutate(prom.MS.7d = (lag(concMS, 0) +
                         lag(concMS, 1) +
                         lag(concMS, 2) +
                         lag(concMS, 3) +
                         lag(concMS, 4) +
                         lag(concMS, 5) +
                         lag(concMS, 6))/7) %>%
  mutate(prom.MV.7d = (lag(concMV, 0) +
                         lag(concMV, 1) +
                         lag(concMV, 2) +
                         lag(concMV, 3) +
                         lag(concMV, 4) +
                         lag(concMV, 5) +
                         lag(concMV, 6))/7) %>%
  filter(year(fecha) == 2020) %>%
  ggplot() +
  geom_line(aes(x = fecha, y = prom.MV.7d), size = 1, color = "cyan4") +
  geom_point(aes(x= fecha, y = concMV), size = 1, color = "blue4") +
  scale_y_continuous(limits = c(82,89)) +
  labs(x = "Fecha", y = "Promedio movil MV (%)")

ggsave(pre.esp.mv, file="pre_esp_MV.png", width = 25, height = 8, units = "cm", dpi=320)

# Evoluación mantos
datosMantos <- datosOutPre %>%
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
                       lag(total.mantos,6))/7) %>%
  filter(month(fecha) %in% c(1,2,3,4)) %>%
  mutate(fecha = substring(as.character(fecha), first=6, last=10)) %>%
  mutate(fecha = factor(fecha, ordered = TRUE))

pre.mantos <- ggplot(tmp.mantos, aes(x=fecha, y=total.mantos.7d, group=1)) +
  geom_line(data = subset(tmp.mantos, year %in% c("2019")), aes(color="Blue"), size=1) +
  geom_line(data = subset(tmp.mantos, year %in% c("2020")), aes(color="Red"), size=1) +
  labs(x = "Fecha", y = "Suma total mantos de lodo (m)") +
  labs(colour="year") +
  scale_color_discrete(labels = c("2019", "2020")) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01"), 
                   labels = c("Enero", "Febrero", "Marzo", "Abril"))

ggsave(pre.mantos, file="pre_mantos.png", width = 25, height = 12, units = "cm", dpi=320)


  datosMantos %>%
    mutate(year = factor(year(fecha))) %>%
    mutate(manto_pm7 = (lag(total.mantos,0) +
                          lag(total.mantos,1) +
                          lag(total.mantos,2) +
                          lag(total.mantos,3) +
                          lag(total.mantos,4) +
                          lag(total.mantos,5) +
                          lag(total.mantos,6))/7) %>%
    ggplot() +
    geom_line(aes(x=fecha, y=manto_pm7), colour="Brown4", size=1) +
    labs(x = "Fecha", y = "Suma total mantos de lodo (m)")
  scale_x_date(date_breaks = "1 month", date_labels = "%B", limits = c(today()-106, today()-1))  
  
### Resultados generales Sequedad, Tasa de Captura y Ratios según SCADA
# Base para los promedios móviles
df.prommobil.esp <- datosEspCambi %>%
  filter(fecha < date("2020-04-19"))  %>%
  mutate(sequedad = if_else(is.na(sequedad), (lag(sequedad, 1) + lag(sequedad, 2) + lag(sequedad, 3))/3, sequedad),
         tasaCaptura = if_else(is.na(tasaCaptura), (lag(tasaCaptura, 1) + lag(tasaCaptura, 2) + lag(tasaCaptura, 3))/3, tasaCaptura)
        ) %>%
  group_by(fecha) %>%
  summarise(polimero.Total = sum(poliMS),
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
  mutate(sequedad.dia = sequedad.Total / lodo.Total) %>%
  mutate(sum.sequedad.7d = (lag(sequedad.Total, 0) +
                                  lag(sequedad.Total, 1) +
                                  lag(sequedad.Total, 2) +
                                  lag(sequedad.Total, 3) +
                                  lag(sequedad.Total, 4) +
                                  lag(sequedad.Total, 5) +
                                  lag(sequedad.Total, 6))) %>%
  mutate(tasaCap.dia = tasaCap.Total / lodo.Total) %>%
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
sequedad.esp <- df.prommobil.esp %>%
  ggplot() +
  geom_line(aes(fecha, prom.sequedad.7d), color ="Blue", size = 1) +
  geom_point(aes(fecha, sequedad.dia)) +
  geom_segment(y = 16, yend = 16, x = date("2020-01-01"), xend = date("2020-04-18"), color = "red") +
  labs(x = "Fecha", y = "Sequedad Esp. (%)")

ggsave(sequedad.esp, file="sequedad_esp.png", width = 30, height = 6, units = "cm", dpi=320)
  
# Figura Promedio Movil Tasa de Captura
tasa.cap.esp <- df.prommobil.esp %>%
  ggplot() +
  geom_line(aes(fecha, prom.tasaCap.7d), color ="Blue", size = 1) +
  geom_point(aes(fecha, tasaCap.dia)) +
  geom_segment(y = 90, yend = 90, x = date("2020-01-01"), xend = date("2020-04-18"), color = "red") +
  labs(x = "Fecha", y = "Tasa de Captura Esp. (%)") +
  scale_y_continuous(limits = c(85,100))

ggsave(tasa.cap.esp, file="tasa_cap_esp.png", width = 30, height = 6, units = "cm", dpi=320)

# Figura Promedio Movil Ratio
ratio.esp <- df.prommobil.esp %>%
  ggplot() +
  geom_line(aes(fecha, prom.ratio.7d), color ="Blue", size = 1) +
  geom_point(aes(fecha, ratio.dia)) +
  geom_segment(y = 12.0, yend = 12.0, x = date("2020-01-01"), xend = date("2020-04-18"), color = "red") +
  labs(x = "Fecha", y = "Ratio Esp. (kg/ton)") +
  scale_y_continuous(limits = c(4,17))

ggsave(ratio.esp, file="ratio_esp.png", width = 30, height = 6, units = "cm", dpi=320)

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
  

#################################################################################################################
# DESHIDRATACION
#################################################################################################################

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







