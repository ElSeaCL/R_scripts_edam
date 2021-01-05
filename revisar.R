clas_esp <- read.xlsx("T:/PROCESOS/18. Seguimientos/Espesamiento/src/categoria_resultados_esp.xlsx",
                     detectDates = TRUE)
clas_dh <- read.xlsx("T:/PROCESOS/18. Seguimientos/Deshidratacion/src/categoria_resultados_dh.xlsx",
                     detectDates = TRUE)

cent <- "A"
best_cent <- clas_esp %>%
  mutate(resultado = as.factor(resultado),
         semana = as.factor(semana)) %>%
  filter(ano == 2020,
         centrifuga == cent) %>%
  ggplot() +
  geom_col(aes(x=semana, y= total, fill=resultado), position = "fill") +
  labs(title = "Proporción de resultados por semana",
       subtitle = paste0("Centr\u00EDfuga ", cent)
       )
ggsave(best_cent,file=paste(c("best_dh.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)

total_cent <- clas_dh %>%
  mutate(resultado = as.factor(resultado),
         semana = as.factor(semana)) %>%
  filter(ano == 2020) %>%
  group_by(centrifuga, resultado) %>%
  summarise(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x=centrifuga, y= total, fill=resultado), position = "fill") +
  labs(title = "Proporción de resultados a la fecha")
ggsave(total_cent,file=paste(c("total_dh.png"), sep = "", collapse = ""),
       width = 9, height = 9, units = "cm", dpi=320)

setwd("T:/PROCESOS/18. Seguimientos/Deshidratacion/src")
best_ratio <- .tmp_esp %>%
  filter(ano == 2020,
         indice == 1,
         centrifuga == cent) %>%
  ggplot() +
  geom_point(aes(x=fecha, y=ratio), color = "Blue") +
  labs(x="Fecha", y="Ratio (kg/ton)", title="Ratio diario", 
       subtitle = paste0("Centrífuga ", cent))
ggsave(best_ratio,file=paste(c("worst_esp.png"), sep = "", collapse = ""),
       width = 15, height = 7, units = "cm", dpi=320)
  
  
  
.tmp_plot <- ggplot(.tmp_data) +
  geom_line(data=.tmp_df, aes_string(x=fecha, y= atributo_prom), size=1, color="Blue") +
  geom_point(data=.tmp_df, aes_string(x= fecha, y=atributo_mean), color="Red") +
  scale_y_continuous(limits = limits_y) +
  labs(x="Fecha", y="Ratio (kg/ton)") +
  axis +
  title

ratio_esp <- .tmp_esp %>%
  filter(ano == 2020,
         indice == 1,
         direccion == "CAMBI",
         centrifuga == cent) %>%
  mutate(polTotal = ratio*flujoMS) %>%
  group_by(centrifuga) %>%
  summarise(polTotal = sum(polTotal),
            flujoMS = sum(flujoMS)) %>%
  mutate(ratio.ponderado = polTotal / flujoMS) %>%
  ggplot() +
  geom_col(aes(centrifuga, y=ratio.ponderado), fill="darkorange4") +
  labs(title="Ratio ponderado a la fecha",
       x="Centrífuga", y="Ratio (kg/ton)")

ggsave(ratio_esp,file=paste(c("ratio_esp.png"), sep = "", collapse = ""),
       width = 9, height = 9, units = "cm", dpi=320)
  