library(ggplot2)

ggplot() +
  geom_rect(aes(xmin = 15, xmax = 17, ymin = 90, ymax = 100), fill = "turquoise", alpha = 1 ) +
  geom_rect(aes(xmin = 13, xmax = 15, ymin = 90, ymax = 100), fill = "yellow3", alpha = 1 ) +
  geom_rect(aes(xmin = 15, xmax = 17, ymin = 80, ymax = 90), fill = "yellow3", alpha = 1 ) +
  geom_rect(aes(xmin = 13, xmax = 15, ymin = 80, ymax = 90), fill = "tomato3", alpha = 1 ) +
  geom_rect(aes(xmin = 17, xmax = 19, ymin = 80, ymax = 90), fill = "tomato3", alpha = 1 ) +
  geom_rect(aes(xmin = 17, xmax = 19, ymin = 90, ymax = 100), fill = "springgreen3", alpha = 1 ) +
  annotate("text", x = 16, y = 95, label = "Positivo") +
  annotate("text", x = 14, y = 95, label = "Medio") +
  annotate("text", x = 16, y = 85, label = "Medio") +
  annotate("text", x = 14, y = 85, label = "Negativo") +
  annotate("text", x = 18, y = 85, label = "Negativo") +
  annotate("text", x = 18, y = 95, label = "Exceso") +
  labs(title="Rango de clasificación") + labs(x="Sequedad (%)", y = "Tasa de Captura (%)")
  
  
  
