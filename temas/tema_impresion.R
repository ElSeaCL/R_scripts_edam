#################################################################################################################
# TEMA GGPLOT IMPRESIÓN
# Por: Sebastián Gonzalez
# 07-04-2020
#################################################################################################################

#################################################################################################################
# TEMAS FIGURAS
#################################################################################################################

# Formato figuras
.ejes <- element_text(color = "black", size = 12)
.titulo_ejes <- element_text(color = "black", size = 15, face="bold")
.titulo <- element_text(color ="black", face = "bold", size = 16)
.legenda <- element_text(color ="black", size = 10)
.legenda_titulo <- element_text(color ="black", face = "bold", size = 10)

theme_word <- theme(axis.text.x = .ejes,
                    axis.title.x = .titulo_ejes,
                    axis.text.y = .ejes,
                    axis.title.y = .titulo_ejes, 
                    plot.title = .titulo,
                    legend.text = .legenda,
                    legend.title = .legenda_titulo) 

print("Tema para figuras ggplot para impresión cargado satisfactoriamente")