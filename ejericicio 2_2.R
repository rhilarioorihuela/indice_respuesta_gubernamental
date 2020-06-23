install.packages("dplyr")
library(dplyr)
library(ggplot2)
#Descargar DB Indice de respuesta gubernamental
db_general <- read.csv("D:/ClasesR/tarea/indice_deceso_data.csv", sep =";")
db_general$Departamento <- as.factor(db_general$Departamento)
db_general
str(db_general)
View(db_general)
#Quitamos los valores de Perú para sincerar la base de datos
data_Peru <- db_general[1,]
data_Peru
db_general <- db_general[-1,]
db_general
#Graficas
g <- ggplot(data = db_general, aes(x = Total, y = Departamento, color = Departamento, fill = Departamento)) + geom_bar( stat = "identity") 
g <- g + geom_text(aes(label = Total), vjust=0.3, hjust=-0.1, color="black", size=3.5) + labs(x = "N° Total de Fallecidos", y = "Departamentos", title = "Probabilidad de deceso po NCD y COvid-19 - Departamentos del Perú")
g <- g + theme(legend.position="top")
g
g1 <- ggplot(data = db_general, aes(x = Departamento, y = porcentaje_deceso, group = 1)) + geom_point(data= db_general, group = 1, size = 2, aes(x = Departamento, y = porcentaje_deceso))
g1 <- g1 + geom_smooth(size = 1) + theme(axis.text.x = element_text(angle = -90, vjust = 0.5),plot.background = element_rect(fill = "grey80", colour = NA))
g1 <- g1 + labs(x = "Departamentos", y = "% de Deceso por NCD y Covid-19", title = "Probabilidad de deceso po NCD y COvid-19 - Departamentos del Perú")
g1 <- g1 + geom_hline(yintercept = 0.93, linetype="dashed", color="red",size = 0.5 ) + annotate(geom = "text", x = 25, y = 1, label = "% Deceso-Perú (0.93)", color = "red")
g1 <- g1 + annotate(geom = "curve", x = 23.5, y = 1, xend = 20, yend = 0.93, curvature = .3, arrow = arrow(length = unit(1, "mm")))
g1

