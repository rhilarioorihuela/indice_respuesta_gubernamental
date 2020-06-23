install.packages("dplyr")
library(dplyr)
library(ggplot2)
#Descargar DB Indice de respuesta gubernamental
db_irg <- read.csv("D:/ClasesR/tarea/indice_respuesta_gubernamental.csv", sep =";")
#Nombres de Columnas
colnames(db_irg) <- c("date","valor")
#Cambio de Formato de la columna
db_irg$date <- as.Date(db_irg$date,format="%d/%m/%Y")
db_irg$months <- as.factor(format(db_irg$date,'%B'))
db_irg$date <- as.character(db_irg$date)
names(db_irg)
View(db_irg)
str(db_irg)
#-------Función fechas ----------------------------------
select_dates = function(fecha_inicio,fecha_termino){
  db_temp <- db_irg  %>%
    filter(date >= fecha_inicio) %>%
    filter(date <= fecha_termino)
  db_temp
}

#-------Graficar-------------------------------------
db_1 = select_dates("2020-02-01","2020-05-18")
db_1

ggplot(db_1, aes(date, valor,group = 1)) + geom_line()

g <- ggplot(db_1) + aes(date, valor,group = 1) + geom_line(size = 2, aes(color = months)) + labs(x = "Fechas", y = "Indice de Respuesta Gubernamental", title = "Indice de Respuesta Gubernamental en Cuarentea - PERÚ")
g <- g  + theme(axis.text.x = element_text(angle = -90, vjust = 0.5),plot.background = element_rect(fill = "grey80", colour = NA))
g <- g + geom_vline(xintercept = 32, linetype="dashed", color="red",size = 0.5 ) + geom_vline(xintercept = 62, linetype="dashed", color="red",size = 0.5) + annotate(geom = "label", x = 62, y = 100, label = "97") + annotate(geom = "label", x = 32, y = 95, label = "92")
g


