install.packages("dplyr")
library(dplyr)
library(ggplot2)

download.file( "https://covid.ourworldindata.org/data/owid-covid-data.csv", destfile = "D:/ClasesR/tarea/covid19.csv")
db <- read.csv("D:/ClasesR/tarea/covid19.csv")
db$location <- as.factor(db$location)
View(db)
str(db)


#Ejemplo para comprender la FUnción
date_temp <- "2020-04-01"
date_temp
str(db)
db_temp_1 <- db %>%
  filter( location == "Brazil"
         | location == "Mexico"
         | location == "Peru"
         | location == "United States") %>%
  filter(date >= date_temp) %>%
  select( location, date, total_cases)

db_temp_1
str(db_temp_1)
View(db_temp_1)

#----------FUNCION FECHAS --------------------
date_general = function(fecha){
  date_general <- db  %>%
    filter( location == "World"
    ) %>%
    filter(date >= fecha) %>%
    select( date)
  date_general
}
date_especifico = function(fecha_inicio, fecha_final){
  date_general_doubling <- db  %>%
    filter( location == "World"
    ) %>%
    filter(date <= fecha_inicio) %>%
    filter(date >= fecha_final) %>%
    select( date)
  date_general_doubling
}
#----------FUNCION TOTAL CASOS -----------------
total_casos = function(pais, fecha){
  db_temp <- db %>%
    filter( location == pais) %>%
    filter(date >= fecha) %>%
    select( location, date, total_cases)
  db_temp
}
#----------FUNCION DOUBLING --------------------
valor_duplicacion = function(pais, fecha_inicio, fecha_termino){
  #Selección de la muestra
  date = "2020-04-01"
  Country_cases <- db %>%
    filter(location == pais) %>%
    filter(date >= date) %>%
    select(date, location,total_cases)
  #contando numero de Filas
  nrow(Country_cases)
  #calculando el tiempo de duplicación
  Country_cases_temp <- Country_cases
  Country_doubling <- c(Country_cases[row_number(Country_cases$total_cases)+4,3]/Country_cases[row_number(Country_cases$total_cases),3])
  Country_doubling <- round((5*log(2))/(log(Country_doubling)),2)

  #Quitar los valores NA
  Country_doubling_valor <- Country_doubling[-c(78:81)]
  #Quitar las fechas que no tienen dias de duplicación
  Country_cases_temp <- Country_cases_temp[-(1:4),]
  #reemplazar los valores de total_cases po dias de duplicacion
  Country_cases_temp[3]<- Country_doubling_valor
  #Determina el Intervalo de fechas de analisis
  Country_doubling <- Country_cases_temp  %>%
    filter(date <= fecha_inicio) %>%
    filter(date >= fecha_termino) %>%
    select( date, location, total_cases)
  Country_doubling
}


#------Vista de casos totales-------------------------

Peru_cases = total_casos("Peru","2020-04-01")
Mexico_cases = total_casos("Mexico","2020-04-01")
EE_UU_cases = total_casos("United States","2020-04-01")
Brazil_cases = total_casos("Brazil","2020-04-01")
date_casos_totales = date_general("2020-04-01")
#Mostrar los objetos
Peru_cases
Mexico_cases
EE_UU_cases
Brazil_cases

#-------- Número de Observaciones por cada variable------
cant_row_countries <- data.frame(nrow(date_casos_totales), 
                                 nrow(Brazil_cases),
                                 nrow(EE_UU_cases),
                                 nrow(Mexico_cases),
                                 nrow(Peru_cases))
colnames(cant_row_countries) <- c("date", "Brazil_cases", "EE_UU_cases", "Mexico_cases", "Peru_cases")
cant_row_countries

#------Vista del Indice de Duplicación-----------------------
#Determinamos los datos de las fechas
Peru_doubling = valor_duplicacion("Peru", "2020-06-07", "2020-05-22")
Mexico_doubling = valor_duplicacion("Mexico", "2020-06-07", "2020-05-22")
EE_UU_doubling = valor_duplicacion("United States", "2020-06-07", "2020-05-22")
Brazil_doubling = valor_duplicacion("Brazil", "2020-06-07", "2020-05-22")
date_general_doubling = date_especifico("2020-06-07", "2020-05-22")
Peru_doubling
Mexico_doubling
#Data de EEUU se retrasa en 27 días 
EE_UU_doubling_temp = valor_duplicacion("United States", "2020-05-11", "2020-04-25")
EE_UU_doubling$total_cases <- EE_UU_doubling_temp$total_cases
EE_UU_doubling
#Unir todos los DF en uno para graficar
data_dobling = rbind(Peru_doubling,Mexico_doubling,EE_UU_doubling,Brazil_doubling)
data_dobling

#-------Graficar-------------------------------------

g <- ggplot(data_dobling) + aes(x = date, y = total_cases, colour = location, label=total_cases) + scale_y_log10() + geom_point(alpha = 7, size = 2) + labs(x = "Fechas", y = "Días de Duplicación", title = "Tiempo de duplicación de Covid-19", color = "location")
g + geom_vline(xintercept = 4, linetype="dashed", color="red") + annotate("text", label = "Apertura parcial económica", x = 5.5, y = 10, size = 5, colour = "black") + annotate(geom = "text", x = 17, y = 30, label = "28.83")
g

geom_text(hjust = 0, nudge_x = 0.05)

