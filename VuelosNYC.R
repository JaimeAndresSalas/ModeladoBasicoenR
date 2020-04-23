library(tidyverse)
library(modelr)
library(nycflights13)
library(lubridate)

#En este ejercicio se realizara un modelado de los vuelos que entran en el 
#Aeropuerto de NYC, sabiendo que afecta al numero de vuelos en un dia

daily <- flights%>%
  mutate(date= make_date(year, month, day))%>%
  group_by(date)%>%
  summarise(n =n())
daily

daily%>%
  ggplot(aes(date, n))+
  geom_line() #Representacion de los vuelos en un ano

# Por el grafico hacemos hipotesis que lo mas representativo es el dia de la semana

daily%>%
  mutate(wday())
