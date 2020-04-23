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
  mutate(wday = wday(date, label=TRUE))-> daily

daily%>%
  ggplot(aes(wday,n))+
  geom_boxplot()
# Del grafico se ven que existen un gran numero de vuelos en los dias laborales.
# Hipotesis, los vuelos se hacen mas de manera por trabajo
# Modelo de vuelos por dia de la semana

mod <- lm(n~ wday, data=daily)

grid <- daily%>%
  data_grid(wday)%>%
  add_predictions(mod,"n")

daily%>%
  ggplot(aes(wday,n))+
  geom_boxplot()+
  geom_point(data= grid, color="red", size=4)


#Visualizando el residuo

daily <- daily%>%
  add_residuals(mod)
daily%>%
  ggplot(aes(date, resid))+
  geom_ref_line(h= 0)+
  geom_line()
#Visualizando los residuos se puede ver picos grandes, es decir que el modelo tiene picos muy altos
# Que se pueden ver que son temporales por el mes del ano, es decir que depende de la temporada
# COmo es de esperarse hay temporadas altas y bajas en el transito aereo 
# Es importante buscar una manera de corregir el modelo segun la temporada

# Suavisaremos la curva quitando los dias que son festivos en USA

daily%>%
  ggplot(aes(date, resid))+
  geom_ref_line(h=0)+
  geom_line(color = "grey30")+
  geom_smooth(se= TRUE, span= 0.2)
#Comenzaremos buscando mejorar datos de los sabados
daily%>%
  filter(wday=="sáb")%>%
  ggplot(aes(date,n))+
  geom_point()+
  geom_line()+
  scale_x_date(NULL, date_breaks = "1 month", date_labels = '%b')

#Vacaciones en ese ano USa 26 Junio - 9 Septiembre

# 5 Junio - 25 Agosto

term <- function(date){
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels= c("spring", "summer", "fall")
  )
}

daily <- daily %>%
  mutate(term= term(date))

daily%>%
  filter(wday=="sáb")%>%
  ggplot(aes(date, n, color= term))+
  geom_point(alpha=0.25)+
  geom_line()+
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# Este grafico nos permite ver como varian cada dia segun el termino del ano
# Se ve claramente que no es una relacion lineal, depende mucho del termino del ano 
daily%>%
  ggplot(aes(wday,n, color=term))+
  geom_boxplot()

#Creacion de los dos modelos uno lineal, y otro con la interaccion entre dia y termino del ano
mod1 <- lm(n~ wday, data=daily)
mod2 <- lm(n~ wday*term, data=daily)

daily%>%
  gather_residuals(without_term= mod1, with_term= mod2)%>%
  ggplot(aes(date, resid, color=model))+
  geom_line(alpha=0.5)
# Se ve claramente como mejora considerando el trimestre del ano
# Por lo cual se ve claramente que facilita el modelado usando los trismestre del ano

grid <- daily%>%
  data_grid(wday, term)%>%
  add_predictions(mod2,"n")

daily%>%
  ggplot(aes(wday,n))+
  geom_boxplot()+
  geom_point(data=grid, color="red")+
  facet_wrap(~term)
# Aun existe algunos outliers, sera necesario usar un modelo lineal robusto?

#Modelo lineal robusto ->> Elimina efecto de Outliers
mod3 <- MASS::rlm(n~ wday*term, data=daily)
daily%>%
  add_residuals(mod3, "resid")%>%
  ggplot(aes(date, resid))+
  geom_hline(yintercept = 0, size=2, color="white")+
  geom_line()
