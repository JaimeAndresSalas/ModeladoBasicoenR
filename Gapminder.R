library(tidyverse)
library(modelr)
library(gapminder)
?gapminder
gapminder%>%
  ggplot(aes(year,lifeExp, group=country))+
  geom_line(alpha=0.2)

ec <- filter(gapminder, country=='Ecuador')

ec%>%
  ggplot(aes(year,lifeExp))+
  geom_line()+
  ggtitle("Full data = ") +
  theme(plot.title = element_text(hjust=0.5))


ec_mod <- lm(lifeExp~year, data=ec)

ec%>%
  add_predictions(ec_mod)%>%
  ggplot(aes(year,pred))+
  geom_line()+
  ggtitle("Linear Trend")+
  theme(plot.title = element_text(hjust = 0.5))

ec%>%
  add_residuals(ec_mod)%>%
  ggplot(aes(year, resid))+
  geom_hline(yintercept = 0, color="white", size=3)+
  geom_line()+
  ggtitle("Residual Pattern")+
  theme(plot.title = element_text(hjust = 0.5))
