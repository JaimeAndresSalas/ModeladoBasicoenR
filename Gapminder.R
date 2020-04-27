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



#DAtaframe anidado

by_country <- gapminder%>%
  group_by(country, continent)%>%
  nest()
by_country%>%View()

by_country $data[[1]]

country_model <- function(df){
  lm(lifeExp~year, data=df)
}

models <- map(by_country$data, country_model)

by_country <- by_country%>%
  mutate(model=map(data,country_model))

by_country%>%
  filter(continent=="Europe")

by_country <- by_country%>%
  mutate(resids= map2(data, model,add_residuals))

by_country

resids <- unnest(by_country, resids)
resids

resids%>%
  ggplot(aes(year,resid))+
  geom_line(aes(group=country),alpha=0.2)+
  geom_smooth(se=FALSE)
resids%>%
  ggplot(aes(year,resid))+
  geom_line(aes(group=country),alpha=0.2)+
  geom_smooth(se=FALSE)+
  facet_wrap(~continent)

#Midiendo modelo con broom
library(broom)

glance(ec_mod)

by_country%>%
  mutate(glance = map(model,glance))%>%
  unnest(glance, .drop =TRUE)
glance%>%
  arrange(r.squared)

glance%>%
  ggplot(aes(continent, r.squared))+
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared<0.25)

gapminder%>%
  semi_join(bad_fit, by="country")

