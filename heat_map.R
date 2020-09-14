library(geobr)
mg<-read_state(code_state ='MG')
#caso queira visualizar, basta executar plot(mg)

library(readr)
setwd('E:/GitHub/Mpa_de_calor/Mapa_de_calor')
temperatura_minasgerais <- read_delim("temperatura.txt", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)

library(sf)
temperatura_minasgerais.sf <- st_as_sf(temperatura_minasgerais,coords = c('Longitude','Latitude'),crs=4674) #o código 4674 é uma referência as coordenadas, esse é o código utilizado para o Brasil e outros países próximos.

library(dplyr)
estrutura.mg <-st_make_grid(mg,cellsize = c(.07,.07)) %>% 
  st_as_sf() %>%
  filter(st_contains(mg,.,sparse = FALSE))

plot(estrutura.mg)

library(gstat)
modelo<-gstat(formula = temp~1,
              data = as(temperatura_minasgerais.sf,'Spatial'),
              set=list(idp=3))


temp.interpolacao <- predict(modelo,as(estrutura.mg,'Spatial')) %>%
  st_as_sf()

library(ggplot2)
library(fields) #biblioteca para usar a paleta de cores tim.colors
ggplot(temp.interpolacao) + 
  geom_sf(aes(fill=var1.pred,col=var1.pred))+
  geom_sf(data=mg,fill='transparent')+
  scale_color_gradientn(colors = tim.colors(50),
                        limits=c(19,28))+
  scale_fill_gradientn(colors = tim.colors(50),
                       limits=c(19,28))+
  theme_bw()+
  labs(title = "Dados Interpolados",
       fill ='ºC',
       color= 'ºC')
plot(estrutura.mg)
