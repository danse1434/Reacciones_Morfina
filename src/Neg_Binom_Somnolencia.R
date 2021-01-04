##########################################################################-
#### REGRESION LOGISTICA APLICADA

# Paquetes
require('tidyverse')
require('grid')
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
require('betareg')

##########################################################################-
# Introducción ------------------------------------------------------------
##########################################################################-

setwd(file.path('F:','Documentos','Estudio - Documentos',
                'Estadistica_2013-2','Regresion','7. Modelos GLM',
                'Binominal','Reacciones_Morfina'))

df1 <- read.csv(file = './Datos/Somnolencia.csv', header = T, sep = ',', 
                dec = '.')

##########################################################################-
# Regresión Beta ---------------------------------------------------------- 
##########################################################################-

br1 <- df1 %>% 
  filter(Farmaco == 'Meperidina') %>% 
  betareg(formula = Respuesta ~ Dosis, 
               data = .)

br2 <- df1 %>% 
  filter(Farmaco == 'Morfina') %>% 
  betareg(formula = Respuesta ~ Dosis, 
          data = .)


##########################################################################-
# Predicción Modelo -------------------------------------------------------
##########################################################################-
# Datos Nuevos
newdata2 <- data.frame(Dosis = rep(seq(from = 0.01, to = 4.0, by = 0.01),2))
newdata2$Farmaco = rep(x = c('Meperidina', 'Morfina'), 
                       each = dim(newdata2)[1]/2)

# Predicciones Regresión Beta
newdata2 <- newdata2 %>% 
  mutate(Fit = case_when(
    Farmaco == 'Meperidina' ~ predict(br1, newdata = newdata2, 
                                      type = 'response'),
    Farmaco == 'Morfina' ~ predict(br2, newdata = newdata2, 
                                   type = 'response'),
    TRUE ~ NA_real_
  )) %>% 
  mutate(se.fit = case_when(
    Farmaco == 'Meperidina' ~ predict(br1, newdata = newdata2, 
                                      type = 'variance'),
    Farmaco == 'Morfina' ~ predict(br2, newdata = newdata2, 
                                   type = 'variance'),
    TRUE ~ NA_real_
  ))

newdata2 <- newdata2 %>% 
  mutate(LI = case_when(
    Farmaco == 'Meperidina' ~ predict(br1, newdata = newdata2, 
                                      type = 'quantile', at = c(0.025)), 
    Farmaco == 'Morfina' ~ predict(br2, newdata = newdata2, 
                                   type = 'quantile', at = c(0.025)), 
    TRUE ~ NA_real_
  )) %>%
  mutate(LS = case_when(
    Farmaco == 'Meperidina' ~ predict(br1, newdata = newdata2, 
                                      type = 'quantile', at = c(0.975)), 
    Farmaco == 'Morfina' ~ predict(br2, newdata = newdata2, 
                                   type = 'quantile', at = c(0.975)), 
    TRUE ~ NA_real_
  ))

##########################################################################-
# Gráfico de Puntos  ------------------------------------------------------
##########################################################################-
# Morfina - Gráfico
GA <- df1 %>% 
  ggplot(mapping = aes(x = Dosis, y = Respuesta, group = Farmaco, 
                       col = Farmaco), 
         shape = 8) + 
  geom_ribbon(data = newdata2, aes(x = Dosis, ymin = LI, ymax = LS, 
                                   fill = Farmaco), alpha = 0.1,
              inherit.aes = F) +
  geom_point() +
  geom_line(data=newdata2, aes(y = Fit)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Somnolencia')+
  theme_classic() +
  facet_grid(.~Farmaco) +
  theme(panel.border = element_rect(colour = 'black', fill = NA),
        legend.position = 'none') 
  



newdata3 <- newdata2 %>% 
  filter(Farmaco == 'Morfina' & Dosis < 0.7 | 
           Farmaco == 'Meperidina')


GB <- 
  df1 %>% 
  ggplot(mapping = aes(x = Dosis, y = Respuesta, group = Farmaco, 
                       col = Farmaco), 
         shape = 8) + 
  geom_ribbon(data = newdata3, aes(x = Dosis, ymin = LI, ymax = LS, 
                                   fill = Farmaco), alpha = 0.1,
              inherit.aes = F) +
  geom_point() +
  geom_line(data=newdata3, aes(y = Fit)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Somnolencia')+
  theme_classic() +
  facet_grid(.~Farmaco, scales = 'free_x') +
  theme(panel.border = element_rect(colour = 'black', fill = NA),
        legend.position = 'none') 


ggsave(filename = './Figuras/Somnolencia_NB.pdf', device = 'pdf',
       plot = GB,
       width = 7,height = 4, units = 'in')  
  
# df1 %>% 
#   group_by(Farmaco) %>% 
#   summarise(min=min(Dosis),
#             max=max(Dosis))
  
  

  
  