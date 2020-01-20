##########################################################################-
#### REGRESION LOGISTICA APLICADA

# Paquetes
require('tidyverse')
require('grid')
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
require('betareg')

# Librería Predeterminada
setwd(file.path('F:','Documentos','Estudio - Documentos',
                'Estadistica_2013-2','Regresion','7. Modelos GLM',
                'Binominal','Reacciones_Morfina'))

df1 <- read.csv(file = './Datos/Somnolencia.csv', header = T, sep = ',', dec = '.')

# Regresión Beta
br1 <- betareg(formula = Respuesta ~ Dosis, 
               data = df1[df1[,'Farmaco']== 'Meperidina',])


br2 <- betareg(formula = Respuesta ~ Dosis, 
               data = df1[df1[,'Farmaco']== 'Morfina',])

# Datos Nuevos
newdata2 <- with(df1, data.frame(Dosis = rep(seq(from = 0.07, to = 4.0, by = 0.01),2)))
newdata2[,'Farmaco'] = rep(x = c('Meperidina', 'Morfina'), each = dim(newdata2)[1]/2)

# Predicciones Regresión Beta
for(i in 1:dim(newdata2)[1]){
  if (newdata2[i,'Farmaco'] == 'Meperidina') {
    newdata2[i,'fit'] = predict(br1, newdata = newdata2[i,], type = 'response')
  } else {
    newdata2[i,'fit'] = predict(br2, newdata = newdata2[i,], type = 'response')
  }
}

# Desviación Estandar Regresión Beta
for(i in 1:dim(newdata2)[1]){
  if (newdata2[i,'Farmaco'] == 'Meperidina') {
    newdata2[i,'se.fit'] = predict(br1, newdata = newdata2[i,], type = 'variance') %>% sqrt(.)
  } else {
    newdata2[i,'se.fit'] = predict(br2, newdata = newdata2[i,], type = 'variance') %>% sqrt(.)
  }
}

# Gráfico de Puntos
# Morfina - Gráfico
# GA <- 
  ggplot() + 
  geom_point(data = df1[df1[,'Farmaco']=='Morfina',], 
             aes(x = Dosis, y = Respuesta, group = Farmaco),shape = 8, col = 'red2') + 
  geom_line(data=newdata2[newdata2[,'Farmaco']=='Morfina',], 
            aes(x = Dosis, y = fit, group = Farmaco), col = 'red2') +
  scale_x_continuous(breaks = seq(0.05,0.6,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Somnolencia')+
  annotate(geom = 'label', x = 0.5, y = 0.2, label = 'Morfina') +
  theme_classic() +
  theme(panel.border = element_rect(colour = 'black', fill = NA))+
  coord_cartesian(xlim = c(0.05,0.6)) 

# Meperidina - Gráfico
# GB <- 
  ggplot() + 
  geom_point(data = df1[df1[,'Farmaco']=='Meperidina',], 
             aes(x = Dosis, y = Respuesta, group = Farmaco),shape = 8, col = 'blue3') + 
  geom_line(data=newdata2[newdata2[,'Farmaco']=='Meperidina',], 
            aes(x = Dosis, y = fit, group = Farmaco), col = 'blue3') +
  labs(x = 'Dosis (mg/kg)', y = 'Somnolencia') +
  scale_x_continuous(breaks = seq(0.00,3.5,0.5)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  theme_classic() + 
  annotate(geom = 'label', x = 2.5, y = 0.2, label = 'Meperidina') +
  theme(panel.border = element_rect(colour = 'black', fill = NA))+
  coord_cartesian(xlim = c(0.3,3.5)) 

