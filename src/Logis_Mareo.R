#### REGRESION LOGISTICA APLICADA

# Paquetes
require('tidyverse')
require('grid'); vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# Librería Predeterminada
setwd(file.path('F:','Documentos','Estudio - Documentos',
                'Estadistica_2013-2','Regresion','7. Modelos GLM',
                'Binominal','Reacciones_Morfina'))

df1 <- read.csv(file = './Datos/Mareo.csv', header = T, sep = ',', dec = '.')

# Función para expandir datos reportados como proporciones en datos originales
Tabla_Expand <- function(data){
  list_1 = list()
  for (i in 1:(dim(data)[1])) {
    df = data.frame(
      Farmaco = rep(x = data[i,'Farmaco'], times = data[i,'N']),
      Dosis = rep(x = data[i,'Dosis'], times = data[i,'N']),
      Respuesta = c(rep(1,round(data[i,'N']*data[i,'Respuesta'])),
                    rep(0,round(data[i,'N']*(1-data[i,'Respuesta']))) ) )
    #rbinom(n = data[i,'N'], size = 1,prob = data[i,'Respuesta'])
    list_1[[i]] = df
  }
  df = do.call(rbind.data.frame, list_1)
  return(df)
}

# Regresión Logarítmica
df2 <- Tabla_Expand(df1)

rlm1 <- glm(Respuesta ~ Dosis, 
            data = df2[df2[,'Farmaco']== 'Meperidina',], 
            family = 'binomial')
rlm2 <- glm(Respuesta ~ Dosis, 
            data = df2[df2[,'Farmaco']== 'Morfina',], 
            family = 'binomial')

# Datos Nuevos
newdata2 <- with(df2, data.frame(Dosis = rep(seq(from = 0.07, to = 4.0, by = 0.01),2)))
newdata2[,'Farmaco'] = rep(x = c('Meperidina', 'Morfina'), each = dim(newdata2)[1]/2)

# Predicciones Regresión Logística
for(i in 1:dim(newdata2)[1]){
  if (newdata2[i,'Farmaco'] == 'Meperidina') {
      newdata2[i,'fit'] = predict(rlm1, newdata = newdata2[i,], type = 'link', se = T)$fit
  } else {
      newdata2[i,'fit'] = predict(rlm2, newdata = newdata2[i,], type = 'link', se = T)$fit
  }
}
# SE Regresión Logística
for(i in 1:dim(newdata2)[1]){
  if (newdata2[i,'Farmaco'] == 'Meperidina') {
    newdata2[i,'se.fit'] = predict(rlm1, newdata = newdata2[i,], type = 'link', se = T)$se.fit
  } else {
    newdata2[i,'se.fit'] = predict(rlm2, newdata = newdata2[i,], type = 'link', se = T)$se.fit
  }
}
# Predicciones Dominio de Probabilidades
newdata2 <- within(newdata2, {
            Pred <- plogis(fit)
            LI <- plogis(fit - (1.96 * se.fit))
            LS <- plogis(fit + (1.96 * se.fit))
})

# Gráfico de Puntos
# Morfina - Gráfico
GA <- 
  ggplot() + 
  geom_point(data = df1[df1[,'Farmaco']=='Morfina',], 
             aes(x = Dosis, y = Respuesta, group = Farmaco),shape = 8, col = 'red2') + 
  geom_point(data = df2[df2[,'Farmaco']=='Morfina',], 
             aes(x = Dosis, y = Respuesta, group = Farmaco), col = 'red2')+
  
  geom_line(data=newdata2[newdata2[,'Farmaco']=='Morfina',], 
            aes(x = Dosis, y = Pred, group = Farmaco), col = 'red2') +
  geom_ribbon(data=newdata2[newdata2[,'Farmaco']=='Morfina',], 
              aes(x=Dosis, ymin = LI, ymax = LS, group = Farmaco), alpha = 0.1, fill = 'red2') +
  scale_x_continuous(breaks = seq(0.05,0.6,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Mareo')+
  annotate(geom = 'label', x = 0.5, y = 0.2, label = 'Morfina') +
  theme_classic() +
  theme(panel.border = element_rect(colour = 'black', fill = NA))+
  coord_cartesian(xlim = c(0.05,0.6)) 

# Meperidina - Gráfico
GB <- 
  ggplot() + 
  geom_point(data = df1[df1[,'Farmaco']=='Meperidina',], 
             aes(x = Dosis, y = Respuesta, group = Farmaco),shape = 8, col = 'blue3') + 
  geom_point(data = df2[df2[,'Farmaco']=='Meperidina',], 
             aes(x = Dosis, y = Respuesta, group = Farmaco), col = 'blue3')+
  geom_line(data=newdata2[newdata2[,'Farmaco']=='Meperidina',], 
            aes(x = Dosis, y = Pred, group = Farmaco), col = 'blue3') +
  geom_ribbon(data=newdata2[newdata2[,'Farmaco']=='Meperidina',], 
              aes(x=Dosis, ymin = LI, ymax = LS, group = Farmaco), alpha = 0.1, fill = 'blue3') +
  labs(x = 'Dosis (mg/kg)', y = 'Mareo') +
  scale_x_continuous(breaks = seq(0.00,3.5,0.5)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  theme_classic() + 
  annotate(geom = 'label', x = 2.5, y = 0.2, label = 'Meperidina') +
  theme(panel.border = element_rect(colour = 'black', fill = NA))+
  coord_cartesian(xlim = c(0.3,3.5)) 

  pdf(file = './Figuras/Mareo.pdf', width = 7,height = 4)
  {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(GA, vp = vplayout(1, 1))
  print(GB, vp = vplayout(1, 2))
  }; dev.off()


# Desviación
# Meperidina
with(rlm1, null.deviance - deviance)
with(rlm1, df.null - df.residual)
with(rlm1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(rlm1)

# Morfina
with(rlm2, null.deviance - deviance)
with(rlm2, df.null - df.residual)
with(rlm2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(rlm2)





















