#### REGRESION LOGISTICA APLICADA

# Paquetes
require(readxl)
require(MASS)
require(betareg)
require(plotly)
require(patchwork)
require(tidymodels)
require(tidyverse)

source(file.path('src', '10_funciones_auxiliares.R'), encoding = 'UTF-8')

theme_set(theme_bw())

df1 <- list.files(file.path('data'), pattern = '*.csv') %>%
  map_df( ~ read_csv(file.path('data', .))) %>% 
  mutate(Reaccion = ifelse(is.na(Reaccion), 'Somnolencia', Reaccion))

# Regresión Logarítmica
df2 <- Tabla_Expand_2(df1)
glimpse(df2)


# par(mfrow=c(1,2))
# df1 %>% filter(Farmaco=='Morfina') %>% plot(Respuesta ~ Dosis, ., col='red', main='Morfina')
# df1 %>% filter(Farmaco=='Meperidina') %>% plot(Respuesta ~ Dosis, ., col='blue', main='Meperidina')
# dev.off()

# Especificación de receta
lr_recipe <- 
  recipe(R ~ Dosis, data=df2)

# Especificación de modelo logístico base
lr_mod1 <- 
  logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode("classification") %>% 
  translate()


lr_workflow_1 <- 
  workflow() %>% 
  add_model(lr_mod1) %>% 
  add_recipe(lr_recipe)

dfT <- df2 %>% 
  group_by(Farmaco, Reaccion) %>% 
  nest() %>% 
  mutate(
    # Tiydmodels just sucks!
    # fit1 = map(data, ~fit(lr_mod1, R ~ Dosis, data = .x))
    fit1 = map(data, ~glm(R~Dosis, data=.x, family = 'binomial'))
    )

dfT <- dfT %>% 
  mutate(
    tidy   = map(fit1, ~tidy(.x, conf.int=TRUE)),
    glance = map(fit1, ~glance(.x)),
    predict= map2(fit1, data, ~createLogPredictions(.x, .y)),
    obs    = map(predict, 'obs'),
    grid   = map(predict, 'grid')
  )


#-------------------------------------------------------------------------------#
# Respuesta de Modelo -------------------------------
#-------------------------------------------------------------------------------#

g1 <- dfT %>% 
  select(Farmaco, Reaccion, grid) %>% 
  unnest(cols = c(grid)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Dosis, 
             colour = Farmaco, 
             )) +
  geom_line(aes(y=pred)) + 
  geom_point(data=df1, aes(y=Respuesta), shape=8) +
  geom_point(data = df2, aes(y = ifelse(R == '0', 0, 1)), 
             alpha = 0.05, shape = 16, position = position_jitter(w=0.05, h=0)) +
  geom_ribbon(aes(ymin=li, ymax=ls, 
                  fill = after_scale(alpha(colour, 0.1)) ))+
  facet_grid(Reaccion ~ Farmaco, scales = 'free_x') +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Probabilidad') +
  scale_color_manual(values = c('red', 'blue')) +
  theme(legend.position = 'none')


g2 <- ggplotly(
  g1, 
  # width = 672, height = 573,
  tooltip = c('Dosis', 'Respuesta', 'pred')
)

htmlwidgets::saveWidget(as_widget(g2), 
                        file.path('figures', "01_dosis_resp.html") %>% 
                          normalizePath())

#-------------------------------------------------------------------------------#
# Términos Binominal -----------------------------------------------------
#-------------------------------------------------------------------------------#

g3 <- dfT %>% 
  select(Farmaco, Reaccion, tidy) %>% 
  unnest(cols = c(tidy)) %>% 
  ggplot(aes(x=estimate, y=term, colour = Farmaco)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high)) +
  facet_grid(Reaccion ~ Farmaco, scales='free_x') + 
  geom_vline(xintercept = 0, lty='dashed') +
  labs(x = 'Estimado', y = 'Término') +
  scale_color_manual(values = c('red', 'blue')) +
  theme(legend.position = 'none')

g3b <- ggplotly(
  g3, tooltip = c('Dosis', 'Respuesta', 'pred')
)

htmlwidgets::saveWidget(as_widget(g3b), 
                        file.path('figures', "02_regBinominal.html") %>% 
                          normalizePath())

#-------------------------------------------------------------------------------#
# Regresión binomial negativa -----------------
#-------------------------------------------------------------------------------#

dfT1 <- df1 %>% 
  group_by(Farmaco, Reaccion) %>% 
  nest() %>% 
  mutate(
    fit1 = map(data, ~betareg(formula = Respuesta ~ Dosis, data = .))
  )

dfT1 <- dfT1 %>% 
  mutate(
    tidy   = map(fit1, ~tidy(.x, conf.int=TRUE)),
    glance = map(fit1, ~glance(.x)),
    predict= map2(fit1, data, ~createBetaPredictions(.x, .y)),
    obs    = map(predict, 'obs'),
    grid   = map(predict, 'grid')
  )


g4 <- 
  dfT1 %>% 
  select(Farmaco, Reaccion, grid) %>% 
  unnest(cols = c(grid)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Dosis, 
             colour = Farmaco, 
  )) +
  geom_line(aes(y=pred)) + 
  geom_point(data=df1, aes(y=Respuesta), shape=8) +
  geom_ribbon(aes(ymin=li, ymax=ls, 
                  fill = after_scale(alpha(colour, 0.1)) ))+
  facet_grid(Reaccion ~ Farmaco, scales = 'free_x') +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Proporcion') +
  scale_color_manual(values = c('red', 'blue')) +
  theme(legend.position = 'none')

g4b <- ggplotly(
  g4, tooltip = c('Dosis', 'Respuesta', 'pred')
)

htmlwidgets::saveWidget(as_widget(g4b), 
                        file.path('figures', "03_regBeta.html") %>% 
                          normalizePath())

# Términos
dfT1 %>% 
  select(Farmaco, Reaccion, glance) %>% 
  unnest(cols = c(glance))

#-------------------------------------------------------------------------------#
# Regresión logística ordinal -----------------------------------------------------
#-------------------------------------------------------------------------------#
drowsiness_DF <- read_excel("Opioides_RAM.xlsx", sheet = "S20") %>% 
  pivot_longer(cols = c('Marcada', 'Moderada', 'Leve'), 
               names_to='Somnolencia', values_to='Valor') %>% 
  group_by(Farmaco, Dosis) %>% 
  mutate(Prop = Valor/sum(Valor)) %>% 
  ungroup()

drowsiness_DF_1 <- Tabla_Expand_3(drowsiness_DF)

dfT2 <- drowsiness_DF_1 %>% 
  group_by(Farmaco) %>% 
  nest() %>% 
  mutate(
    fit1 = map(data, ~polr(R ~ Dosis, data = ., , Hess = TRUE))
  )

dfT2 <- dfT2 %>% 
  mutate(
    tidy   = map(fit1, ~tidy(.x, conf.int=TRUE)),
    glance = map(fit1, ~glance(.x)),
    predict= map2(fit1, data, ~createPolrPredictions(.x, .y)),
    obs    = map(predict, 'obs'),
    grid   = map(predict, 'grid')
  )


g5 <- dfT2 %>% 
  select(Farmaco, grid) %>% 
  unnest(cols = c(grid)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c('Marcada', 'Moderada', 'Leve'), 
               names_to='Somnolencia', values_to='Valor') %>%
  ggplot(aes(x=Dosis, colour = Somnolencia, shape=Somnolencia)) +
  geom_line(aes(y=Valor)) + 
  geom_point(data=drowsiness_DF_1, aes(y=Prop)) +
  facet_wrap(Farmaco ~ ., scales = 'free_x') +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  labs(x = 'Dosis (mg/kg)', y = 'Proporcion') +
  scale_color_manual(values = c('red', 'blue', 'green')) +
  theme(legend.position = 'left')


g5b <- ggplotly(
  g5, tooltip = c('Dosis', 'Valor', 'Prop')
)

htmlwidgets::saveWidget(as_widget(g5b), 
                        file.path('figures', "05_regLogisOrdinal.html") %>% 
                          normalizePath())



