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

#-------------------------------------------------------------------------------#
#' Expandir datos reportados como proporciones a variable binaria
#' 
#' @param data datos originales
#' @param prop_var columna donde está la variable que muestra la proporción 
#' de casos positivos
#' @param conteo_var columna donde está la variable que muestra la población 
#' a la que se refiere esta proporción
#' @param respuesta_var Nombre de la columna donde se encontrará la variable 
#' binaria
#'
#' @return
#' @export
#'
#' @examples
#' Tabla_Expand_2(df1)
#' 
#' 
Tabla_Expand_2 <- function(data, 
                           prop_var = 'Respuesta', conteo_var = 'N', respuesta_var = 'R'){
  
  prop_var_quo      <- rlang::ensym(prop_var)
  conteo_var_quo    <- rlang::ensym(conteo_var)
  respuesta_var_quo <- rlang::ensym(respuesta_var)
  
  df <- data %>% 
    mutate(
      !!respuesta_var_quo := map2(
        !!prop_var_quo, 
        !!conteo_var_quo, 
        ~ c(rep(1, .x * .y), rep(0, (1 - .x) * .y))
        )
    ) %>% 
    unnest(cols = c(!!respuesta_var_quo)) %>% 
    mutate(!!respuesta_var_quo := factor(!!respuesta_var_quo))
  
  return(df)
}



#-------------------------------------------------------------------------------#

createLogPredictions <- function(model, data, lims, alpha=0.05, var='Dosis') {
  var_quo = ensym(var)
  
  if(!missing(lims)){
    dgrid  = tibble(
      !!var_quo := seq(lims[1], lims[2], length.out = 1000L))
  } else {
    dgrid  = tibble(
      !!var_quo := data %>% 
        `$`(!!var_quo) %>% 
        {seq(min(.), max(.), length.out = 1000L)})
  }
  
  
  ypred1 = predict(model, newdata = data,  type='link', se.fit=TRUE)
  yclas1 = predict(model, newdata = data,  type='response')
  ypred2 = predict(model, newdata = dgrid, type='link', se.fit=TRUE)
  
  createDF <- function(df, pred) {
    pred.me = plogis(pred$fit)
    pred.li = plogis(pred$fit - (qnorm(1 - alpha/2) * pred$se.fit))
    pred.ls = plogis(pred$fit + (qnorm(1 - alpha/2) * pred$se.fit))
    
    df %>% add_column(pred=pred.me, li=pred.li, ls=pred.ls)
  }
  
  return(list(obs = createDF(data, ypred1) %>% add_column(C = yclas1), grid = createDF(dgrid, ypred2)))
}

#-------------------------------------------------------------------------------#
createBetaPredictions <- function(model, data, lims, alpha=0.05, var='Dosis') {
  var_quo = ensym(var)
  
  if(!missing(lims)){
    dgrid  = tibble(
      !!var_quo := seq(lims[1], lims[2], length.out = 1000L))
  } else {
    dgrid  = tibble(
      !!var_quo := data %>% 
        `$`(!!var_quo) %>% 
        {seq(min(.), max(.), length.out = 1000L)})
  }
  
  createDF <- function(pred) {
    pred.me = predict(model, newdata = pred, type='response')
    pred.li = predict(model, newdata = pred, type='quantile', at=c(0.05/2))
    pred.ls = predict(model, newdata = pred, type='quantile', at=c(1 - 0.05/2))
    
    pred %>% add_column(pred=pred.me, li=pred.li, ls=pred.ls)
  }
  
  return(list(obs = createDF(data), grid = createDF(dgrid)))
}

#-------------------------------------------------------------------------------#
Tabla_Expand_3 <- function(data, 
                           prop_var = 'Somnolencia', conteo_var = 'Valor', respuesta_var = 'R'){
  
  prop_var_quo      <- rlang::ensym(prop_var)
  conteo_var_quo    <- rlang::ensym(conteo_var)
  respuesta_var_quo <- rlang::ensym(respuesta_var)
  
  df <- data %>% 
    mutate(
      !!respuesta_var_quo := map2(
        !!prop_var_quo, 
        !!conteo_var_quo,
        ~case_when(
          .x == 'Marcada' ~ rep(3, .y),
          .x == 'Moderada' ~ rep(2, .y),
          .x == 'Leve' ~ rep(1, .y),
          TRUE ~ rep(0, .y)
        )
      )) %>% 
    unnest(cols = c(!!respuesta_var_quo)) %>% 
    mutate(!!respuesta_var_quo := factor(!!respuesta_var_quo, levels = c(1,2,3), ordered = TRUE))
  
  return(df)
}

#-------------------------------------------------------------------------------#
createPolrPredictions <- function(model, data, lims, alpha=0.05, var='Dosis') {
  var_quo = ensym(var)
  
  if(!missing(lims)){
    dgrid  = tibble(
      !!var_quo := seq(lims[1], lims[2], length.out = 1000L))
  } else {
    dgrid  = tibble(
      !!var_quo := data %>% 
        `$`(!!var_quo) %>% 
        {seq(min(.), max(.), length.out = 1000L)})
  }
  
  createDF <- function(pred) {
    pred.me = predict(model, newdata = pred, type='probs') %>% as_tibble()
    
    pred %>% add_column(
      Leve=pred.me$`1`, 
      Moderada=pred.me$`2`, 
      Marcada=pred.me$`3`
    )
  }
  
  return(list(obs = createDF(data), grid = createDF(dgrid)))
}
