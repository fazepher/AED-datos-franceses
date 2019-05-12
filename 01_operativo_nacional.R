####################################################################################################################
####################################################################################################################
########################################### JORNADA ELECTORAL MÉXICO 2018 ##########################################
##################################### APLICACIÓN DE VISUALIZACIÓN DE RESULTADOS ####################################
####################################################################################################################
##################################################### NUMÉRIKA #####################################################
########################################## FERNANDO ANTONIO ZEPEDA HERRERA #########################################
####################################################################################################################

####################################################################################################################
########################################## GRÁFICOS DE OPERATIVO NACIONAL ##########################################
####################################################################################################################


# Presidente 
graf_nacional_presidente <- function(input, output, session){
  
  Datos_EP %>% 
    filter(Estimacion == "EFECTIVA",
           CARGO == "Presidente",
           OPERATIVO == "Nacional") %>% 
    distinct(Estimacion,CARGO,FECHA,OPERATIVO,
             Candidatura,Prop,LInf95,LSup95,
             Cuestionarios,Secciones,Probabilidad) %>% 
    genera_graf_tendencia_tabla %>% 
    plot
  
}

# Senadores 
graf_nacional_senadores <- function(input, output, session){
  
  Datos_EP %>% 
    filter(Estimacion == "EFECTIVA",
           CARGO == "Senadores",
           OPERATIVO == "Nacional") %>% 
    distinct(Estimacion,CARGO,FECHA,OPERATIVO,
             Candidatura,Prop,LInf95,LSup95,
             Cuestionarios,Secciones,Probabilidad) %>% 
    genera_graf_tendencia_tabla %>% 
    plot
  
  
}

# Diputados 
graf_nacional_diputados <- function(input, output, session){
  
  Datos_EP %>% 
    filter(Estimacion == "EFECTIVA",
           CARGO == "Diputados",
           OPERATIVO == "Nacional") %>% 
    distinct(Estimacion,CARGO,FECHA,OPERATIVO,
             Candidatura,Prop,LInf95,LSup95,
             Cuestionarios,Secciones,Probabilidad) %>% 
    genera_graf_tendencia_tabla %>% 
    plot
  
}