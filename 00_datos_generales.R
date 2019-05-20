#############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
#################################################### AED ###################################################
############################################################################################################
########################################## ADICIONALES APLICACION ##########################################
############################################################################################################

#### Datos franceses ####

COMUNAS_2007 <- read_csv("datos/comunas_07.csv", 
                         locale = locale(encoding = "latin1"))

fr_anc_reg_metr <- read_csv(file = "datos/fr_anc_reg_metr.csv",
                       locale = locale(encoding = "latin1")) 

paleta_tesis_fn <- read_csv("datos/PALETA_TESIS_FN.csv",col_types = "cc") 

datos_electorales_completos <- read_csv("datos/RESULTADOS_ELECTORALES.csv", 
                                        locale = locale(encoding = "latin1")) 

datos_censales <- read_csv("datos/DATOS_CENSALES.csv", 
                           locale = locale(encoding = "latin1"),
                           col_types = paste(c("c","i",rep("d",21)),collapse="")) 

otros_datos_comunales <- read_csv("datos/OTROS_DATOS_COMUNALES.csv", 
                                  locale = locale(encoding = "latin1"),
                                  col_types = paste(c("c","i",rep("d",11)),collapse="")) 

tabla_variables <- read_csv("datos/Variables.csv", 
                            locale = locale(encoding = "latin1")) 

#### Funciones para asociaciones ####

geofacet_disp_votos_cat_reg <- function(datos,titulo,tit_x,color){
  
  graf <- ggplot(datos, aes(x=Pct,y=PCT_VOTOS_BR,alpha = Alpha)) +
    geom_point(color=color, size = rel(0.2)) +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free") +
    scale_y_continuous(trans = "logit") +
    scale_alpha_continuous(range = c(0.4,1)) +
    labs(title = titulo,
         caption = "Geofacet de @fazepher con datos oficiales franceses",
         x = tit_x,
         y = "% votos brutos en escala logit") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(margin = margin(5,0,8,0), size = 15, hjust = 0.5),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 8),
          axis.text.y = element_text(margin = margin(0,1,0,3), size = 8),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, margin = margin(2,0,1,0)))
  return(graf)
  
}

geofacet_smooth_votos_cat_dpto <- function(datos,met,titulo,tit_x,color,...){
  
  graf <- ggplot(datos, aes(x=Pct,y=PCT_VOTOS_BR)) +
    geom_smooth(method = met, color= color, size = rel(1),...) +
    geom_smooth(aes(group = COD_DPTO), method = met, se = FALSE, color=color, size = rel(0.65), alpha = 0.8,...) +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name") +
    scale_y_continuous(trans = "logit") +
    labs(title = titulo,
         caption = "Geofacet de @fazepher con datos oficiales franceses",
         x = tit_x,
         y = "% votos brutos en escala logit") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(margin = margin(5,0,8,0), size = 15, hjust = 0.5),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 8),
          axis.text.y = element_text(margin = margin(0,1,0,3), size = 8),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, margin = margin(2,0,1,0)))
  return(graf)
  
}

logit <- function(x){
  log(x/(1-x))
}

genera_datos_corr <- function(datos){
  datos %>%
    filter(PCT_VOTOS_BR > 0) %>%
    group_by(NOM_REG,COD_REG,NOM_DPTO,COD_DPTO) %>%
    summarise(Comunas = n(),
              Med_Voto = median(PCT_VOTOS_BR),
              Med_Pct = median(Pct),
              Corr = cor(Pct,logit(PCT_VOTOS_BR)),
              p = cor.test(Pct,logit(PCT_VOTOS_BR)) %>% extract2("p.value")) %>%
    mutate_if(is.numeric,list(~round(.,3))) %>% 
    ungroup %>%
    mutate(COD_DPTO = reorder(COD_DPTO,Corr))
}

geofacet_corr_votos_cat_dpto <- function(datos,titulo,tit_x,tit_y,color){
  
  graf <- datos %>%
    ggplot(aes(x = COD_DPTO, y = Corr, alpha = p)) +
    geom_hline(yintercept = 0, color = color, size = rel(0.5)) +
    geom_col(fill = color, width = 0.5) +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free_x") +
    scale_y_continuous(limits = c(-1,1)) +
    scale_alpha_continuous(trans = "reverse") + 
    theme_minimal() +
    labs(title = titulo,
         caption = "Geofacet de @fazepher con datos oficiales franceses",
         x = tit_x,
         y = tit_y) +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(margin = margin(5,0,8,0), size = 15, hjust = 0.5),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 8),
          axis.text.y = element_text(margin = margin(0,1,0,3), size = 8),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, margin = margin(2,0,1,0)))
  return(graf)
  
}

#### Funciones para distribuciones ####

genera_distr_reg <- function(datos, var){
  datos %>%
    group_by(COD_REG) %>%
    mutate(MEDIANA = median(!!enquo(var))) %>%
    ungroup() %>%
    mutate(MEDIANA_NAL = median(!!enquo(var)),
           code=COD_REG)
}

geofacet_distr_hist <- function(datos, var, titulo, tit_x, color){

  graf <- datos %>% 
    ggplot(aes(x = !!enquo(var),alpha = MEDIANA/MEDIANA_NAL, stat(density))) +
    geom_histogram(binwidth = 0.01, fill = color) +
    geom_density(data = select(datos,-COD_REG), fill="transparent", color="black") +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name") +
    scale_alpha_continuous(range = c(0.3,1)) +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    labs(title = titulo,
         x = tit_x) +
    theme_minimal() +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 10, margin = margin(2,0,1,0)),
          plot.title = element_text(margin = margin(5,0,8,0), size = 15, hjust = 0.5),
          panel.grid = element_blank(),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 8),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

  return(graf)

}

genera_datos_referencia <- function(datos, var){
  summarise(datos,
            Med = median(!!enquo(var)),
            Q25 = quantile(!!enquo(var),0.25),
            Q75 = quantile(!!enquo(var),0.75))
}

genera_datos_dpto <- function(datos,med_nal,var){
  datos %>%
    group_by(COD_DPTO) %>%
    mutate(Mediana_Dpto = median(!!enquo(var))) %>%
    ungroup %>%
    mutate(COD_DPTO = reorder(COD_DPTO,Mediana_Dpto),
           Mediana_Nacional = med_nal)
}

geofacet_violines_dpto <- function(datos, datos_referencia, var, titulo, texto_metropoli, color){

  lim_y <- datos %>%
    top_n(1,!!enquo(var)) %>%
    extract2(ensym(var)) %>%
    unique

  pct_comuna_sub <- ggplot(datos, aes(x=texto_metropoli,y=!!enquo(var))) +
    geom_hline(yintercept = datos_referencia[["Med"]], color="gray20") +
    geom_hline(yintercept = datos_referencia[["Q25"]], color = "gray50") +
    geom_hline(yintercept = datos_referencia[["Q75"]], color = "gray50") +
    geom_violin(fill = "transparent", color = color) +
    scale_y_continuous(limits = c(0,lim_y), trans = "log1p") +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 10),
          axis.text.y = element_text(size = 15))

  pct_comuna_dpto <- ggplot(datos, aes(x = COD_DPTO, y = !!enquo(var))) +
    geom_hline(yintercept = datos_referencia[["Med"]], color="gray20") +
    geom_hline(yintercept = datos_referencia[["Q25"]], color = "gray50") +
    geom_hline(yintercept = datos_referencia[["Q75"]], color = "gray50") +
    geom_violin(aes(alpha = Mediana_Dpto/Mediana_Nacional), fill = color) +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free_x") +
    scale_alpha_continuous(range = c(0.3,1)) +
    scale_y_continuous(limits = c(0,lim_y), trans = "log1p") +
    labs(title = titulo,
         subtitle = "% de votos por comuna por departamento") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 8),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text.x = element_text(size = 10, margin = margin(2,0,1,0)),
          plot.title = element_text(margin = margin(5,0,8,0), size = 15, hjust = 0.5),
          plot.subtitle = element_text(margin = margin(5,0,5,0), size = 10, hjust = 0.5))

  graf <- ggplot(tibble(x=0:1,y=0:1),aes(x,y)) +
    theme_void() +
    annotation_custom(grob = ggplotGrob(pct_comuna_dpto),
                      xmin = 0,xmax = 1,ymin = 0,ymax = 1) +
    annotation_custom(grob = ggplotGrob(pct_comuna_sub),
                      xmin = 0.82,xmax = 0.92,ymin = 0.8,ymax = 0.95)
  return(graf)

}

