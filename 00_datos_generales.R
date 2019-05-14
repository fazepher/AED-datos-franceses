
#### Datos franceses ####

COMUNAS_2007 <- read_csv("datos/comunas_07.csv", 
                         locale = locale(encoding = "latin1"))

COMUNAS_2012 <- read_csv("datos/comunas_12.csv", 
                         locale = locale(encoding = "latin1")) 

fr_anc_reg_metr <- read_csv(file = "datos/fr_anc_reg_metr.csv",
                       locale = locale(encoding = "latin1")) 

paleta_tesis_fn <- read_csv("datos/PALETA_TESIS_FN.csv",col_types = "cc") 

datos_electorales_completos <- read_csv("datos/RESULTADOS_ELECTORALES.csv", 
                                        locale = locale(encoding = "latin1")) 

datos_censales <- read_csv("datos/DATOS_CENSALES.csv", 
                           locale = locale(encoding = "latin1")) 

datos_censales <- read_csv("datos/DATOS_CENSALES.csv", 
                           locale = locale(encoding = "latin1")) 

otros_datos_comunales <- read_csv("datos/OTROS_DATOS_COMUNALES.csv", 
                                  locale = locale(encoding = "latin1")) 

tabla_variables <- read_csv("datos/Variables.csv", 
                            locale = locale(encoding = "latin1")) 

#### Funciones generales de gráficas ####

logit <- function(x){
  log(x/(1-x))
}

geofacet_distr_hist_votos <- function(datos_graf,titulo, tit_x, color){
  
  datos_distr_por_anc_reg <- datos_graf %>% 
    group_by(COD_REG) %>% 
    mutate(MEDIANA = median(PCT_VOTOS_BR)) %>% 
    ungroup() %>% 
    mutate(MEDIANA_NAL = median(PCT_VOTOS_BR)) %>% 
    mutate(NOM_REG = reorder(NOM_REG,MEDIANA),code=COD_REG)
  
  graf <- ggplot(datos_distr_por_anc_reg,
         aes(x = PCT_VOTOS_BR, fill = FAMILIA, 
             alpha = MEDIANA/MEDIANA_NAL, stat(density))) + 
    geom_histogram(binwidth = 0.01) +
    geom_density(data = select(datos_distr_por_anc_reg,-COD_REG),fill="transparent",color="black") +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name") + 
    scale_fill_manual(values = color) + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_x_continuous(trans = "log1p", breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
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

geofacet_violines_votos_dpto <- function(datos_graf, titulo, texto_metropoli, color){
  
  lim_y <- datos_graf %>% 
    top_n(1,PCT_VOTOS_BR) %>% 
    extract2("PCT_VOTOS_BR") %>% 
    unique
  
  datos_referencia <- summarise(datos_graf,
                                Med = median(PCT_VOTOS_BR),
                                Q25 = quantile(PCT_VOTOS_BR,0.25),
                                Q75 = quantile(PCT_VOTOS_BR,0.75))
  
  pct_comuna_sub <- ggplot(datos_graf, aes(x=texto_metropoli,y=PCT_VOTOS_BR,color=FAMILIA)) +
    geom_hline(yintercept = datos_referencia[["Med"]], color="gray20") +
    geom_hline(yintercept = datos_referencia[["Q25"]], color = "gray50") +
    geom_hline(yintercept = datos_referencia[["Q75"]], color = "gray50") +
    geom_violin(fill = "transparent", color = color) +
    scale_y_continuous(limits = c(0,lim_y), trans = "log1p",
                       breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 10),
          axis.text.y = element_text(size = 15))
  
  datos_dptos <- datos_graf %>% 
    group_by(COD_DPTO) %>% 
    mutate(Mediana_Dpto = median(PCT_VOTOS_BR)) %>% 
    ungroup %>% 
    mutate(COD_DPTO = reorder(COD_DPTO,Mediana_Dpto),
           Mediana_Nacional = datos_referencia[["Med"]])
  
  pct_comuna_dpto <- ggplot(datos_dptos, aes(x = COD_DPTO, y = PCT_VOTOS_BR)) + 
    geom_hline(yintercept = datos_referencia[["Med"]], color="gray20") + 
    geom_hline(yintercept = datos_referencia[["Q25"]], color = "gray50") + 
    geom_hline(yintercept = datos_referencia[["Q75"]], color = "gray50") + 
    geom_violin(aes(alpha = Mediana_Dpto/Mediana_Nacional), fill = color) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free_x") + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_y_continuous(limits = c(0,lim_y), trans = "log1p",
                       breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
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

geofacet_distr_hist_cat <- function(datos_graf, titulo, tit_x, color){
  
  datos_distr_por_anc_reg <- datos_graf %>% 
    group_by(COD_REG) %>% 
    mutate(MEDIANA = median(Pct)) %>% 
    ungroup() %>% 
    mutate(MEDIANA_NAL = median(Pct)) %>% 
    mutate(NOM_REG = reorder(NOM_REG,MEDIANA),code=COD_REG)
  
  graf <- ggplot(datos_distr_por_anc_reg,
                 aes(x = Pct, fill = FAMILIA, 
                     alpha = MEDIANA/MEDIANA_NAL, stat(density))) + 
    geom_histogram(binwidth = 0.01) +
    geom_density(data = select(datos_distr_por_anc_reg,-COD_REG),fill="transparent",color="black") +
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name") + 
    scale_fill_manual(values = color) + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_x_continuous(trans = "log1p", breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
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

geofacet_violines_cat_dpto <- function(datos_graf, titulo, texto_metropoli, color){
  
  lim_y <- datos_graf %>% 
    top_n(1,Pct) %>% 
    extract2("Pct") %>% 
    unique
  
  datos_referencia <- summarise(datos_graf,
                                Med = median(Pct),
                                Q25 = quantile(Pct,0.25),
                                Q75 = quantile(Pct,0.75))
  
  pct_comuna_sub <- ggplot(datos_graf, aes(x=texto_metropoli,y=Pct,color=FAMILIA)) +
    geom_hline(yintercept = datos_referencia[["Med"]], color="gray20") +
    geom_hline(yintercept = datos_referencia[["Q25"]], color = "gray50") +
    geom_hline(yintercept = datos_referencia[["Q75"]], color = "gray50") +
    geom_violin(fill = "transparent", color = color) +
    scale_y_continuous(limits = c(0,lim_y), trans = "log1p",
                       breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(3,0,1,0), size = 10),
          axis.text.y = element_text(size = 15))
  
  datos_dptos <- datos_graf %>% 
    group_by(COD_DPTO) %>% 
    mutate(Mediana_Dpto = median(Pct)) %>% 
    ungroup %>% 
    mutate(COD_DPTO = reorder(COD_DPTO,Mediana_Dpto),
           Mediana_Nacional = datos_referencia[["Med"]])
  
  pct_comuna_dpto <- ggplot(datos_dptos, aes(x = COD_DPTO, y = Pct)) + 
    geom_hline(yintercept = datos_referencia[["Med"]], color="gray20") + 
    geom_hline(yintercept = datos_referencia[["Q25"]], color = "gray50") + 
    geom_hline(yintercept = datos_referencia[["Q75"]], color = "gray50") + 
    geom_violin(aes(alpha = Mediana_Dpto/Mediana_Nacional), fill = color) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free_x") + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_y_continuous(limits = c(0,lim_y), trans = "log1p",
                       breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
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

geofacet_disp_votos_cat_reg <- function(datos_graf,titulo,tit_x,color){
  
  graf <- ggplot(datos_graf, aes(x=Pct,y=PCT_VOTOS_BR,alpha = Alpha)) + 
    geom_point(color=color, size = rel(0.2)) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free") + 
    scale_x_continuous(breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
    scale_y_continuous(trans = "logit", breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
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

geofacet_smooth_votos_cat_dpto <- function(datos_graf,titulo,tit_x,color){
  
  graf <- ggplot(datos_graf, aes(x=Pct,y=PCT_VOTOS_BR)) + 
    geom_smooth(method = "lm", color= color, size = rel(1)) + 
    geom_smooth(aes(group = COD_DPTO), method = "lm", se = FALSE, color=color, size = rel(0.65), alpha = 0.8) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name") + 
    scale_y_continuous(trans = "logit", breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) + 
    scale_x_continuous(breaks = waiver(), labels = function(x) percent(x,accuracy = 1)) +
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

geofacet_corr_votos_cat_dpto <- function(datos_graf,titulo,tit_x,tit_y,color){
  
  graf <- datos_graf %>% 
    filter(PCT_VOTOS_BR > 0) %>% 
    group_by(COD_REG,COD_DPTO) %>% 
    summarise(Corr = cor(Pct,logit(PCT_VOTOS_BR)), 
              p = cor.test(Pct,logit(PCT_VOTOS_BR)) %>% extract2("p.value") %>% multiply_by(-1) %>% add(1)) %>% 
    ungroup %>% 
    mutate(COD_DPTO = reorder(COD_DPTO,Corr)) %>% 
    ggplot(aes(x = COD_DPTO, y = Corr, alpha = p)) + 
    geom_hline(yintercept = 0, color = color, size = rel(0.5)) + 
    geom_col(fill = color, width = 0.5) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free_x") + 
    scale_y_continuous(limits = c(-1,1)) + 
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

