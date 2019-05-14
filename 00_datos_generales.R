
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

geofacet_disp_votos_cat_reg <- function(datos_graf,titulo,tit_x,color){
  
  graf <- ggplot(datos_graf, aes(x=Pct,y=PCT_VOTOS_BR,alpha = Alpha)) + 
    geom_point(color=color, size = rel(0.2)) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free") + 
    scale_y_continuous(trans = "logit") + 
    scale_alpha_continuous(range = c(0.4,1)) + 
    theme_minimal() + 
    labs(title = titulo, 
         caption = "Geofacet de @fazepher con datos oficiales franceses",
         x = tit_x,
         y = "% votos brutos en escala logit") + 
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
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free") + 
    scale_y_continuous(trans = "logit") + 
    #scale_alpha_continuous(range = c(0.4,1)) + 
    theme_minimal() + 
    labs(title = titulo, 
         caption = "Geofacet de @fazepher con datos oficiales franceses",
         x = tit_x,
         y = "% votos brutos en escala logit") + 
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
    group_by(COD_REG,COD_DPTO) %>% 
    summarise(Corr = cor(Pct,PCT_VOTOS_BR), 
              p = cor.test(Pct,PCT_VOTOS_BR) %>% extract2("p.value") %>% multiply_by(-1) %>% add(1)) %>% 
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

