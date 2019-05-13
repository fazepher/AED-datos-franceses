
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

tabla_variables <- read_csv("datos/Variables.csv", 
                            locale = locale(encoding = "latin1")) 

#### Funciones generales de gráficas ####

geofacet_disp_votos_cat_reg <- function(datos_graf,titulo,color){
  
  graf <- ggplot(datos_graf, aes(x=Pct,y=PCT_VOTOS_BR,alpha = Alpha)) + 
    geom_point(color=color, size = rel(0.2)) + 
    facet_geo(~COD_REG, grid = fr_anc_reg_metr, label = "name", scales = "free") + 
    scale_y_continuous(trans = "logit") + 
    scale_alpha_continuous(range = c(0.4,1)) + 
    theme_minimal() + 
    labs(title = titulo, 
         caption = "Geofacet de @fazepher con datos oficiales franceses",
         x = "% población comunal",
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

