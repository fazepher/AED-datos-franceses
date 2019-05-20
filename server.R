############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
#################################################### AED ###################################################
############################################################################################################
############################################# SERVER APLICACION ############################################
############################################################################################################

#### Preambulo ####

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(flexdashboard)
library(shinycssloaders)
library(rlang)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(magrittr)
library(geofacet)
library(DT)

#### Servidor (server) ####
server <- function(input,output){
  
  # Selecciones iniciales
  aaaa <- reactive({
    str_extract_all(input$elec,"[0-9]{4}") %>% unlist
  })
  color <- reactive({
    filter(paleta_tesis_fn, FAMILIA == input$familia) %>% 
      extract2("COLOR")
  })
  
  # Categoría seleccionada
  cats_disp <- reactive({
    filter(tabla_variables, Variable == input$var) %>% 
    {set_names(extract2(.,"Cats"),extract2(.,"Etiqueta"))}
  })
  
  output$cat <- renderUI({
    selectInput("cat","Categoría",req(cats_disp()))
  })
  
  etiqueta_cat <- reactive({
    filter(tabla_variables, Cats == req(input$cat)) %>% 
      extract2("Etiqueta")
  })
  
  # Datos seleccionados
  datos_electorales <- reactive({
    filter(datos_electorales_completos, 
           ELECCION == input$elec, 
           FAMILIA == input$familia) %>% 
      mutate(PCT_VOTOS_BR = (VOT_CANDIDATO + 0.5)/(INSCRITOS + 1))
  })
  
  output$tabla_votos_nal <- renderTable({
    datos_electorales() %>% 
      summarise(Mediana = median(PCT_VOTOS_BR),
                Media = mean(PCT_VOTOS_BR),
                Mínimo = min(PCT_VOTOS_BR),
                Máximo = max(PCT_VOTOS_BR),
                `Cuartil 1` = quantile(PCT_VOTOS_BR,0.25),
                `Cuartil 3` = quantile(PCT_VOTOS_BR,0.75),
                `Desviación Estándar` = sd(PCT_VOTOS_BR)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric,list(~ round(.,3))) %>% 
      gather(Estadístico, `% Votos`)
  })
  
  datos_graf_solo_insee <- reactive({
    datos_censales %>% 
      filter(AAAA == aaaa()) %>% 
      inner_join(COMUNAS_2007) %>% 
      filter(COD_REG %in% fr_anc_reg_metr$code) %>% 
      filter(Pob >= input$pob_min) %>% 
      left_join(otros_datos_comunales, by = c("CODGEO","AAAA")) %>% 
      select(CODGEO, COD_DPTO:NOM_REG, req(input$cat)) %>% 
      dplyr::rename(Pct = UQ(req(input$cat))) %>% 
      filter(!is.na(Pct)) 
  })
  
  output$tabla_cat_nal <- renderTable({
    datos_graf_solo_insee() %>% 
      summarise_at("Pct",
                   list(Mediana = ~ median(.),
                        Media = ~ mean(.),
                        Mínimo = ~ min(.),
                        Máximo = ~ max(.),
                        `Cuartil 1` = ~ quantile(.,0.25),
                        `Cuartil 3` = ~ quantile(.,0.75),
                        `Desviación Estándar` = ~sd(.))) %>% 
      ungroup() %>% 
      mutate_if(is.numeric,list(~ round(.,3))) %>% 
      gather(Estadístico, `% Pob`)
  })
  
  # Distribuciones del voto
  datos_votos_reg <- reactive({
    datos_electorales() %>% 
      inner_join(COMUNAS_2007) %>% 
      genera_distr_reg(var = PCT_VOTOS_BR)
  })
  
  output$distr_hist_votos <- renderPlot({
    geofacet_distr_hist(datos = datos_votos_reg(),
                        var = PCT_VOTOS_BR,
                        titulo = paste(input$familia,"en las",input$elec,sep=" "),
                        tit_x = "% votos comunal por region y para la metrópoli entera",
                        color = color())
  })
  
  output$tabla_votos_reg <- renderDataTable({
    datos_votos_reg() %>% 
      group_by(NOM_REG) %>% 
      summarise(Comunas = n(),
                Mediana = unique(MEDIANA),
                Media = mean(PCT_VOTOS_BR),
                Min = min(PCT_VOTOS_BR),
                Max = max(PCT_VOTOS_BR),
                Q1 = quantile(PCT_VOTOS_BR,0.25),
                Q3 = quantile(PCT_VOTOS_BR,0.75),
                SD = sd(PCT_VOTOS_BR),
                Cociente_Med = unique(MEDIANA)/unique(MEDIANA_NAL)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric,list(~ round(.,3)))
  },
  rownames = FALSE,
  colnames = c("Región","Núm. Comunas",
               "Mediana","Media","Mínimo","Máximo","1er Cuartil","3er Cuartil","Desv. Est.",
               "Cociente Mediana Nacional"),
  filter = "top",
  options = list(
    lengthMenu = c(1,5,10,15,20),
    pageLength = 20,
    dom = "ltp",
    language = list(
      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
    )
  )
  )
  
  datos_referencia_votos <- reactive({
    datos_electorales() %>% 
      inner_join(COMUNAS_2007) %>% 
      genera_datos_referencia(var = PCT_VOTOS_BR)
  })
  
  datos_votos_dpto <- reactive({
    datos_electorales() %>% 
      inner_join(COMUNAS_2007) %>% 
      genera_datos_dpto(med_nal = datos_referencia_votos() %>% extract2("Med"),
                        var = PCT_VOTOS_BR)
  })
  
  output$distr_viol_votos <- renderPlot({
    geofacet_violines_dpto(datos = datos_votos_dpto(),
                           datos_referencia = datos_referencia_votos(),
                           var = PCT_VOTOS_BR, 
                           titulo = paste(input$familia,"en las",input$elec,sep=" "),
                           texto_metropoli = "Metrópili entera",
                           color = color())
  })
  
  output$tabla_votos_dpto <- renderDataTable({
    datos_votos_dpto() %>% 
      group_by(NOM_REG,NOM_DPTO,COD_DPTO) %>% 
      summarise(Comunas = n(),
                Mediana = unique(Mediana_Dpto),
                Media = mean(PCT_VOTOS_BR),
                Min = min(PCT_VOTOS_BR),
                Max = max(PCT_VOTOS_BR),
                Q1 = quantile(PCT_VOTOS_BR,0.25),
                Q3 = quantile(PCT_VOTOS_BR,0.75),
                SD = sd(PCT_VOTOS_BR),
                Cociente_Med = unique(Mediana_Dpto)/unique(Mediana_Nacional)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric,list(~ round(.,3)))
  },
  rownames = FALSE,
  colnames = c("Región", "Departamento", "Código INSEE de Departamento","Núm. Comunas",
               "Mediana","Media","Mínimo","Máximo","1er Cuartil","3er Cuartil","Desv. Est.","Cociente Mediana Nacional"),
  filter = "top",
  options = list(
    lengthMenu = 2:8,
    pageLength = 5,
    dom = "ltp",
    language = list(
      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
    )
  )
  )
  
  # Distribuciones de la categoría
  datos_cat_reg <- reactive({
    datos_graf_solo_insee() %>% 
      genera_distr_reg(var = Pct)
  })
  
  output$tabla_cat_reg <- renderDataTable({
    datos_cat_reg() %>% 
      group_by(NOM_REG) %>% 
      summarise(Comunas = n(),
                Mediana = unique(MEDIANA),
                Media = mean(Pct),
                Min = min(Pct),
                Max = max(Pct),
                Q1 = quantile(Pct,0.25),
                Q3 = quantile(Pct,0.75),
                SD = sd(Pct),
                Cociente_Med = unique(MEDIANA)/unique(MEDIANA_NAL)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric,list(~ round(.,3)))
  },
  rownames = FALSE,
  colnames = c("Región","Núm. Comunas",
               "Mediana","Media","Mínimo","Máximo","1er Cuartil","3er Cuartil","Desv. Est.","Cociente Mediana Nacional"),
  filter = "top",
  options = list(
    lengthMenu = c(1,5,10,15,20),
    pageLength = 20,
    dom = "ltp",
    language = list(
      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
    )
  )
  )
  
  output$distr_hist_cat <- renderPlot({
    geofacet_distr_hist(datos = datos_cat_reg(),
                        var = Pct,
                        titulo = paste(etiqueta_cat(),"en",aaaa(),sep=" "),
                        tit_x = "% poblacion comunal por región y para la metrópoli entera",
                        color = color())
  })
  
  datos_referencia_cat <- reactive({
    datos_graf_solo_insee() %>% 
      genera_datos_referencia(var = Pct)
  })
  
  datos_cat_dpto <- reactive({
    datos_graf_solo_insee() %>% 
      genera_datos_dpto(med_nal = datos_referencia_cat() %>% extract2("Med"),
                        var = Pct)
  })
  
  output$distr_viol_cat <- renderPlot({
    geofacet_violines_dpto(datos = datos_cat_dpto(),
                           datos_referencia = datos_referencia_cat(),
                           var = Pct, 
                           titulo = paste(etiqueta_cat(),"en",aaaa(),sep=" "),
                           texto_metropoli = "Metrópili entera",
                           color = color())
  })
  
  output$tabla_cat_dpto <- renderDataTable({
    datos_cat_dpto() %>% 
      group_by(NOM_REG,NOM_DPTO,COD_DPTO) %>% 
      summarise(Comunas = n(),
                Mediana = unique(Mediana_Dpto),
                Media = mean(Pct),
                Min = min(Pct),
                Max = max(Pct),
                Q1 = quantile(Pct,0.25),
                Q3 = quantile(Pct,0.75),
                SD = sd(Pct),
                Cociente_Med = unique(Mediana_Dpto)/unique(Mediana_Nacional)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric,list(~ round(.,3)))
  },
  rownames = FALSE,
  colnames = c("Región", "Departamento", "Código INSEE de Departamento","Núm. Comunas",
               "Mediana","Media","Mínimo","Máximo","1er Cuartil","3er Cuartil","Desv. Est.","Cociente Mediana Nacional"),
  filter = "top",
  options = list(
    lengthMenu = 2:8,
    pageLength = 5,
    dom = "ltp",
    language = list(
      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
    )
  )
  )
  
  # Diagrama de dispersión
  datos_graf <- reactive({
    datos_graf_solo_insee() %>% 
      inner_join(datos_electorales()) %>% 
      group_by(COD_REG) %>% 
      mutate(Alpha = 1/n()) %>% 
      ungroup
  })
  
  output$graf_disper <- renderPlot({
    geofacet_disp_votos_cat_reg(datos_graf(),
                                paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                if_else(input$var %in% c("Escolaridad","Empleo"),
                                        "% población correspondiente",
                                        "% población comunal"),
                                color = color())
  })
  
  # Correlaciones lineales
  datos_corr <- reactive({
    datos_graf() %>% 
      genera_datos_corr
  })
  
  output$graf_corr <- renderPlot({
    geofacet_corr_votos_cat_dpto(datos_corr(),
                                 paste("Correlación", etiqueta_cat(),"y",input$familia,"en las",input$elec,sep=" "),
                                 "Código INSEE de Departamento",
                                 "Coeficiente de correlación",
                                 color = color())
  })
  
  output$tabla_corr <- renderDataTable({
    datos_corr() %>% 
      select(-COD_REG)
    },
    rownames = FALSE,
    colnames = c("Región","Departamento","Código INSEE de Departamento","Núm. Comunas",
                 paste("Mediana %",c(input$familia,etiqueta_cat())),
                 "Correlación","Valor p Corr. 0"),
    filter = "top",
    options = list(
      lengthMenu = 2:8,
      pageLength = 5,
      dom = "ltp",
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
      )
    )
  )
  
  # Tendencias ingenuas
  output$graf_smooth_loess <- renderPlot({
    geofacet_smooth_votos_cat_dpto(datos_graf(),
                                   "loess",
                                   paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                   if_else(input$var %in% c("Escolaridad","Empleo"),
                                           "% población correspondiente",
                                           "% población comunal"),
                                   color = color(),
                                   span = input$span)
  })
  
  output$graf_smooth_lm <- renderPlot({
    geofacet_smooth_votos_cat_dpto(datos_graf(),
                                   "lm",
                                   paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                   if_else(input$var %in% c("Escolaridad","Empleo"),
                                           "% población correspondiente",
                                           "% población comunal"),
                                   color = color())
  })
  
}