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
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(magrittr)
library(geofacet)

#### Servidor (server) ####
server <- function(input,output){
  
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
  
  datos_electorales <- reactive({
    filter(datos_electorales_completos, ELECCION == input$elec, FAMILIA == input$familia)
  })
  aaaa <- reactive({
    str_extract_all(input$elec,"[0-9]{4}") %>% unlist
  })
  color <- reactive({
    filter(paleta_tesis_fn, FAMILIA == input$familia) %>% 
      extract2("COLOR")
  })
  
  datos_graf_solo_insee <- reactive({
    datos_censales %>% 
      filter(AAAA == aaaa()) %>% 
      inner_join(COMUNAS_2007) %>% 
      filter(COD_REG %in% fr_anc_reg_metr$code) %>% 
      filter(Pob >= input$pob_min) %>% 
      left_join(otros_datos_comunales, by = c("CODGEO","AAAA")) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,req(input$cat)) %>% 
      dplyr::rename(Pct = UQ(req(input$cat))) %>% 
      filter(!is.na(Pct)) 
  })
  
  datos_graf <- reactive({
    datos_graf_solo_insee() %>% 
      inner_join(datos_electorales()) %>% 
      group_by(COD_REG) %>% 
      mutate(Alpha = 1/n()) %>% 
      ungroup
  })
  
  output$distr_hist_votos <- renderPlot({
    datos_electorales() %>%
      inner_join(COMUNAS_2007) %>%
      geofacet_distr_hist_votos(paste(input$familia,"en las",input$elec,sep=" "),
                                "% votos comunal por region y para la metrópoli entera",
                                color = color())
  })

  output$distr_viol_votos <- renderPlot({
    datos_electorales() %>%
      inner_join(COMUNAS_2007) %>%
      geofacet_violines_votos_dpto(paste(input$familia,"en las",input$elec,sep=" "),
                                   "Metrópili entera",
                                   color = color())
  })

  output$distr_hist_cat <- renderPlot({
    geofacet_distr_hist_cat(datos_graf(),
                            paste(etiqueta_cat(),"en",aaaa(),sep=" "),
                            "% poblacion comunal por región y para la metrópoli entera",
                            color = color())
  })

  output$distr_viol_cat <- renderPlot({
    geofacet_violines_cat_dpto(datos_graf(),
                               paste(etiqueta_cat(),"en",aaaa(),sep=" "),
                               "Metrópili entera",
                               color = color())
  })
  
  output$graf_disper <- renderPlot({
     geofacet_disp_votos_cat_reg(datos_graf(),
                                  paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                  if_else(input$var %in% c("Escolaridad","Empleo"),
                                          "% población correspondiente",
                                          "% población comunal"),
                                  color = color())
  })

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

  output$tabla_corr <- DT::renderDataTable({
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
  
  output$graf_smooth <- renderPlot({
    geofacet_smooth_votos_cat_dpto(datos_graf(),
                                     paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                     if_else(input$var %in% c("Escolaridad","Empleo"),
                                             "% población correspondiente",
                                             "% población comunal"),
                                     color = color())
  })
  
}