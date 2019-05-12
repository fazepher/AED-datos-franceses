#############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
#################################################### AED ###################################################
############################################################################################################
################################################# APLICACIÓN ###############################################
############################################################################################################


#### Preámbulo ####

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(magrittr)
library(shinyWidgets)
library(flexdashboard)
library(shinycssloaders)
library(geofacet)

#### Módulos adicionales ####

source("00_datos_generales.R")

#### Interfaz (ui) ####

ui <- fluidPage(
  
  theme = shinythemes::shinytheme("simplex"),
  
  # Encabezado 
  encabezado(id = "encabezado"),
  
  # Pestañas generales
  navbarPage(
    title = "Analizar por:",
    # Paneles por elección
    navbarMenu(
      title = "Elección",
      tabPanel(
        title = "Diagramas de dispersión",
        fluidRow(
          column(2,
                 selectInput("elec", "Elección", c("Presidenciales 2007","Legislativas 2007",
                                                   "Presidenciales 2012","Legislativas 2012")),
                 selectInput("familia", "Familia política", paleta_tesis_fn$FAMILIA),
                 selectInput("var", "Variable", tabla_variables$Variable),
                 uiOutput("cat"),
                 align = "left"),
          column(10,
                 plotOutput("graf_disper",
                            height = "800px") %>% withSpinner(color = "#6C7B8B"),
                 align = "center")
        )
      )
    )
  )
)

#### Servidor (server) ####
server <- function(input,output){
  
  # Categoría seleccionada
  cats_disp <- reactive({
    
    filter(tabla_variables, Variable == req(input$var)) %>% 
    {set_names(extract2(.,"Cats"),extract2(.,"Etiqueta"))}
    
  })
  
  output$cat <- renderUI({
    selectInput("cat","Categoría",cats_disp())
  })
  
  etiqueta_cat <- reactive({
    filter(tabla_variables, Cats == req(input$cat)) %>% 
      extract2("Etiqueta")
  })
  
  # Gráficos para operativo Nacional
  datos_electorales <- reactive({
    filter(datos_electorales_completos, ELECCION == input$elec, FAMILIA == input$familia)
  })
  aaaa <- reactive({str_extract_all(input$elec,"[0-9]{4}") %>% unlist
  })
  
  datos_graf_solo_censales <- reactive({
    datos_censales %>% 
      filter(AÑO == aaaa()) %>% 
      inner_join(COMUNAS_2007) %>% 
      filter(COD_REG %in% fr_anc_reg_metr$code) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,req(input$cat)) %>% 
      rename(Pct = UQ(req(input$cat))) %>% 
      filter(!is.na(Pct)) 
  })
  
  datos_graf <- reactive({
    datos_graf_solo_censales() %>% 
      inner_join(datos_electorales()) %>% 
      group_by(COD_REG) %>% 
      mutate(Alpha = 1/n())
  })
  
  output$graf_disper <- renderPlot({
    filter(paleta_tesis_fn, FAMILIA == input$familia) %>% 
      extract2("COLOR") %>% 
      geofacet_disp_votos_cat_reg(datos_graf(), 
                                  paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                  color = .)
  })
  
}

#### Shiny ####
shinyApp(ui,server)