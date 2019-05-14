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
library(shinythemes)

#### Módulos adicionales ####

source("00_datos_generales.R")

#### Interfaz (ui) ####

ui <- fluidPage(
  
  theme = shinytheme("simplex"),
  
  # Encabezado 
  fluidRow(
    column(3, img(src="banda_azul.png", height="100%", width="100%")),
    column(6, h1("Análisis exploratorio de datos franceses", 
                 style = "font-family: Times New Roman"), 
           align = "center"),
    column(3, img(src="banda_roja.png", height="100%", width="100%"))
  ),
  
  # Opciones de gráficos
  fluidRow(
    column(2,
           selectInput("elec", "Elección", c("Presidenciales 2007","Legislativas 2007",
                                             "Presidenciales 2012","Legislativas 2012")),
           selectInput("familia", "Familia política", paleta_tesis_fn$FAMILIA),
           selectInput("var", "Variable", tabla_variables$Variable),
           uiOutput("cat"),
           sliderInput("pob_min","Población mínima",0,1000,0,50)
    ),
    column(10,
           # Pestañas generales
           navbarPage(
             title = "Mostrar",
             # Paneles por elección
             tabPanel(
               title = "Diagramas de dispersión",
               plotOutput("graf_disper",
                          height = "800px") %>% withSpinner(color = "#6C7B8B")
             ),
             tabPanel(
               title = "Tendencias ingenuas",
               plotOutput("graf_smooth",
                          height = "800px") %>% withSpinner(color = "#6C7B8B")
             )
           )
    )
  )
)
