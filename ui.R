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