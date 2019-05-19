#############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
#################################################### AED ###################################################
############################################################################################################
############################################ INTERFAZ APLICACION ###########################################
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

#### Modulos adicionales ####

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
  
  # Opciones de graficos
  fluidRow(
    column(2,
           h4("Seleccionar filtros"),
           hr(),
           h4("% de votos (bruto)"),
           selectInput("elec", "Elección", 
                       c("Presidenciales 2007","Legislativas 2007", "Presidenciales 2012","Legislativas 2012"),
                       "Presidenciales 2012"),
           selectInput("familia", "Familia política", paleta_tesis_fn$FAMILIA),
           h5("Estadísticos descriptivos nacionales", align = "center"),
           tableOutput("tabla_votos_nal"),
           hr(),
           h4("% de población"),
           selectInput("var", "Variable", tabla_variables$Variable),
           uiOutput("cat"),
           sliderInput("pob_min","Población mínima",0,1000,0,50),
           h5("Estadísticos descriptivos nacionales", align = "center"),
           tableOutput("tabla_cat_nal")
    ),
    column(10,
           # Pestanas generales
           navbarPage(
             title = "Mostrar",
             # Paneles por eleccion
             navbarMenu(
               title = "Asociaciones",
               tabPanel(
                 title = "Correlaciones lineales por departamento",
                 h3("Correlaciones voto-categoría poblacional por departamento", align = "center"),
                 tabsetPanel(
                   tabPanel(
                     title = "Gráfico",
                     plotOutput("graf_corr",
                                height = "1000px") %>% withSpinner(color = "#6C7B8B")
                   ),
                   tabPanel(
                     title = "Tabla de datos",
                     dataTableOutput("tabla_corr") %>% withSpinner(color = "#6C7B8B")
                   )
                 )
               ),
               tabPanel(
                 title = "Diagramas de dispersión por región",
                 plotOutput("graf_disper",
                            height = "1000px") %>% withSpinner(color = "#6C7B8B")
               ),
               tabPanel(
                 title = "Tendencias ingenuas",
                 plotOutput("graf_smooth",
                            height = "1000px") %>% withSpinner(color = "#6C7B8B")
               )
             ),
             navbarMenu(
               title = "Distribuciones",
               tabPanel("Histogramas voto por región",
                        h3("% de votos por región", align = "center"),
                        tabsetPanel(
                          tabPanel(
                            title = "Gráfico",
                            plotOutput("distr_hist_votos", 
                                       height = "1000px") %>% withSpinner(color = "#6C7B8B")
                          ),
                          tabPanel(
                            title = "Tabla de datos",
                            dataTableOutput("tabla_votos_reg") %>% withSpinner(color = "#6C7B8B")
                          )
                        )
               ),
               tabPanel("Violines voto por departamento",
                        h3("% de votos por departamento", align = "center"),
                        tabsetPanel(
                          tabPanel(
                            title = "Gráfico",
                            plotOutput("distr_viol_votos",
                                       height = "1000px") %>% withSpinner(color = "#6C7B8B")
                          ),
                          tabPanel(
                            title = "Tabla de datos",
                            dataTableOutput("tabla_votos_dpto") %>% withSpinner(color = "#6C7B8B")
                          )
                        )
               ),
               tabPanel("Histogramas categoría por región",
                        h3("% de población comunal por región", align = "center"),
                        tabsetPanel(
                          tabPanel(
                            title = "Gráfico",
                            plotOutput("distr_hist_cat",
                                       height = "1000px") %>% withSpinner(color = "#6C7B8B")
                          ),
                          tabPanel(
                            title = "Tabla de datos",
                            dataTableOutput("tabla_cat_reg") %>% withSpinner(color = "#6C7B8B")
                          )
                        )
               ),
               tabPanel("Violines categoría por departamento",
                        h3("% de votos por departamento", align = "center"),
                        tabsetPanel(
                          tabPanel(
                            title = "Gráfico",
                            plotOutput("distr_viol_cat",
                                       height = "1000px") %>% withSpinner(color = "#6C7B8B")
                          ),
                          tabPanel(
                            title = "Tabla de datos",
                            dataTableOutput("tabla_cat_dpto") %>% withSpinner(color = "#6C7B8B")
                          )
                        )
               )
             )
           )
    )
  )
  
)
