

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
  aaaa <- reactive({str_extract_all(input$elec,"[0-9]{4}") %>% unlist
  })
  
  datos_graf_solo_insee <- reactive({
    datos_censales %>% 
      filter(AÑO == aaaa()) %>% 
      inner_join(COMUNAS_2007) %>% 
      filter(COD_REG %in% fr_anc_reg_metr$code) %>% 
      filter(Pob >= input$pob_min) %>% 
      left_join(otros_datos_comunales, by = c("CODGEO","AÑO")) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,req(input$cat)) %>% 
      dplyr::rename(Pct = UQ(req(input$cat))) %>% 
      filter(!is.na(Pct)) 
  })
  
  datos_graf <- reactive({
    datos_graf_solo_insee() %>% 
      inner_join(datos_electorales()) %>% 
      group_by(COD_REG) %>% 
      mutate(Alpha = 1/n())
  })
  
  output$graf_disper <- renderPlot({
    filter(paleta_tesis_fn, FAMILIA == input$familia) %>% 
      extract2("COLOR") %>% 
      geofacet_disp_votos_cat_reg(datos_graf(), 
                                  paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                  if_else(input$var %in% c("Escolaridad","Empleo"),
                                          "% población correspondiente",
                                          "% población comunal"),
                                  color = .)
  })
 
  output$graf_corr <- renderPlot({
    filter(paleta_tesis_fn, FAMILIA == input$familia) %>% 
      extract2("COLOR") %>% 
      geofacet_corr_votos_cat_dpto(datos_graf(), 
                                   paste("Correlación", etiqueta_cat(),"y",input$familia,"en las",input$elec,sep=" "),
                                   "Código INSEE de Departamento",
                                   "Coeficiente de correlación",
                                   color = .)
  })
  
  output$graf_smooth <- renderPlot({
    filter(paleta_tesis_fn, FAMILIA == input$familia) %>% 
      extract2("COLOR") %>% 
      geofacet_smooth_votos_cat_dpto(datos_graf(), 
                                     paste(etiqueta_cat(),"vs",input$familia,"en las",input$elec,sep=" "),
                                     if_else(input$var %in% c("Escolaridad","Empleo"),
                                             "% población correspondiente",
                                             "% población comunal"),
                                     color = .)
  })
  
  
  
}