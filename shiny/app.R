library(shiny)
library(jsonlite)
library(tibble)
library(dplyr)
library(sf)
library(xml2)
library(tidyverse)
library(bslib)
library(RCurl)

addResourcePath("frames", getwd())
directory.data <- "../data/"
directory.rmd <- "../Rmd/"
directory.output <- "output"
df_init_data <-  read.csv("init-data.csv", sep = ";") %>% 
  transform(filepath = paste0(directory.data, name, '.', extension),
            param.name = paste0('url.', name))

loadMunicipios <- function() {
  file <- paste0(directory.data, df_init_data[df_init_data$name == 'cl_area',]$name, '.', df_init_data[df_init_data$name == 'cl_area',]$extension)
  CL_AREA <- as_list(read_xml(file))
  
  df_CL_AREA <- tibble::as_tibble(CL_AREA) %>% 
    unnest_wider('codes') %>%
    transform(name = lapply(name, '[[', 2)) %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    readr::type_convert() %>%
    mutate(parent = gsub("^.*).", "", parent)) %>%
    select(code = id, value = name, parent)
  
  df_CL_AREA_mun <- df_CL_AREA %>% 
    select(id = code, municipio = value, code = parent) %>%
    filter(substring(id, 0,2) != "ES") %>% 
    transform(id = sub("\\_.*", "", id)) %>%
    filter(nchar(id) > 0 & !grepl("(", municipio, fixed = TRUE)) %>%
    left_join(df_CL_AREA, by="code") %>%
    select(id, municipio, code = parent, id_comarca = code, comarca = value) %>% 
    left_join(df_CL_AREA, by="code") %>% 
    select(id, municipio, id_comarca, comarca, code = parent, id_gran_comarca = code, gran_comarca = value) %>% 
    left_join(df_CL_AREA, by="code") %>% 
    select(id, municipio, id_comarca, comarca, id_gran_comarca, gran_comarca, code = parent, id_isla = code, isla = value) %>% 
    left_join(df_CL_AREA, by="code") %>% 
    select(id, municipio, id_comarca, comarca, id_gran_comarca, gran_comarca, id_isla, isla, provincia = value) %>%
    arrange(id) %>%
    arrange(id_isla) %>%
    arrange(provincia)
  
  return(df_CL_AREA_mun %>% select(id, municipio, id_isla, isla))
}

get_periods_from_files <- function(df_fichas) {
  output_fichas <- data.frame(code = list.dirs(path = directory.output, recursive = F, full.names = F)) %>% left_join(df_fichas, by = "code") %>% filter(!is.na(periodicidad))
  
  output_periods <- data.frame() #row.names = c("id_ficha", "A", "M", "Q", "O"))
  for(ficha_index in 1:nrow(output_fichas)) {
    id_ficha <- output_fichas$code[ficha_index]
    output_A <- data.frame(A = list.dirs(paste(directory.output, id_ficha, sep = "/"), recursive = F, full.names = F)) %>% filter(startsWith(A, "2"))
    for(A_index in 1:nrow(output_A)) {
      A <- output_A$A[A_index]
      output_filenames <- list.files(paste(directory.output, id_ficha, A, sep = "/"), recursive = F, full.names = F, pattern = "*.html")
      output_filepaths <- list.files(paste(directory.output, id_ficha, A, sep = "/"), recursive = F, full.names = T, pattern = "*.html")
      if(!is_empty(output_filenames)) {
        #Anual
        if(output_fichas$periodicidad[ficha_index] =="A") {
          output_municipio <- data.frame(
            id_ficha = id_ficha,
            A = A,
            period = NA,
            filename = output_filenames,
            path = output_filepaths) %>%
            transform(id_municipio = sub(paste0(".*", id_ficha, "_"), "", filename)) %>%
            transform(id_municipio = sub(".html", "", id_municipio)) %>% 
            select(-filename)
        } else {
          output_municipio <- data.frame(
            id_ficha = id_ficha,
            A = A,
            filename = output_filenames,
            path = output_filepaths) %>%
            transform(id_municipio = sub(paste0(".*", id_ficha, "_.*_"), "", filename), period = sub(paste0(".*", id_ficha, "_"), "", filename)) %>%
            transform(id_municipio = sub(".html", "", id_municipio), period = sub("_.*.html", "", period)) %>% 
            transform(period = sub("Q|M", "", period)) %>% 
            select(-filename)
        }
        output_periods <- rbind(
          output_periods, 
          output_municipio
        )
        
      }
    }
  }
  output_periods
}  

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "lumen"),
  includeCSS("www/styles.css"),
  tags$script(HTML(
    "function resizeIframe(obj) {
      obj.style.height = (obj.contentWindow.document.documentElement.scrollHeight + 100) + 'px';
      var cssLink = document.createElement('link');
      cssLink.href = '../../../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }
    
    function resizeGlosarioIframe(obj) {
      obj.style.height = (obj.contentWindow.document.documentElement.scrollHeight + 100) + 'px';
      var cssLink = document.createElement('link');
      cssLink.href = '../../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }
    
    function resizeAyudaIframe(obj) {
      obj.style.height = (obj.contentWindow.document.documentElement.scrollHeight + 100) + 'px';
      var cssLink = document.createElement('link');
      cssLink.href = '../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }
    "
  )),
  htmlOutput('header'),
  uiOutput("fichas"),
  htmlOutput('footer'),
)

server <- function(input, output) {
  df_fichas <- data.frame(code = list.dirs(path = directory.output, recursive = F, full.names = F)) %>% left_join(read.csv("fichas.csv", sep = ";"), by = "code") %>% filter(!is.na(periodicidad))
  periods <- get_periods_from_files(df_fichas)
  municipios <- loadMunicipios()
  
  option_island <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>%
      select(isla, id_isla) %>% unique %>% deframe
  })
  
  option_island_2 <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha_2) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>%
      select(isla, id_isla) %>% unique %>% deframe
  })
  
  option_mun <- reactive({ 
    periods %>% 
      filter(id_ficha == input$id_ficha) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>% 
      filter(id_isla %in% input$id_isla) %>% 
      select(municipio, id) %>% 
      deframe 
  })
  
  option_mun_2 <- reactive({ 
    periods %>% 
      filter(id_ficha == input$id_ficha_2) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>% 
      filter(id_isla %in% input$id_isla_2) %>% 
      select(municipio, id) %>% 
      deframe 
  })
  
  fichas <- df_fichas %>% select(description, code) %>% deframe
  
  output$periodicidad <- renderText ({
    periodicidad()
  })
  
  output$periodicidad_2 <- renderText ({
    periodicidad2()
  })
  
  periodicidad <- reactive({
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha,]
    ficha_actual$periodicidad
  })
  
  periodicidad2 <- reactive({
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha_2,]
    ficha_actual$periodicidad
  })
  
  outputOptions(output, "periodicidad", suspendWhenHidden = FALSE)
  outputOptions(output, "periodicidad_2", suspendWhenHidden = FALSE)
  
  output$header <- renderUI({
    #header.json <- fromJSON("https://datos.canarias.es/api/estadisticas/cmetadata/v1.0/properties/metamac.app.style.header.url.json")
    #header.html <- getURL(paste0(header.json$value, '?appName=Fichas%20municipales'))
    #header.html <- getURL('https://www3-pre.gobiernodecanarias.org/aplicaciones/appsistac/organisations/istac/common/header/header.html')
    includeHTML('www/header.html')
  })
  
  output$footer <- renderUI({
    #footer.json <- fromJSON("https://datos.canarias.es/api/estadisticas/cmetadata/v1.0/properties/metamac.app.style.footer.url.json")
    #footer.html <- getURL(footer.json$value)
    #footer.html <- getURL('https://www3-pre.gobiernodecanarias.org/aplicaciones/appsistac/organisations/istac/common/footer/footer.html')
    includeHTML('www/footer.html')
  })
  
  output$report <- renderUI({
    path_fichero <- (periods %>% filter(id_ficha == input$id_ficha & A == input$año & id_municipio == input$id_municipio))
    if(periodicidad() == "M") {
      path_fichero <- path_fichero %>% filter(period == input$mes)
    } else if(periodicidad() == "Q") {
      path_fichero <- path_fichero %>% filter(period == input$trimestre)
    }
    iframe <- tags$iframe(src=paste("frames", path_fichero$path, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeIframe(this)")
  })
  
  output$report_2 <- renderUI({
    path_fichero <- (periods %>% filter(id_ficha == input$id_ficha_2 & A == input$año_2 & id_municipio == input$id_municipio_2))
    if(periodicidad2() == "M") {
      path_fichero <- path_fichero %>% filter(period == input$mes_2)
    } else if(periodicidad2() == "Q") {
      path_fichero <- path_fichero %>% filter(period == input$trimestre_2)
    }
    iframe_2 <- tags$iframe(src=paste('frames', path_fichero$path, sep= "/"), frameborder="0", scrolling="no", style = "width: 100%; border: 0; margin: 0 auto; display: block;", onload="resizeIframe(this)")
  })
  
  output$glosario <- renderUI({
    ficha <- df_fichas %>% filter(code == input$id_ficha)
    iframe <- tags$iframe(src=paste("frames", "output", ficha$code, ficha$glosario, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeGlosarioIframe(this)")
  })
  
  output$glosario_2 <- renderUI({
    ficha <- df_fichas %>% filter(code == input$id_ficha_2)
    iframe <- tags$iframe(src=paste("frames", "output", ficha$code, ficha$glosario, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeGlosarioIframe(this)")
  })
  
  output$ayuda <- renderUI({
    iframe <- tags$iframe(src=paste("frames", "output", "MODULO_AYUDA.html", sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeAyudaIframe(this)")
  })
  
  option_year <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio) %>% 
      select(A) %>% 
      deframe
  })
  
  option_year_2 <- reactive({ 
    periods %>% 
      filter(id_ficha %in% input$id_ficha_2 & id_municipio == input$id_municipio_2) %>% 
      select(A) %>% 
      deframe 
  })
 
  option_month <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio & A == input$año) %>% 
      select(id = period) %>%
      left_join(
        data.frame(M = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
               id = as.character(1:12)),
        by ='id') %>% 
      select(M, id) %>%
    deframe
  })
  
  option_month_2 <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha_2 & id_municipio == input$id_municipio_2 & A == input$año_2) %>% 
      select(id = period) %>%
      left_join(
        data.frame(M = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                   id = as.character(1:12)),
        by ='id') %>% 
      select(M, id) %>%
      deframe
  })
  
  option_ficha <- reactive({df_fichas %>% select(description, code) %>% deframe})
  
  option_trim <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio & A == input$año) %>% 
      select(id = period) %>%
      left_join(
        data.frame(mes = c("Marzo", "Junio", "Septiembre", "Diciembre"),
                   id = c("3", "6", "9", "12"),
                   Q = paste0("Trimestre ", seq(1:4))),
        by ='id') %>% 
      select(Q, id) %>%
      deframe
  })
  
  option_trim_2 <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha_2 & id_municipio == input$id_municipio_2 & A == input$año_2) %>% 
      select(id = period) %>%
      left_join(
        data.frame(mes = c("Marzo", "Junio", "Septiembre", "Diciembre"),
                   id = c("3", "6", "9", "12"),
                   Q = paste0("Trimestre ", seq(1:4))),
        by ='id') %>% 
      select(Q, id) %>%
      deframe
  })
  
  output$select_island <- renderUI({ selectInput("id_isla", h3("Isla"), choices = option_island()) })
  output$select_island_2 <- renderUI({ selectInput("id_isla_2", "", choices = option_island_2()) })
  
  output$select_mun <- renderUI({ selectInput("id_municipio", h3("Municipio"), choices = option_mun()) })
  output$select_mun_2 <- renderUI({ selectInput("id_municipio_2", "", choices = option_mun_2()) })
  
  output$select_ficha <- renderUI({ selectInput('id_ficha', h3('Ficha'), choices=option_ficha()) })
  output$select_ficha_2 <- renderUI({ selectInput('id_ficha_2', "", choices=option_ficha()) })
  
  output$select_year <- renderUI({ selectInput("año", h3("Año"), choices = option_year()) })
  output$select_year_2 <- renderUI({ selectInput("año_2", "", choices = option_year_2()) })
  
  output$select_month <- renderUI({ selectInput("mes", h3("Periodo"), choices = option_month()) })
  output$select_month_2 <- renderUI({ selectInput("mes_2", "", choices = option_month_2()) })
  
  output$select_trim <- renderUI({ selectInput("trimestre", h3("Periodo"), choices = option_trim()) })
  output$select_trim_2 <- renderUI({ selectInput("trimestre_2", "", choices = option_trim_2()) })
  
  observeEvent(eventExpr = input$plus, handlerExpr = { output$fichas <- getLayout2() })
  observeEvent(eventExpr = input$plus_3, handlerExpr = { output$fichas <- getLayout4() })
  observeEvent(eventExpr = input$minus_2, handlerExpr = { output$fichas <- getLayout1() })
  observeEvent(eventExpr = input$minus_4, handlerExpr = { output$fichas <- getLayout3() })
  observeEvent(eventExpr = input$ver_glosario, handlerExpr = { output$fichas <- getLayout3() })
  observeEvent(eventExpr = input$ver_glosario_2, handlerExpr = { output$fichas <- getLayout4() })
  observeEvent(eventExpr = input$ocultar_glosario_3, handlerExpr = { output$fichas <- getLayout1() })
  observeEvent(eventExpr = input$ocultar_glosario_4, handlerExpr = { output$fichas <- getLayout2() })
  observeEvent(eventExpr = input$ayuda, handlerExpr = { output$fichas <- getLayout5() })
  
  output$pdf <- downloadHandler(
    filename = function() {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha & A == input$año & id_municipio == input$id_municipio))
      if(periodicidad() == "M") {
        path_fichero <- path_fichero %>% filter(period == input$mes)
      } else if(periodicidad() == "Q") {
        path_fichero <- path_fichero %>% filter(period == input$trimestre)
      }
      path_fichero <- strsplit(x = sub("html", "pdf", path_fichero$path), split="/")[[1]]
      path_fichero[length(path_fichero)]
    },
    content = function(con) {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha & A == input$año & id_municipio == input$id_municipio))
      if(periodicidad() == "M") {
        path_fichero <- path_fichero %>% filter(period == input$mes)
      } else if(periodicidad() == "Q") {
        path_fichero <- path_fichero %>% filter(period == input$trimestre)
      }
      
      filename <- sub("html", "pdf", path_fichero$path)
      print(filename)
      file.copy(filename, con)
    },
    contentType = "application/PDF"
  )
  
  output$pdf_2 <- downloadHandler(
    filename = function() {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha_2 & A == input$año_2 & id_municipio == input$id_municipio_2))
      if(periodicidad2() == "M") {
        path_fichero <- path_fichero %>% filter(period == input$mes_2)
      } else if(periodicidad2() == "Q") {
        path_fichero <- path_fichero %>% filter(period == input$trimestre_2)
      }
      path_fichero <- strsplit(x = sub("html", "pdf", path_fichero$path), split="/")[[1]]
      path_fichero[length(path_fichero)]
    },
    content = function(con) {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha_2 & A == input$año_2 & id_municipio == input$id_municipio_2))
      if(periodicidad2() == "M") {
        path_fichero <- path_fichero %>% filter(period == input$mes_2)
      } else if(periodicidad2() == "Q") {
        path_fichero <- path_fichero %>% filter(period == input$trimestre_2)
      }
      
      filename <- sub("html", "pdf", path_fichero$path)
      print(filename)
      file.copy(filename, con)
    },
    contentType = "application/PDF"
  )
  
  output$ficha_params <- renderUI (
    fluidRow(class = "params-row",
      column(3, uiOutput("select_ficha")),
      column(2, uiOutput("select_island")),
      column(2, uiOutput("select_mun")),
      column(1, uiOutput("select_year")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("select_month")),
             conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("select_trim"))),
      column(2, 
             actionButton(inputId = "plus", icon = icon("plus"), label = ""), 
             actionButton("ver_glosario", label = "Ver glosario"), 
             downloadButton("pdf", "PDF"),
             actionButton("ayuda", icon = icon("question"), label = ""))
    )
  )
  
  output$ficha_params_2 <- renderUI (
    fluidRow(class = "params-row",
      column(3, uiOutput("select_ficha")),
      column(2, uiOutput("select_island")),
      column(2, uiOutput("select_mun")),
      column(1, uiOutput("select_year")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("select_month")),
             conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("select_trim"))),
      column(2, downloadButton("pdf", "PDF"), actionButton("ayuda", icon = icon("question"), label = "")),
      
      column(3, uiOutput("select_ficha_2")),
      column(2, uiOutput("select_island_2")),
      column(2, uiOutput("select_mun_2")),
      column(1, uiOutput("select_year_2")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad_2 == "M"', uiOutput("select_month_2")),
             conditionalPanel(condition = 'output.periodicidad_2 == "Q"', uiOutput("select_trim_2"))),
      column(2, actionButton(inputId = "minus_2", icon = icon("minus"), label = ""), actionButton("ver_glosario_2", label = "Ver glosario"),
             downloadButton("pdf_2", "PDF"))
    )
  )
  
  output$ficha_params_3 <- renderUI (
    fluidRow(class = "params-row",
             column(3, uiOutput("select_ficha")),
             column(2, uiOutput("select_island")),
             column(2, uiOutput("select_mun")),
             column(1, uiOutput("select_year")),
             column(2, 
                    conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("select_month")),
                    conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("select_trim"))),
             column(2, actionButton(inputId = "plus_3", icon = icon("plus"), label = ""), actionButton("ocultar_glosario_3", label = "Ocultar glosario"),
                    downloadButton("pdf", "PDF"), actionButton("ayuda", icon = icon("question"), label = ""))
    )
  )
  
  
  output$ficha_params_4 <- renderUI (
    fluidRow(class = "params-row",
             column(3, uiOutput("select_ficha")),
             column(2, uiOutput("select_island")),
             column(2, uiOutput("select_mun")),
             column(1, uiOutput("select_year")),
             column(2, 
                    conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("select_month")),
                    conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("select_trim"))),
             column(2, downloadButton("pdf", "PDF"), actionButton("ayuda", icon = icon("question"), label = "")),
             
             column(3, uiOutput("select_ficha_2")),
             column(2, uiOutput("select_island_2")),
             column(2, uiOutput("select_mun_2")),
             column(1, uiOutput("select_year_2")),
             column(2, 
                    conditionalPanel(condition = 'output.periodicidad_2 == "M"', uiOutput("select_month_2")),
                    conditionalPanel(condition = 'output.periodicidad_2 == "Q"', uiOutput("select_trim_2"))),
             column(2, actionButton(inputId = "minus_4", icon = icon("minus"), label = ""), actionButton("ocultar_glosario_4", label = "Ocultar glosario"),
                    downloadButton("pdf2", "PDF"))
    )
  )
  
  output$ficha_params_5 <- renderUI (fluidRow(column(1), column(5, actionButton(inputId = "minus_2", label = "Cerrar ayuda")), column(6)))
  
  getLayout1 <- function() {
    renderUI (
      fluidRow(
        column(12, uiOutput("ficha_params")),
        column(12, htmlOutput('report'))
      )
    )
  }
   
  getLayout2 <- function() {
    renderUI (
      fluidRow(
        column(12, uiOutput("ficha_params_2")),
        column(6, htmlOutput('report')),
        column(6, htmlOutput('report_2'))
      )
    ) 
  }

  getLayout3 <- function() {
    renderUI (
      fluidRow(
        column(12, uiOutput("ficha_params_3")),
        column(6, htmlOutput('report')),
        column(6, htmlOutput('glosario'))
      )
    ) 
  }
  
  getLayout4 <- function() {
    renderUI (
      fluidRow(
        column(12, uiOutput("ficha_params_4")),
        column(6, htmlOutput('report')),
        column(6, htmlOutput('report_2')),
        column(6, htmlOutput('glosario')),
        column(6, htmlOutput('glosario_2'))
      )
    ) 
  }
  
  getLayout5 <- function() {
    renderUI (
      fluidRow(
        column(12, uiOutput("ficha_params_5")),
        column(12, htmlOutput('ayuda'))
      )
    ) 
  }
  
  output$fichas <- getLayout1()
}

shinyApp(ui = ui, server = server)
