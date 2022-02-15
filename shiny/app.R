library(shiny)
library(jsonlite)
library(tibble)
#library(plyr)
library(dplyr)
library(sf)
#library(funr)
#library(shinyscreenshot)
library(xml2)
library(tidyverse)
library(bslib)
#library(RCurl)
#library(webshot)

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
      cssLink.href = '../../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }"
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
      filter(id_ficha == input$id_ficha) %>% 
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
    header.json <- fromJSON("https://datos.canarias.es/api/estadisticas/cmetadata/v1.0/properties/metamac.app.style.header.url.json")
    header.html <- getURL(paste0(header.json$value, '?appName=Fichas%20municipales'))
    HTML(header.html)
  })

  output$footer <- renderUI({
    footer.json <- fromJSON("https://datos.canarias.es/api/estadisticas/cmetadata/v1.0/properties/metamac.app.style.footer.url.json")
    footer.html <- getURL(footer.json$value)
    HTML(footer.html)
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
    dir.fichero <- paste0("./output/",input$año_2, "/")
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha_2,]
    nombre.fichero <- paste0(input$id_ficha_2, "_",
                             input$año, "_",
                             ifelse(periodicidad2() == "M", paste0(periodicidad2(), input$mes_2, "_"), ""),
                             ifelse(periodicidad2() == "Q", paste0(periodicidad2(), as.numeric(input$trimestre_2), "_"), ""),
                             input$id_municipio_2, ".html")
    iframe_2 <- tags$iframe(src=paste0('frames/', dir.fichero, nombre.fichero), frameborder="0", scrolling="no", style = "width: 100%; border: 0; margin: 0 auto; display: block;", onload="resizeIframe(this)")
  })
  
  observeEvent(input$download, {
    dir.fichero <- paste0("./output/",input$año, "/")
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha,]
    nombre.fichero <- paste0(input$id_ficha, "_",
                             input$año, "_",
                             ifelse(periodicidad() == "M", paste0(periodicidad(), input$mes, "_"), ""),
                             ifelse(periodicidad() == "Q", paste0(periodicidad(), as.numeric(input$trimestre), "_"), ""),
                             input$id_municipio, ".html")
    webshot(url = paste0(dir.fichero, nombre.fichero), file = "test.png", cliprect = "viewport")
    #shinyapp <- shiny::shinyAppDir(appdir)
    #appshot(shinyapp, "01_hello_app.png", selector = "#report")
  })
  observeEvent(input$download_2, {
    screenshot(selector = "#report_2", filename =  paste0("ficha_", input$año_2, "_", input$id_municipio_2))
  })
  
  output$downloader <- downloadHandler(
      "results_from_shiny.pdf",
      content = 
        function(file)
        {
          dir.fichero <- paste0("../output/",input$año, "/")
          ficha_actual <- df_fichas[df_fichas$code == input$id_ficha,]
          nombre.fichero <- paste0(input$id_ficha, "_",
                                   input$año, "_",
                                   ifelse(periodicidad() == "M", paste0(periodicidad(), input$mes, "_"), ""),
                                   ifelse(periodicidad() == "Q", paste0(periodicidad(), as.numeric(input$trimestre), "_"), ""),
                                   input$id_municipio, ".html")
          print(paste0(dir.fichero, nombre.fichero))
          rmarkdown::render(
            input = "print/pdf.Rmd",
            output_file = "built_report.pdf",
            params = list(html.file = paste0(dir.fichero, nombre.fichero))
          ) 
          readBin(con = "print/built_report.pdf", 
                  what = "raw",
                  n = file.info("print/built_report.pdf")[, "size"]) %>%
            writeBin(con = file)
        }
    )

  option_year <- reactive({
    periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio) %>% 
      select(A) %>% 
      deframe
  })
  
  option_year_2 <- reactive({  periods %>% filter(code %in% input$id_ficha_2) %>% select(A) %>% deframe })
 
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
      filter(code %in% input$id_ficha_2 & A %in% input$año_2) %>% 
      select(M) %>%
      left_join(
        data.frame(M = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                   id = 1:12),
        by ='M') %>% 
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
      filter(code %in% input$id_ficha_2 & A %in% input$año_2) %>% 
      select(mes = Q) %>%
      left_join(
        data.frame(mes = c("Marzo", "Junio", "Septiembre", "Diciembre"),
                   id = c(3, 6, 9, 12),
                   Q = paste0("Trimestre ", seq(1:4))),
        by ='mes') %>% 
      select(Q, id) %>%
      deframe
  })
  
  output$select_island <- renderUI({ selectInput("id_isla", h3("Isla"), choices = option_island()) })
  output$select_island_2 <- renderUI({ selectInput("id_isla_2", "", choices = option_island()) })
  
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
  observeEvent(eventExpr = input$minus, handlerExpr = { output$fichas <- getLayout1() })
  
  output$ficha_params <- renderUI (
    fluidRow(
      column(3, uiOutput("select_ficha")),
      column(2, uiOutput("select_island")),
      column(2, uiOutput("select_mun")),
      column(2, uiOutput("select_year")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("select_month")),
             conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("select_trim"))),
      column(1, actionButton(inputId = "plus", label = "Añadir ficha"))
      #column(1, actionButton("download", "Descargar"))
    )
  )
  
  output$ficha_params_2 <- renderUI (
    fluidRow(
      column(3, uiOutput("select_ficha_2")),
      column(2, uiOutput("select_island_2")),
      column(2, uiOutput("select_mun_2")),
      column(2, uiOutput("select_year_2")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad_2 == "M"', uiOutput("select_month_2")),
             conditionalPanel(condition = 'output.periodicidad_2 == "Q"', uiOutput("select_trim_2"))),
      column(1, actionButton(inputId = "minus", label = "Quitar ficha"))
      #column(1, actionButton("download", "Descargar"))
    )
  )
  
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
        column(12, uiOutput("ficha_params")),
        column(12, uiOutput("ficha_params_2")),
        column(6, htmlOutput('report')),
        column(6, htmlOutput('report_2'))
      )
    ) 
  }
  
  output$fichas <- getLayout1()
}

shinyApp(ui = ui, server = server)
