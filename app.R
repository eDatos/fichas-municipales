#*******************************************************#
#* COMPARADOR INTERACTIVO DE FICHAS MUNICIPALES
#* Desarrollado por: Luis Manuel Pérez Geraldino
#* Fecha versión: Diciembre 2021.
#* Plataforma: R-Shiny
#*******************************************************#

library(shiny)
library(jsonlite)
library(tibble)
library(dplyr)
library(xml2)
library(tidyverse)
library(bslib)
library(RCurl)

addResourcePath("frames", getwd())
directory.data <- "data/"
directory.rmd <- "Rmd/"
directory.output <- "output"
df_init_data <-  read.csv("init-data.csv", sep = ";", encoding = "UTF-8") %>% 
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
            transform(period = as.numeric(sub("Q|M", "", period))) %>% 
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
      var scale = Math.min(1, obj.parentElement.offsetWidth / 940);
      obj.style.height = obj.contentWindow.document.documentElement.scrollHeight + 'px';
      //obj.parentElement.style.height = obj.style.height;
      obj.style.transform = 'scale(' + scale + ')';
      //console.log(Math.min(1, obj.offsetWidth / 940));
      var cssLink = document.createElement('link');
      cssLink.href = '../../../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }
    
    function resizeGlosarioIframe(obj) {
      var scale = Math.min(1, obj.parentElement.offsetWidth / 940);
      obj.style.height = obj.contentWindow.document.documentElement.scrollHeight + 'px';
      //obj.parentElement.style.height = obj.style.height;
      obj.style.transform = 'scale(' + scale + ')';
      var cssLink = document.createElement('link');
      cssLink.href = '../../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }
    
    function resizeAyudaIframe(obj) {
      var scale = Math.min(1, obj.parentElement.offsetWidth / 940);
      obj.style.height = obj.contentWindow.document.documentElement.scrollHeight + 'px';
      //obj.parentElement.style.height = obj.style.height;
      obj.style.transform = 'scale(' + scale + ')';
      var cssLink = document.createElement('link');
      cssLink.href = '../www/styles.css'; 
      cssLink.rel = 'stylesheet'; 
      cssLink.type = 'text/css'; 
      obj.contentWindow.document.head.appendChild(cssLink);
    }
    
    "
  )),
  htmlOutput('header'),
  uiOutput("menu"),
  uiOutput("fichas"),
  htmlOutput('footer')
)

server <- function(input, output) {
  df_fichas <- data.frame(code = list.dirs(path = directory.output, recursive = F, full.names = F)) %>% left_join(read.csv("fichas.csv", sep = ";", encoding = "UTF-8"), by = "code") %>% filter(!is.na(periodicidad))
  periods <- get_periods_from_files(df_fichas)
  municipios <- loadMunicipios()
  
  option_island <- reactive({
    shiny::validate(
      need(input$id_ficha != "", label = "Seleccionar ficha...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>%
      select(isla, id_isla) %>% unique %>% 
      arrange(isla) %>% deframe
  })
  
  option_island_2 <- reactive({
    shiny::validate(
      need(input$id_ficha_2 != "", "Seleccionar ficha...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha_2) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>%
      select(isla, id_isla) %>% unique %>% 
      rbind(data.frame(isla = " Seleccione...", id_isla = "")) %>% 
      arrange(isla) %>% deframe
  })
  
  option_mun <- reactive({
    shiny::validate(
      need(input$id_ficha != "", ""),
      need(input$id_isla != "", "Seleccionar isla...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>% 
      filter(id_isla %in% input$id_isla) %>% 
      select(municipio, id) %>% 
      arrange(municipio) %>%
      deframe 
  })
  
  option_mun_2 <- reactive({ 
    shiny::validate(
      need(input$id_ficha_2 != "", ""),
      need(input$id_isla_2 != "", "Seleccionar isla...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha_2) %>% 
      select(id = id_municipio) %>%
      unique %>%
      left_join(municipios, by = "id") %>% 
      filter(id_isla %in% input$id_isla_2) %>% 
      select(municipio, id) %>% 
      rbind(data.frame(municipio = " Seleccione...", id = "")) %>% 
      arrange(municipio) %>%
      deframe 
  })
  
  fichas <- df_fichas %>% select(description, code) %>% deframe
  
  output$periodicidad <- renderText ({
    periodicidad()
  })
  
  output$periodicidad_2 <- renderText ({
    periodicidad2()
  })
  
  output$tab <- renderText ({ "fichas" })
  
  periodicidad <- reactive({
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha,]
    ficha_actual$periodicidad
  })
  
  periodicidad2 <- reactive({
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha_2,]
    ficha_actual$periodicidad
  })
  
  output$need_layout_2_fichas <- renderText({need_layout_2_fichas()})
  
  need_layout_2_fichas <- reactive({
    
    if(is.null(input$id_ficha_2) || input$id_ficha_2 == "") {
      return(FALSE)
    }
    ficha_actual <- df_fichas[df_fichas$code == input$id_ficha_2,]
    periodicidad <- ficha_actual$periodicidad
    if(is.null(periodicidad)) {
      return(FALSE)
    }
    if(input$id_isla_2 == "") {
      return(FALSE)
    }
    if(input$id_municipio_2 == "") {
      return(FALSE)
    }
    if(periodicidad == "M") {
      if(!is.null(input$mes_2) && input$mes_2 != 0 && input$mes_2 != "") {
        return(TRUE)
      }
    } else if(periodicidad == "Q") {
      if(!is.null(input$trimestre_2) && input$trimestre_2 != 0 && input$trimestre_2 != "") {
        return(TRUE)
      }
    } else if(periodicidad == "A") {
      if(!is.null(input$anio_2) && input$anio_2 != 0 && input$anio_2 != "") {
        return(TRUE)
      }
    }
    return(FALSE)
  })
  
  outputOptions(output, "periodicidad", suspendWhenHidden = FALSE)
  outputOptions(output, "periodicidad_2", suspendWhenHidden = FALSE)
  outputOptions(output, "need_layout_2_fichas", suspendWhenHidden = FALSE)
  outputOptions(output, "tab", suspendWhenHidden = FALSE)

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
    shiny::validate(
      need(input$id_ficha != "", ""),
      need(input$anio != "", ""),
      need(input$id_municipio != "", "")
    )
    
    path_fichero <- (periods %>% filter(id_ficha == input$id_ficha & A == input$anio & id_municipio == input$id_municipio))
    if(periodicidad() == "M") {
      shiny::validate(need(input$mes != "", ""))
      path_fichero <- path_fichero %>% filter(period == input$mes)
    } else if(periodicidad() == "Q") {
      shiny::validate(need(input$trimestre != "", ""))
      path_fichero <- path_fichero %>% filter(period == input$trimestre)
    }
    iframe <- tags$iframe(src=paste("frames", path_fichero$path, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 940px; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888; transform-origin: left top;", onload="resizeIframe(this)")
  })
  
  output$report_2 <- renderUI({
    shiny::validate(
      need(input$id_ficha_2 != "", ""),
      need(input$anio_2 != "", ""),
      need(input$id_municipio_2 != "", "")
    )
    
    path_fichero <- (periods %>% filter(id_ficha == input$id_ficha_2 & A == input$anio_2 & id_municipio == input$id_municipio_2))
    if(periodicidad2() == "M") {
      shiny::validate(need(input$mes_2 != "", ""))
      path_fichero <- path_fichero %>% filter(period == input$mes_2)
    } else if(periodicidad2() == "Q") {
      shiny::validate(need(input$trimestre_2 != "", ""))
      path_fichero <- path_fichero %>% filter(period == input$trimestre_2)
    }
    iframe_2 <- tags$iframe(src=paste('frames', path_fichero$path, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 940px; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888; transform-origin: left top;", onload="resizeIframe(this)")
  })
  
  output$glosario <- renderUI({
    shiny::validate(need(input$id_ficha != "", ""))
    ficha <- df_fichas %>% filter(code == input$id_ficha)
    iframe <- tags$iframe(src=paste("frames", "output", ficha$code, ficha$glosario, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeGlosarioIframe(this)")
  })
  
  output$glosario_2 <- renderUI({
    shiny::validate(need(input$id_ficha_2 != "", ""))
    ficha <- df_fichas %>% filter(code == input$id_ficha_2)
    iframe <- tags$iframe(src=paste("frames", "output", ficha$code, ficha$glosario, sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeGlosarioIframe(this)")
  })
  
  output$ayuda <- renderUI({
    iframe <- tags$iframe(src=paste("frames", "output", "MODULO_AYUDA.html", sep= "/"), frameborder="0", scrolling="no", class = "paper", style = "width: 100%; border: 0; margin: 0 auto; display: block; box-border: 5px 10px 18px #888888;", onload="resizeAyudaIframe(this)")
  })
  
  option_year <- reactive({
    shiny::validate(
      need(input$id_ficha != "", ""),
      need(input$id_municipio != "", "Seleccionar municipio...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio) %>% 
      select(A) %>% 
      deframe %>% unique
  })
  
  option_year_2 <- reactive({ 
    shiny::validate(
      need(input$id_ficha_2 != "", ""),
      need(input$id_municipio_2 != "", "Seleccionar municipio...")
    )
    
    periods %>% 
      filter(id_ficha %in% input$id_ficha_2 & id_municipio == input$id_municipio_2) %>% 
      mutate(id = A) %>% 
      select(A, id) %>% 
      rbind(data.frame(A = " Seleccione...", id = "")) %>% 
      arrange(A) %>%
      deframe %>% unique
  })
 
  option_month <- reactive({
    shiny::validate(
      need(input$id_ficha != "", ""),
      need(input$id_municipio != "", ""),
      need(input$anio != "", "Seleccionar año...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio & A == input$anio) %>% 
      select(id = period) %>%
      left_join(
        data.frame(M = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
               id = 1:12),
        by ='id') %>% 
      select(M, id) %>%
      arrange(id) %>%
      deframe
  })
  
  option_month_2 <- reactive({
    shiny::validate(
      need(input$id_ficha_2 != "", ""),
      need(input$id_municipio_2 != "", ""),
      need(input$anio_2 != "", "Seleccionar año...")
    )
    
    periods %>% 
      filter(id_ficha == input$id_ficha_2 & id_municipio == input$id_municipio_2 & A == input$anio_2) %>% 
      select(id = period) %>%
      left_join(
        data.frame(M = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                   id = 1:12),
        by ='id') %>%
      mutate(rank = as.numeric(id)) %>%
      rbind(data.frame(M = " Seleccione...", id = "", rank = 0)) %>%
      arrange(rank) %>%
      select(M, id) %>%
      deframe
  })
  
  option_ficha <- reactive({df_fichas %>% select(description, code) %>% arrange(description) %>% deframe })
  option_ficha_2 <- reactive({df_fichas %>% select(description, code) %>% rbind(data.frame(description = " Seleccione...", code = "")) %>% arrange(description) %>% deframe })
  
  option_trim <- reactive({
    shiny::validate(
      need(input$id_ficha != "", ""),
      need(input$id_isla != "", ""),
      need(input$anio != "", "Seleccionar año...")
    )
    
    current_periods <- periods %>% 
      filter(id_ficha == input$id_ficha & id_municipio == input$id_municipio & A == input$anio) %>% 
      select(id = period)
    trim_M_periods <- data.frame(id = seq(3, 12, 3), Q = paste0("Trimestre ", 1:4))
    trim_Q_periods <- data.frame(id = 1:4, Q = paste0("Trimestre ", 1:4))
    trim_selected <- trim_M_periods
    if(all(current_periods$id <= 4)) {
      trim_selected <- trim_Q_periods 
    }

    current_periods %>%
      left_join(trim_selected, by ='id') %>% 
      select(Q, id) %>%
      arrange(id) %>%
      deframe
  })
  
  option_trim_2 <- reactive({
    shiny::validate(
      need(input$id_ficha_2 != "", ""),
      need(input$id_isla_2 != "", ""),
      need(input$anio_2 != "", "Seleccionar año...")
    )
    
    current_periods <- periods%>% 
      filter(id_ficha == input$id_ficha_2 & id_municipio == input$id_municipio_2 & A == input$anio_2) %>% 
      select(id = period)
    trim_M_periods <- data.frame(id = seq(3, 12, 3), Q = paste0("Trimestre ", 1:4))
    trim_Q_periods <- data.frame(id = 1:4, Q = paste0("Trimestre ", 1:4))
    trim_selected <- trim_M_periods
    if(all(current_periods$id <= 4)) {
      trim_selected <- trim_Q_periods 
    }
    current_periods %>%
      left_join(trim_selected, by ='id') %>% 
      mutate(rank = as.numeric(id)) %>%
      rbind(data.frame(Q = " Seleccione...", id = "", rank = 0)) %>%
      arrange(rank) %>%
      select(Q, id) %>%
      deframe
  })
  
  output$header_island <- renderUI({ h3("Isla") })
  output$select_island <- renderUI({ selectInput("id_isla", "", choices = option_island()) })
  output$select_island_2 <- renderUI({ selectInput("id_isla_2", "", choices = option_island_2(), selected = 1) })
  
  output$header_mun <- renderUI({ h3("Municipio") })
  output$select_mun <- renderUI({ selectInput("id_municipio", "", choices = option_mun()) })
  output$select_mun_2 <- renderUI({ selectInput("id_municipio_2", "", choices = option_mun_2(), selected = 1) })
  
  output$header_ficha <- renderUI({ h3('Ficha') })
  output$select_ficha <- renderUI({ selectInput('id_ficha', "", choices=option_ficha()) })
  output$select_ficha_2 <- renderUI({ selectInput('id_ficha_2', "", choices=option_ficha_2(), selected = 1) })
  
  output$header_year <- renderUI({ h3("Año") })
  output$select_year <- renderUI({ selectInput("anio", "", choices = option_year(), selected = option_year()[length(option_year())]) })
  output$select_year_2 <- renderUI({ selectInput("anio_2", "", choices = option_year_2(), selected=1) })
  
  output$header_month <- renderUI({ h3("Periodo") })
  output$select_month <- renderUI({ selectInput("mes", "", choices = option_month(), selected = option_month()[length(option_month())]) })
  output$select_month_2 <- renderUI({ selectInput("mes_2", "", choices = option_month_2(), selected = 0) })
  
  output$header_trim <- renderUI({ h3("Periodo") })
  output$select_trim <- renderUI({ selectInput("trimestre", "", choices = option_trim(), selected = option_trim()[length(option_trim())]) })
  output$select_trim_2 <- renderUI({ selectInput("trimestre_2", "", choices = option_trim_2(), selected = 0) })
  
  observeEvent(eventExpr = input$ver_fichas, handlerExpr = 
                 { 
                   output$tab <- renderText ({"fichas"});
                   if(need_layout_2_fichas()) { 
                     output$fichas <- getLayout2() ;
                   } else { 
                     output$fichas <- getLayout1();
                   }
                 }
              )
  observeEvent(eventExpr = input$ver_glosario, handlerExpr = 
                 { 
                   output$tab <-  renderText ({"glosario"}); 
                   if(need_layout_2_fichas()) { 
                     output$fichas <- getLayout4() 
                   } else { 
                     output$fichas <- getLayout3() 
                   }
                 }
              )
  observeEvent(eventExpr = input$ver_ayuda, handlerExpr = { output$tab <- renderText ({"ayuda"}); output$fichas <- getLayout5(); })

  observeEvent(eventExpr = input$mes_2, handlerExpr = { if(periodicidad2() == "M" && input$mes_2 != "") output$fichas <- getLayout2() })
  observeEvent(eventExpr = input$trimestre_2, handlerExpr = { if(periodicidad2() == "Q" && input$trimestre_2 != "") output$fichas <- getLayout2() })
  observeEvent(eventExpr = input$anio_2, handlerExpr = { if(periodicidad2() == "A" && input$anio_2 != "") output$fichas <- getLayout2() })

  output$pdf <- downloadHandler(
    filename = function() {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha & A == input$anio & id_municipio == input$id_municipio))
      if(periodicidad() == "M") {
        path_fichero <- path_fichero %>% filter(period == input$mes)
      } else if(periodicidad() == "Q") {
        path_fichero <- path_fichero %>% filter(period == input$trimestre)
      }
      path_fichero <- strsplit(x = sub("html", "pdf", path_fichero$path), split="/")[[1]]
      path_fichero[length(path_fichero)]
    },
    content = function(con) {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha & A == input$anio & id_municipio == input$id_municipio))
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
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha_2 & A == input$anio_2 & id_municipio == input$id_municipio_2))
      if(periodicidad2() == "M") {
        path_fichero <- path_fichero %>% filter(period == input$mes_2)
      } else if(periodicidad2() == "Q") {
        path_fichero <- path_fichero %>% filter(period == input$trimestre_2)
      }
      path_fichero <- strsplit(x = sub("html", "pdf", path_fichero$path), split="/")[[1]]
      path_fichero[length(path_fichero)]
    },
    content = function(con) {
      path_fichero <- (periods %>% filter(id_ficha == input$id_ficha_2 & A == input$anio_2 & id_municipio == input$id_municipio_2))
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
  
  output$pdf_3 <- downloadHandler(
    filename = function() {
      ficha <- df_fichas %>% filter(code == input$id_ficha)
      sub("html", "pdf", ficha$glosario)
    },
    content = function(con) {
      ficha <- df_fichas %>% filter(code == input$id_ficha)
      filename <- paste("output", ficha$code, sub("html", "pdf", ficha$glosario), sep = "/")
      print(filename)
      file.copy(filename, con)
    },
    contentType = "application/PDF"
  )
  
  output$pdf_4 <- downloadHandler(
    filename = function() {
      ficha <- df_fichas %>% filter(code == input$id_ficha_2)
      sub("html", "pdf", ficha$glosario)
    },
    content = function(con) {
      ficha <- df_fichas %>% filter(code == input$id_ficha_2)
      filename <- paste("output", ficha$code, sub("html", "pdf", ficha$glosario), sep = "/")
      print(filename)
      file.copy(filename, con)
    },
    contentType = "application/PDF"
  )
  
  output$ficha_params <- renderUI (
    fluidRow(class = "params-row",
      column(3, uiOutput("header_ficha")),
      column(2, uiOutput("header_island")),
      column(2, uiOutput("header_mun")),
      column(2, uiOutput("header_year")),
      column(2, 
            conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("header_month")),
            conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("header_trim"))
      ),
      column(1),
      
      column(3, uiOutput("select_ficha")),
      column(2, uiOutput("select_island")),
      column(2, uiOutput("select_mun")),
      column(1, uiOutput("select_year")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad == "M"', uiOutput("select_month")),
             conditionalPanel(condition = 'output.periodicidad == "Q"', uiOutput("select_trim"))),
      column(2, downloadButton("pdf", "Descargar PDF") 
             ),
      
      column(3, uiOutput("select_ficha_2")),
      column(2, uiOutput("select_island_2")),
      column(2, uiOutput("select_mun_2")),
      column(1, uiOutput("select_year_2")),
      column(2, 
             conditionalPanel(condition = 'output.periodicidad_2 == "M"', uiOutput("select_month_2")),
             conditionalPanel(condition = 'output.periodicidad_2 == "Q"', uiOutput("select_trim_2"))),
      column(2, conditionalPanel(condition = 'output.need_layout_2_fichas == "TRUE"', downloadButton("pdf_2", "Descargar PDF")))
    )
  )
  
  output$glosario_params <- renderUI (
    fluidRow(class = "glosario-row",
             column(12, conditionalPanel(condition = 'output.need_layout_2_fichas != "TRUE"', downloadButton("pdf_3", "Descargar PDF"))),
             column(6, conditionalPanel(condition = 'output.need_layout_2_fichas == "TRUE"', downloadButton("pdf_3", "Descargar PDF"))),
             column(6, conditionalPanel(condition = 'output.need_layout_2_fichas == "TRUE"', downloadButton("pdf_4", "Descargar PDF")))
    )
  )
  
  getLayout1 <- function() {
    renderUI (
      fluidRow(
        column(12,htmlOutput('report'))
      )
    )
  }
   
  getLayout2 <- function() {
    renderUI (
      fluidRow(
        column(6, htmlOutput('report')),
        column(6, htmlOutput('report_2'))
      )
    ) 
  }

  getLayout3 <- function() {
    renderUI (
      fluidRow(style = "margin-top: 20px;",
        column(12, htmlOutput('glosario'))
      )
    ) 
  }
  
  getLayout4 <- function() {
    renderUI (
      fluidRow(style = "margin-top: 20px;",
        column(6, htmlOutput('glosario')),
        column(6, htmlOutput('glosario_2'))
      )
    ) 
  }
  
  getLayout5 <- function() {
    renderUI (
      fluidRow(style = "margin-top: 20px;",
        column(12, htmlOutput('ayuda'))
      )
    ) 
  }
  
  output$menu <- renderUI(
    fluidRow(
      column(12, conditionalPanel(condition = 'output.tab == "fichas"', uiOutput("ficha_params"))),
      column(12, conditionalPanel(condition = 'output.tab == "glosario"', uiOutput("glosario_params")))
    )
  )
  outputOptions(output, "menu", suspendWhenHidden = FALSE)
  
  output$fichas <- getLayout1()
}

shinyApp(ui = ui, server = server)
