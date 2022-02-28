library(dplyr)
library(xml2)
library(sf)
library(tibble)
library(tidyverse)
library(jsonlite)
library(RCurl)
library(data.table)

#Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")
directory.data <- "data/"
directory.rmd <- "Rmd/"
directory.output <- "output"
df_init_data <-  read.csv("init-data.csv", encoding = "UTF-8", sep = ";") %>% 
  transform(filepath = paste0(directory.data, name, '.', extension),
            param.name = paste0('url.', name))
df_fichas <- read.csv("fichas.csv", encoding = "UTF-8", sep = ";")

downloadData <- function() {
  for (i in 1:nrow(df_init_data)) {
    if(file.exists(df_init_data$filepath[i])) {
      next
    }
    print(paste0('Download resource: ', df_init_data$name[i]))
    if(df_init_data$extension[i] == "json") {
      write(getURL( df_init_data$url[i], httpheader = c(Accept = "application/json"), .encoding = "UTF-8"), df_init_data$filepath[i])
    } else {
      download.file(df_init_data$url[i], df_init_data$filepath[i])
    }
  }
}

get_resource_period_list <- function(init_data_row) {
  source <- fromJSON(init_data_row$filepath) 
  result <- NULL
  if(grepl("/api/", init_data_row$url, fixed = T)) {
    result <- names(source[["dimension"]][["TIME"]][["representation"]]$index)
    if(is.null(result)) {
      result <- source[["dimension"]][["TIME"]][["representation"]]$title$es
      if(is.null(result)) {
        result <- source$metadata$temporalCoverages$item$id
      }
    }
  } else {
    result <- source$categories$labels[[which(source$categories$variable == source$temporals)]]
  }
  if(any(grepl("-", result))) {
    result <- data.frame(code = sub("-M", "-", result)) %>% 
      left_join(fromJSON('https://datos.canarias.es/api/estadisticas/indicators/v1.0/indicators/PARO_REGISTRADO')[["dimension"]][["TIME"]][["representation"]], 
        by = "code"
      ) %>%
      filter(!is.na(title$es))
    result <- result$title$es
  }
  result
}

get_resources_period <- function(df_init_data) {
  result <- get_resource_period_list(df_init_data[1, ])
  for(i in 2:nrow(df_init_data)) {
    result <- intersect(result, get_resource_period_list(df_init_data[i, ]))
  }
  result
}

get_periods_df <- function(df_init_data, df_fichas_row) {
  periods <- get_resources_period(df_init_data[grepl(df_fichas_row['code'], df_init_data[,'fichas'], fixed = T) & df_init_data['periodico'] == 1, ])
  time_period <- df_fichas_row$periodicidad
  if(time_period =='A') {
    periods_df <- data.frame(code = df_fichas_row$code, A = periods, Q = NA, M = NA)
  } else if(time_period == 'Q') {
    periods_df <- data.frame(code = df_fichas_row$code, A = periods, Q = NA, M = NA) %>%
      mutate(A = strsplit(sub(" ", ";", A), split = ';', )) %>% 
      unnest_wider('A') %>% 
      select(code, A = ...1, Q = ...2, M) %>%
      filter(Q %in% c("Marzo", "Junio", "Septiembre", "Diciembre", "Primer trimestre", "Segundo trimestre", "Tercer trimestre", "Cuarto trimestre"))
  } else if(time_period == 'M') {
    periods_df <- data.frame(code = df_fichas_row$code, A = periods, Q = NA, M = NA) %>%
      mutate(A = strsplit(A, split = ' ')) %>% 
      unnest_wider('A') %>% 
      select(code, A = ...1, Q, M = ...2)
  }
  periods_df
}

get_total_periods <- function(df_init_data, df_fichas) {
  total_periods <- data.frame()
  for(i in 1:nrow(df_fichas)) {
    total_periods <- bind_rows(total_periods, get_periods_df(df_init_data, df_fichas[i,]))
  }
  total_periods
}

loadMunicipios <- function() {
  file <- paste0(directory.data, df_init_data[df_init_data$name == 'cl_area',]$name, '.', df_init_data[df_init_data$name == 'cl_area',]$extension)
  CL_AREA <- as_list(read_xml(file))
  
  df_CL_AREA <- tibble::as_tibble(CL_AREA) %>% 
    unnest_wider('codes') %>%
    transform(name = lapply(name, '[[', 2)) %>% # x) { df_CL_AREA["name"][[c(1,x)]][[2]]}) %>% 
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

downloadData()

periods <- get_total_periods(df_init_data, df_fichas %>% filter(code == "paro_registrado")) %>% #%in% c("Elecciones", "presupuest"))) %>%
  left_join(
    data.frame(
      M = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
      id_M = 1:12
    ),
    by ='M') %>% 
  left_join(
    data.frame(
      Q = c("Marzo", "Junio", "Septiembre", "Diciembre", "Primer trimestre", "Segundo trimestre", "Tercer trimestre", "Cuarto trimestre"),
      id_Q = c(3, 6, 9, 12, 1, 2, 3, 4)
    ),
    by ='Q') %>%
  select (code, A, Q = id_Q, M = id_M)

municipios <- loadMunicipios()

generarFicha <- function(ano, id_ficha, periodicidad, mes, trimestre, id_municipio) {
  dir.fichero <- paste0("output/", id_ficha, "/", ano, "/")
  ficha_actual <- df_fichas[df_fichas$code == id_ficha,]
  nombre.fichero <- paste0(id_ficha, "_",
                           ifelse(periodicidad == "M", paste0(periodicidad, mes, "_"), ""),
                           ifelse(periodicidad == "Q", paste0(periodicidad, as.numeric(trimestre), "_"), ""),
                           id_municipio, ".html")
  if(!dir.exists(dir.fichero)) {
    dir.create(dir.fichero)
  }
  if(!file.exists(paste0(dir.fichero, nombre.fichero))) {
    option_params <- list(año = as.numeric(ano), id_municipio = as.numeric(id_municipio))
    if(periodicidad == "M") {
      option_params <- append(option_params, list(mes = as.numeric(mes)))
    }
    if(periodicidad == "Q") {
      #option_params <- append(option_params, list(mes = as.numeric(input$trimestre)*3))
      option_params <- append(option_params, list(mes = as.numeric(trimestre)))
    }
    
    option_params <- append(option_params, (df_init_data[grepl(id_ficha, df_init_data$fichas, fixed = TRUE),] %>% transform(filepath = paste0('../', filepath)) %>% select(param.name, filepath) %>% deframe %>% as.list))
   
    print(option_params)
    rmarkdown::render(
      paste0(directory.rmd, ficha_actual$filename), 
      output_dir = dir.fichero, 
      output_file = nombre.fichero, 
      params = option_params,
      output_options = list(
        lib_dir = paste0('../', dir.fichero, 'js'),
        css = paste0('../../resources/css/FICHA.css'))
    )
 
  }
}

generarFichas <- function(municipios, df_fichas, periods) {
  municipios_filtered <- municipios %>% select(id)
  for(municipio_index in 1:nrow(municipios_filtered)) {
    id_municipio <- municipios_filtered[municipio_index, 'id']
    fichas_filtered <- df_fichas %>% select(code)
    for(ficha_index in 1:nrow(fichas_filtered)) {
      id_ficha <- fichas_filtered[ficha_index, 'code']
      ficha_actual <- df_fichas %>% filter(code %in% id_ficha)
      periodicidad <- ficha_actual$periodicidad
      periods_filtered <- periods %>% filter(code %in% id_ficha)
      for(period_index in 1:nrow(periods_filtered)) {
        period <- periods_filtered[period_index,]
        skip_to_next <- FALSE
        tryCatch(
          {generarFicha(period$A, id_ficha, periodicidad, period$M, period$Q, id_municipio)},
          error = function(e) {
            print(paste('Error:', period$A, id_ficha, periodicidad, period$M, period$Q, id_municipio, sep = ', '))
            skip_to_next <<- TRUE
            },
          finally = {print(paste('Processed:', period$A, id_ficha, periodicidad, period$M, period$Q, id_municipio, sep = ', '))}
        )
        if(skip_to_next) { next }
      }
    }
  }
}

generarFicha(2021, 'alojamientos_turisticos', 'M', 5, NA, 35024)

generarFichas(municipios, df_fichas %>% filter(code == 'alojamientos_turisticos'), periods)

