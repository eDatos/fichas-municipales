library(dplyr)
library(xml2)
library(sf)
library(tibble)
library(tidyverse)
library(RCurl)

#Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")
directory.data <- "data/"
directory.rmd <- "Rmd/"
directory.output <- "output"
df_init_data <-  read.csv("shiny/init-data.csv", encoding = "UTF-8", sep = ";") %>% 
  transform(filepath = paste0(directory.data, name, '.', extension),
            param.name = paste0('url.', name))


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

df_fichas <- read.csv("shiny/fichas.csv", encoding = "UTF-8", sep = ";")
periods <- read.csv(paste0(directory.data,"periods.csv"), sep=",")

municipios <- loadMunicipios()

generarFicha <- function(ano, id_ficha, periodicidad, mes, trimestre, id_municipio) {
  dir.fichero <- paste0("./output/",ano, "/")
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
      output_options = list(lib_dir = paste0('../', dir.fichero, '/libs'))
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
        tryCatch(
          {generarFicha(period$A, id_ficha, periodicidad, period$M, period$Q, id_municipio)},
          error = {},
          finally = {print(paste('Processed:', period$A, id_ficha, periodicidad, period$M, period$Q, id_municipio, sep = ', '))}
        )
      }
    }
  }
}

downloadData()

generarFicha(2017, 'demografia', 'A', NA, NA, 35003)

#generarFichas(municipios, df_fichas %>% filter(code == 'paro'), periods %>% filter(A > 2003))

