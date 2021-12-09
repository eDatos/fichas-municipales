data_municipales <- fromJSON("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00010A_000032/~latest.json?dim=TERRITORIO:35004|35010|35018|35024|35028|35029|35034|35003|35007|35014|35015|35017|35030|35001|35002|35005|35006|35008|35009|35011|35012|35013|35016|35019|35020|35021|35022|35023|35025|35026|35027|35031|35032|35033|38001|38004|38005|38006|38010|38011|38012|38015|38017|38018|38019|38020|38022|38023|38025|38026|38028|38031|38032|38034|38035|38038|38039|38040|38041|38042|38043|38044|38046|38051|38052|38002|38003|38021|38036|38049|38050|38007|38008|38009|38014|38016|38024|38027|38029|38030|38033|38037|38045|38047|38053|38013_2007|38048|38901&lang=es")
convocatoria <- 2019
dR <- length(rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[1]][["code"]],
                 each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]]) *
                   length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]]))) -
  length(unlist(strsplit(data_municipales[["data"]][["observations"]], split = " | ", fixed = TRUE)))

if(convocatoria == 2015){
  df_municipales <- data.frame(
    candidatura = rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[1]][["code"]],
                      each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]]) *
                        length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]])),
    mun = rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]],
              each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]])),
    medida = data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]],
    valor = c(unlist(strsplit(data_municipales[["data"]][["observations"]], split = " | ", fixed = TRUE)), rep(NA, dR)))
  
  df_partido <- data.frame(
    candidatura = data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["value"]][[1]][["id"]],
    name_partido = vector(mode = "character", length = data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["total"]][1]))
  for(i in 1:data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["total"]][1]) {
    df_partido$name_partido[i] <- data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["value"]][[1]][["name"]][["text"]][[i]][["value"]]
  }}else{
    if(convocatoria == 2011){
      df_municipales <- data.frame(
        mun = rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[1]][["code"]],
                  each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]]) *
                    length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]])),
        medida = rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]],
                     each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]])),
        candidatura = data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]],
        valor = c(unlist(strsplit(data_municipales[["data"]][["observations"]], split = " | ", fixed = TRUE)), rep(NA, dR)))
      
      df_partido <- data.frame(
        candidatura = data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["value"]][[3]][["id"]],
        name_partido = vector(mode = "character", length = data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["total"]][3]))
      for(i in 1:data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["total"]][3]) {
        df_partido$name_partido[i] <- data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["value"]][[3]][["name"]][["text"]][[i]][["value"]]
      }}else{
        if(convocatoria %in% c(2019, 2007)){
          df_municipales <- data.frame(
            mun = rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[1]][["code"]],
                      each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]]) *
                        length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]])),
            candidatura = rep(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[2]][["code"]],
                              each = length(data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]])),
            medida = data_municipales[["data"]][["dimensions"]][["dimension"]][["representations"]][["representation"]][[3]][["code"]],
            valor = c(unlist(strsplit(data_municipales[["data"]][["observations"]], split = " | ", fixed = TRUE)), rep(NA, dR)))
          
          df_partido <- data.frame(
            candidatura = data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["value"]][[2]][["id"]],
            name_partido = vector(mode = "character", length = data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["total"]][2]))
          for(i in 1:data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["total"]][2]) {
            df_partido$name_partido[i] <- data_municipales[["metadata"]][["dimensions"]][["dimension"]][["dimensionValues"]][["value"]][[2]][["name"]][["text"]][[i]][["value"]]
          }}
      }
  }

df_municipales$valor <- as.numeric(df_municipales$valor)
df_municipales <- df_municipales %>% transform(mun = if_else(mun == "38013_2007",  "38013", mun))

df_municipales_mun <- df_municipales %>% 
  filter(mun == 38038)

df_municipales_mun_ <- df_municipales_mun %>% 
  filter(substring(candidatura, 0,1) == "P") %>% arrange(valor) %>% select(medida, candidatura, valor)
df_plot_ <- df_municipales_mun_ %>%
  spread(medida, valor) %>% filter(!is.na(VOTOS_VALIDOS_CANDIDATURA)) %>%
  select(candidatura, valor = VOTOS_VALIDOS_CANDIDATURA, porcentaje = RATIO_VOTOS_CANDIDATURA, repr_elegidos = REPRESENTANTES_ELEGIDOS)

df_plot_$repr_elegidos <- as.numeric(df_plot_$repr_elegidos)
df_representantes_ <- df_plot_ %>% 
  filter(repr_elegidos > 0) %>% 
  arrange(-valor) %>% 
  select(candidatura, repr_elegidos)
df_representantes_$candidatura <- ordered(df_representantes_$candidatura, levels = df_representantes_$candidatura[nrow(df_representantes_):1])
df_representantes_ <- df_representantes_ %>% left_join(df_partido, by = "candidatura") 

df_representantes <- list()
df_points <- data.frame()
for(i in 1:nrow(df_representantes_)) {
  df_representantes[[i]] <- data.frame(candidatura = rep(df_representantes_$candidatura[i], times = (df_representantes_$repr_elegidos[i])),
                                       puntos = c(1:df_representantes_$repr_elegidos[i]))
  df_points <- df_points %>% rbind(df_representantes[[i]])
}

df_points <- df_points %>% left_join(df_partido, by = "candidatura") 
df_points$name_partido <- ordered(df_points$name_partido, levels = df_representantes_$name_partido[nrow(df_representantes_):1])

df_points <- df_points %>% transform(hovertext = paste0(
  "<b>", name_partido, " </b>",
  "<br>", trimws(format(puntos, big.mark = ".", decimal.mark = ",")), " representantes elegidos"
)
)

get_nombre_partido <- function(nombre) {
  string_list <- str_split(nombre, pattern = " ")[[1]]
  result <- ""
  line.char <- 0
  for(word.index in 1:length(string_list)) {
    if((line.char + nchar(string_list[word.index])) < 38) {
      result <- paste0(result, " ", string_list[word.index])
      line.char <- line.char + 1 + nchar(string_list[word.index])
    } else {
      result <- paste0(result, "<br>", string_list[word.index])
      line.char <- nchar(string_list[word.index])
    }
  }
  trimws(result)
}

df_representantes_$name_partido_percent <- ""

for(i in 1:nrow(df_representantes_)) {
  df_representantes_$name_partido_percent[i] <- get_nombre_partido(df_representantes_$name_partido[i])
}

df_points$hovertext[which(is.na(df_points$name_partido))] <- ""




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Key Lime
#'
#' @param data,params,size key stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_lime <- function(data, params, size) {
  
  im <- png::readPNG(data$image_filename)
  
  aspect <- dim(im)[1]/dim(im)[2]
  
  grid::rasterGrob(
    image  = im,
    width  = ggplot2::unit(data$size / size         , 'snpc'),
    height = ggplot2::unit(data$size / size * aspect, 'snpc')
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Draw limes
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes see
#'        documentation for \code{ggplot2::geom_point()}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_lime <- function(mapping     = NULL,
                      data        = NULL,
                      stat        = "identity",
                      position    = "identity",
                      ...,
                      na.rm       = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomLime,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm    = na.rm,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomLime
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomLime <- ggplot2::ggproto(
  "GeomLime", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size"),
  default_aes = ggplot2::aes(
    shape  = 19,
    colour = "black",
    size   = 1.5,
    fill   = NA,
    alpha  = NA,
    stroke = 0.5,
    image_filename  = system.file("./img/family.png", mustWork = TRUE),
    scale = 5
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    
    coords <- coord$transform(data, panel_params)
    
    im <- png::readPNG(coords$image_filename)
    aspect <- dim(im)[1]/dim(im)[2]
    
    grid::rasterGrob(
      image  = im,
      x      = coords$x,
      y      = coords$y,
      width  = ggplot2::unit(coords$size * coords$scale         , 'pt'),
      height = ggplot2::unit(coords$size * coords$scale * aspect, 'pt')
    )
    
  },
  
  draw_key = draw_key_lime
)

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
install.packages("ggimage")
library(ggimage)

library(magick)
library(grid)

image <- image_read('./img/family.png')

df_points <- df_points %>% mutate(image = "./img/family.png")

g_representantes <- ggplot(data = df_points,
                           aes(x = puntos, y = name_partido, name = NULL, text = hovertext)) +
  geom_image(stat = "identity", colour = "#008BD0", size = 0.06, alpha = 0.8, aes(image = "./img/family.png")) +
  theme_minimal() +
  guides(fill = guide_legend(title = " ")) +
  labs(title = "", x = NULL, y = NULL, colour = " ") +
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(size = 8),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none") +
  scale_x_continuous(n.breaks = 5, labels = function(x){format(x, big.mark = '.', decimal.mark = ",")}, limits = c(0.3, NA)) +
  scale_y_discrete(labels = df_representantes_$name_partido_percent[nrow(df_representantes_):1])

g_representantes

g_representantes <- ggplotly(g_representantes, tooltip = c("text")) %>%
  layout(hovermode = "y unified", showlegend = FALSE, margin = list(l = -6)) %>%
  config(displaylogo = FALSE, modeBarButtons = list(list("zoom2d"),list("pan2d"),list("resetScale2d"),list("toImage")))

ggplot(data = df_points,
       aes(x = puntos, y = name_partido, name = NULL, text = hovertext)) +
  geom_image(stat = "identity", colour = "#008BD0", size = 0.06, alpha = 0.8, aes(image = image)) +
  theme_minimal() +
  guides(fill = guide_legend(title = " ")) +
  labs(title = "", x = NULL, y = NULL, colour = " ") +
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(size = 8),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none") +
  scale_x_continuous(n.breaks = 5, labels = function(x){format(x, big.mark = '.', decimal.mark = ",")}, limits = c(0.3, NA)) +
  scale_y_discrete(labels = df_representantes_$name_partido_percent[nrow(df_representantes_):1])

grid.raster(image, unit(0.95, "npc"), y = unit(0.95, "npc"))
