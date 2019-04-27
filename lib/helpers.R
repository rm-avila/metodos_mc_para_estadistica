# Ajustar parámetros de Locale
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

# Función para reemplazar NA's por ceros en un vector ---------------------

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

# Función para cargar paquetes e instalar los que hagan falta -------------

cargar_paquetes <- function(...){
  paquetes <- c("extrafont", "tidyverse", "stringr", "foreign", "stringi",
                "RColorBrewer", "ggrepel", "lubridate", ...)
  if (length(setdiff(paquetes, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(paquetes, rownames(installed.packages())))  
  }
  lapply(paquetes, require, character.only = TRUE)
  return(search())
}

cargar_paquetes()


# Función para copiar tablas a portapapeles -------------------------------

# Notar que por default se escriben con separador por columna '|' (pipe)

copiar_tabla <- function(base, sep = ";" , row.names = F, ...){
  if(row.names == T){
    if(!is.data.frame(base)) base <- as.data.frame(base)
    base <- base %>% rownames_to_column()
    names(base)[1] <- "" 
    write.table(base, "clipboard-16384", sep = sep, row.names = F, ...)
  } else{
    write.table(base, "clipboard-16384", sep = sep, row.names = row.names, ...) 
  }
}

# Colores cbb -------------------------------------------------------------

# Paleta (52 colores) con gris

colores <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#00ccff",
             "#a07759", "#FFFF33", "#A65628", "#F0027F", "#BEAED4", "#00927F", 
             "#FDC086", "#7FC97F", "#D90A2D", "#1AEFC7", "#E75927", "#025a19")

# Para color de lìnea y/o puntos

col.ptos <- scale_color_manual(values = colores)

col.compare <- scale_color_manual(values = c("black", colores[1]))

# Para usar como color de relleno

col.fill <- scale_fill_manual(values = colores)


# Theme de ggplot ---------------------------------------------------------

theme_set(theme_bw())

# Creación de algunos directorios (si no existen)
suppressWarnings(dir.create("lib/"))
suppressWarnings(dir.create("src/"))
suppressWarnings(dir.create("data/"))
suppressWarnings(dir.create("plots/"))
suppressWarnings(dir.create("docs/"))


# Agregar transparencia a colores
add_alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
