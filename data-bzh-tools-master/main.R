databzh <- list()

# Setup ----
## Encodage pour l'environnement Mac OS
switch(
  Sys.info()[["sysname"]],
  Windows = {},
  Linux  = {},
  Darwin = {lc <- Sys.setlocale("LC_ALL", "en_US.UTF-8")}
)

## Chemin de ce fichier
.rootPath <- dirname(parent.frame(2)$ofile)

# Couleurs ----
databzh$colours <- c(
  "#973232", "#1E5B5B", "#6D8D2F", "#287928",
  "#E18C8C", "#548787", "#B8D283", "#70B470",
  "#B75353", "#326E6E", "#8CAA4E", "#439243",
  "#711515", "#0D4444", "#4D6914", "#115A11",
  "#490101", "#012C2C", "#2E4401", "#013A01"
)

# Raccourcis couleurs
databzh$colour <- databzh$colour1 <- databzh$colours[1]
databzh$colour2 <- databzh$colours[2]
databzh$colour3 <- databzh$colours[3]
databzh$colour4 <- databzh$colours[4]
databzh$colour5 <- databzh$colours[5]
databzh$colour6 <- databzh$colours[6]
databzh$colour7 <- databzh$colours[7]
databzh$colour8 <- databzh$colours[8]

# Dimensions ----
databzh$width = 1200
databzh$height = 600

# Logo ----
library(png)
library(grid)

databzh$logo <- list()
databzh$logo$simple <- list()
databzh$logo$simple$transparent <- rasterGrob(readPNG(paste0(.rootPath, "/medias/logo-data-bzh.simple.transparent.png")), interpolate = T)
databzh$logo$simple$background <- rasterGrob(readPNG(paste0(.rootPath, "/medias/logo-data-bzh.simple.background.png")), interpolate = T)
databzh$logo$url <- list()
databzh$logo$url$transparent <- rasterGrob(readPNG(paste0(.rootPath, "/medias/logo-data-bzh.url.transparent.png")), interpolate = T)
databzh$logo$url$background <- rasterGrob(readPNG(paste0(.rootPath, "/medias/logo-data-bzh.url.background.png")), interpolate = T)


# Thème ggplot2 ----
library(ggplot2)


#' Génération d'un thème Data-Bzh.
#'
#' @param axis.text
#' @param axis.title
#' @param title
#' @param plot.title
#' @param axis.title.x
#' @param axis.title.y
#' @param legend.text
#' @param plot.margin
#' @param panel.background
#' @param panel.grid.major
#' @param ...
#' @return thème ggplot2.
#' @export
#' @examples
#' databzhTheme()
databzhTheme <- function(axis.text = element_text(size = 10),
                         axis.title = element_text(size = 15),
                         title = element_text(size = 18),
                         plot.title = element_text(margin = margin(0, 0, 20, 0), size = 18, hjust = .5),
                         axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                         axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
                         legend.text = element_text(size = 12),
                         plot.margin = margin(20, 20, 20, 20), 
                         panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(colour = "grey"),
                         ...) {
  th <- theme(
    axis.text = axis.text,
    axis.title = axis.title,
    title = title,
    plot.title = plot.title,
    axis.title.x = axis.title.x,
    axis.title.y = axis.title.y,
    legend.text = legend.text,
    plot.margin = plot.margin, 
    panel.background = panel.background, 
    panel.grid.major = panel.grid.major,
    ...
  )

  return(th)
}


#' Génération d'un logo Data-Bzh.
#'
#' @param xval valeurs affichées sur la des absisses (x)
#' @param yval valeurs affichées sur la des ordonnées (y)
#' @param size taille du logo à afficher
#' @param type type de logo (url.transparent, url.background, simple.transparent, simple.background)
#' @param xpos position horizontale du logo (left, right)
#' @param ypos position verticale du logo (bottom, top)
#' @return objet graphique ggplot2 représentant le logo Data-Bzh.
#' @export
#' @examples
#' logo <- .databzhLogo(...)
.databzhLogo <- function(xval = NULL,
                         yval = NULL,
                         size = 1.5,
                         type = "url.transparent",
                         xpos = "right",
                         ypos = "top") {
  ratio <- size / 10
  
  getMinMax <- function(val = NULL, side = "top.right") {
    if (is.null(val)) {
      return(c(-Inf, Inf))
    }
    
    lim <- range(val)
    
    if (side == "top.right") {
      width <- lim[2] - lim[1]
      return(c(lim[2] - width * ratio, lim[2]))
    } else if (side == "bottom.left") {
      width <- lim[2] - lim[1]
      return(c(lim[1], lim[1] + width * ratio)) 
    }
  }
  
  x <- getMinMax(xval, (if (xpos == "left") "bottom.left" else "top.right"))
  y <- getMinMax(yval, (if (ypos == "bottom") "bottom.left" else "top.right"))
  
  return(
    annotation_custom(
      switch(
        type,
        simple.transparent = databzh$logo$simple$transparent,
        simple.background = databzh$logo$simple$background,
        url.transparent = databzh$logo$url$transparent,
        url.background = databzh$logo$url$background
      ),
      xmin = x[1], xmax = x[2],
      ymin = y[1], ymax = y[2]
    )
  )
}


#' Génération d'un logo Data-Bzh.
#'
#' @param plot objet graphique ggplot2 sur lequel le logo sera affiché
#' @param size taille du logo à afficher
#' @param type type de logo (url.transparent, url.background, simple.transparent, simple.background)
#' @param xpos position horizontale du logo (left, right)
#' @param ypos position verticale du logo (bottom, top)
#' @return objet graphique ggplot2 représentant le logo Data-Bzh.
#' @export
#' @examples
#' plot <- ggplot(...)
#' plot + databzhLogo(plot)
databzhLogo <- function(plot,
                        size = 1.5,
                        type = "url.transparent",
                        xpos = "right",
                        ypos = "top") {
  stopifnot("ggplot" %in% class(plot))

  # Requires ggplot2 version 2.1.0.9001 or higher
  ggb <- ggplot_build(plot)
  
  if (!is.null(ggb$panel)) {
    xlim <- ggb$panel$ranges[[1]]$x.range
    ylim <- ggb$panel$ranges[[1]]$y.range
  } else if (!is.null(ggb$layout)) {
    xlim <- ggb$layout$panel_ranges[[1]]$x.range
    ylim <- ggb$layout$panel_ranges[[1]]$y.range
  } else {
    xlim <- NULL
    ylim <- NULL
  }
  
  return(.databzhLogo(xlim, ylim, size, type, xpos, ypos))
}


#' Affichage d'un graphique ggplot2 avec le logo Data-Bzh.
#'
#' @param plot objet graphique ggplot2
#' @param size taille du logo à afficher
#' @param type type de logo (url.transparent, url.background, simple.transparent, simple.background)
#' @param xpos position horizontale du logo (left, right)
#' @param ypos position verticale du logo (bottom, top)
#' @return objet graphique ggplot2 avec le logo Data-Bzh.
#' @export
#' @examples
#' databzhPlot(ggplot(...))
databzhPlot <- function(plot,
                        size = 1.5,
                        type = "url.transparent",
                        xpos = "right",
                        ypos = "top") {
  stopifnot("ggplot" %in% class(plot))
  
  plotWithLogo <- plot + databzhLogo(plot, size, type, xpos, ypos)
  
  return(plotWithLogo)
}

# Sauvegarde ----
library(grDevices)
library(graphics)


#' Sauvegarde d'un graphique.
#'
#' @param plot objet graphique
#' @param filename nom du fichier à sauver
#' @param width largeur du graphique
#' @param height hauteur du graphique
#' @export
#' @examples
#' databzhSavePlot(ggplot(...), "image.png")
databzhSavePlot <- function(plot,
                            filename,
                            width = databzh$width,
                            height = databzh$height) {
  png(filename, width, height)
  graphics::plot(plot)
  dev.off()
  
  invisible()
}
