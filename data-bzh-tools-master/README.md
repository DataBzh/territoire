# DataBzhTools

Une collection d'outils (fonctions, constantes) facilitant la rédaction d'articles pour le site [Data-Bzh](http://data-bzh.fr).

## Utilisation

```r
source("DataBzhTools/main.R")

(ggp <- databzhPlot(
  ggplot(data = iris, aes(Petal.Length, Petal.Width, Species)) +
    geom_point(aes(colour = Species), size = 6, alpha = .5) +
    xlab("Petal Length") +
    ylab("Petal Width") +
    ggtitle("Iris") +
    databzhTheme(),
  ypos = "bottom"
))

databzhSavePlot(ggp, "demo.png")
```

![demo.R](medias/demo.png)

### Chargement des outils

```r
source("DataBzhTools/main.R")
```

### Fonctions

#### dataBzhPlot

Affichage d'un graphique **ggplot2** avec le logo Data-Bzh.

```r
databzhPlot(ggplot(...))
```

##### Arguments

| Argument | Description | Type / Valeurs |
|----|----|----|
| plot | Objet graphique ggplot2 | ggplot |
| size | Taille du logo à afficher | numeric |
| type | Type de logo | "url.transparent", "url.background", "simple.transparent", "simple.background" |
| xpos | Position horizontale du logo | "left", "right" |
| ypos | Position verticale du logo | "bottom", "top" |

Logos :

| Type | Logo |
|----|----|
| "url.transparent" | ![](medias/logo-data-bzh.url.transparent.png) |
| "url.background" | ![](medias/logo-data-bzh.url.background.png) |
| "simple.transparent" | ![](medias/logo-data-bzh.simple.transparent.png) |
| "simple.background" | ![](medias/logo-data-bzh.simple.background.png) |

#### databzhSavePlot

Sauvegarde d'un graphique.

```r
databzhSavePlot(plot(...), "image.png")
databzhSavePlot(databzhPlot(ggplot(...)), "image.png")
```

##### Arguments

| Argument | Description | Type / Valeurs |
|----|----|----|
| plot | Objet graphique | |
| filename | Nom du fichier à sauver | character |
| width | Largeur du graphique | integer |
| height | Hauteur du graphique | integer |

### Constantes

L'objet `databzh` contient de multiples constantes.

```r
databzh$colour
databzh$width
databzh$height
```

| Constante | Description | Valeur |
|----|----|----|
| colour | Couleur de thème principale | "#973232" |
| colour1 | Couleur de thème principale | "#973232" |
| colour2 | Couleur 2 | "#1E5B5B" |
| colour3 | Couleur 3 | "#6D8D2F" |
| colour4 | Couleur 4 | "#287928" |
| colour5 | Couleur 5 | "#E18C8C" |
| colour6 | Couleur 6 | "#548787" |
| colour7 | Couleur 7 | "#B8D283" |
| colour8 | Couleur 8 | "#70B470" |
| colours | Toutes les Couleurs | c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01") |
| width | Largeur de sauvegarde des images | 1200 |
| height | Hauteur de sauvegarde des images | 600 |
| logo | Logos utilisés par la fonction privée `.databzhLogo` | |
