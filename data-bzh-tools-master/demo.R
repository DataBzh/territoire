source("../DataBzhTools/main.R")

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
