library(ggplot2)
library(ggmap)
library(data.table)
dechet <- read.csv("https://data.rennesmetropole.fr/explore/dataset/matieres-collectees-en-decheteries-et-plateformes-de-vegetaux-a-rennes-metropole/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true", header = TRUE, sep = ";",encoding = "UTF-8")
ggtheme <- gtheme <- theme(axis.text=element_text(size=10),
                           axis.title=element_text(size=15),
                           title=element_text(size=18),
                           plot.title=element_text(margin=margin(0,0,20,0), size=18),
                           axis.title.x=element_text(margin=margin(20,0,0,0)),
                           axis.title.y=element_text(margin=margin(0,20,0,0)),
                           legend.text=element_text(size = 12),
                           plot.margin=margin(20,20,20,20), 
                           panel.background = element_rect(fill = "white"), 
                           panel.grid.major = element_line(colour = "grey"))
palette <- c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01")

#bennes par an
ggplot(dechet, aes(x=Année)) +
  geom_bar(fill = "#973232") +
  xlab("Année") + 
  ylab("Bennes de collecte") + 
  ggtitle("Nombre de bennes de collecte par année") +
  ggtheme

#tonnage par an
ggplot(dechet, aes(x= Année, y = Tonnage))+
  geom_bar(fill = "#973232", stat = "identity") +
  xlab("Année") + 
  ylab("Tonnage") + 
  ggtitle("Tonnage de dechets par année") +
  ggtheme

#Points de collecte
ggplot(dechet, aes(x= Année, y = Tonnage, fill = Equipement))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = palette) +
  xlab("Année") + 
  ylab("Tonnage") + 
  ggtitle("Tonnage par année et par type d'équipement") +
  ggtheme

#Matières collectées totale
dechetdt <- as.data.table(dechet)
dechetdt <- dechetdt[,.(Tonnage = sum(Tonnage)),by=Matière]
ggplot(dechetdt, aes(x= reorder(Matière, Tonnage), y = Tonnage))+
  geom_bar(fill = "#973232", stat = "identity") +
  coord_flip() +
  xlab(" ") + 
  ylab("Tonnage") + 
  ggtitle("Tonnage par matière") +
  ggtheme

#Points de collectes
dechetdt2 <- as.data.table(dechet)
dechetdt2 <- dechetdt2[,.(Tonnage = sum(Tonnage)),by=Site]
ggplot(dechetdt2, aes(x= reorder(Site, Tonnage), y = Tonnage))+
  geom_bar(fill = "#973232", stat = "identity") +
  coord_flip() +
  xlab(" ") + 
  ylab("Tonnage") + 
  ggtitle("Tonnage par point de collecte") +
  ggtheme
