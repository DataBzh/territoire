library(ggplot2)
library(ggmap)
library(magrittr)

palette <- c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01")

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

#Carte 1 
cotes <- read.csv("pack.master.cotes-darmor.pitchs.csv")
fin <- read.csv("pack.master.finistere.pitchs.csv")[,-6]
ill <- read.csv("pack.master.ille-et-vilaine.pitchs.csv")
mor <- read.csv("pack.master.morbihan.pitchs.csv")
cotes$dep <- "CÃ´tes d'Armor"
fin$dep <- "FinistÃ¨re"
ill$dep <- "Ille-et-Vilaine"
mor$dep <- "Morbihan"
bret <- rbind(cotes, fin, ill, mor)

map <- ggmap::get_openstreetmap(bbox = c(left = -4.8, bottom = 47.3, right = -1, top = 48.9))
ggmap(map) + 
  geom_point(data = bret, aes(x = lng, y = lat, col = dep), size = 1.5) + 
  scale_color_manual(values = palette) +
  xlab("") + 
  ylab("") +
  labs(col = "DÃ©partement") + 
  ggtitle("Les Ã©quipement sportifs en Bretagne") + 
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Barplot  

ggplot(bret, aes(x=dep)) +
  geom_bar(fill = "#973232") +
  xlab("DÃ©partement") + 
  ylab("Volume d'Ã©quipement") + 
  ggtitle("Ãquipements sportif par dÃ©partement") + 
  ggtheme

#Par habitant 
parhab <- data.frame(dep = c("22","29","35","56","22","29","35","56"), jdd = c("1","1","1","1","2","2","2","2"), parhab = c(732,787,759,727,330,727,428,372))
ggplot(parhab, aes(x=dep, y = parhab, fill = jdd)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = palette) +
  xlab("DÃ©partement") + 
  ylab(" ") + 
  labs(fill = "Jeu de donnÃ©es") +
  ggtitle("Nombre d'habitant pour un Ã©quipement") + 
  ggtheme

#Par an 
installation <- read.csv2("20160713_RES_FichesInstallations.csv", sep = ";", header = TRUE, encoding = "latin1") %>%
  dplyr::filter(grepl(("22|29|35|56"), DepCode))
installation <- installation[ ,c(1,12,30,31)]
installation$InsDateCreation <- lubridate::ymd_hms(installation$InsDateCreation)
installation$InsDateCreation <- format(as.Date(installation$InsDateCreation), "%Y")
ggplot(installation, aes(x = InsDateCreation, fill = DepCode)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = palette) + 
  xlab(" ") + 
  ylab(" ") + 
  ggtitle("Nouvelle installation par an") + 
  labs(fill = "DÃ©partement") + 
  ggtheme
