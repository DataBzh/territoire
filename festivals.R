library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggmap)
festi <- read.csv2("http://datainfolocale.opendatasoft.com/explore/dataset/festivals-2016/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
bret <- festi %>% 
  filter(grepl(("22|29|35|56"), Département))
bretsecu <- bret
bret <- tidyr::separate(bret, latlong, into = c("lat", "lon"), sep = ",")
bret[49,16] <- "48.1119791219"
bret[49,17] <- "-1.68186449144"
bret$mois <- NA
bret[grep("octobre", bret$Plage), 21] <- "Octobre"
bret[grep("septembre", bret$Plage), 21] <- "Septembre"
bret[grep("août", bret$Plage), 21] <- "Août"
bret[grep("juillet", bret$Plage), 21] <- "Juillet"
bret[grep("juin", bret$Plage), 21] <- "Juin"
bret[grep("mai", bret$Plage), 21] <- "Mai"
bret[grep("avril", bret$Plage), 21] <- "Avril"
bret[grep("mars", bret$Plage), 21] <- "Mars"
bret[173, 21] <- "Août"
bret[grep("22", bret$Département), 3] <- "Côtes d'Armor"
bret[grep("29", bret$Département), 3] <- "Finistère"
bret[grep("35", bret$Département), 3] <- "Ille-et-Vilaine"
bret[grep("56", bret$Département), 3] <- "Morbihan"
bret$Plage <- gsub("du ", "",bret$Plage)
bret <- separate(bret, Plage, into = c("début", "fin"), sep = " au ")
bret$début <- paste(bret$début, " 2016")
bret$début <- dmy(bret$début)
bret[173, "début"] <- as.Date("2016-08-13")
bret[174, "début"] <- as.Date("2016-08-10")
bret$fin <- dmy(bret$fin)
bret$empty <- bret$fin - bret$début
bret$empty <- as.numeric(bret$empty) + 1
names(bret)[21] <- "durée"

palette <- c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01")

freq1 <- table(bret$Catégorie) %>%
  sort(., decreasing = TRUE) %>%
  head(., 15) %>%
  as.data.frame()
freq1$type <- rownames(freq1)
  
freq2 <- table(bret$Département) %>%
  sort(., decreasing = TRUE) %>%
  as.data.frame()
freq2$dep <- rownames(freq2)

freq3 <- table(bret$mois) %>%
  sort(., decreasing = TRUE) %>%
  as.data.frame()
freq3$type <- rownames(freq3)

gtheme <- theme(axis.text=element_text(size=10),
                axis.title=element_text(size=15),
                title=element_text(size=18),
                plot.title=element_text(margin=margin(0,0,20,0), size=18),
                axis.title.x=element_text(margin=margin(20,0,0,0)),
                axis.title.y=element_text(margin=margin(0,20,0,0)),
                legend.text=element_text(size = 12),
                plot.margin=margin(20,20,20,20), 
                panel.background = element_rect(fill = "white"), 
                panel.grid.major = element_line(colour = "grey")) 

ggplot(freq1, aes(x=reorder(type, .), y=.)) + 
  geom_bar(stat = "identity", fill = "#973232") + 
  coord_flip() + 
  ggtitle("Catégories les plus représentées") +
  xlab("") + 
  ylab("") +
  gtheme

ggplot(freq2, aes(x=dep, y=.)) + 
  geom_bar(stat = "identity", fill = "#973232") + 
  coord_flip() + 
  ggtitle("Festivals par département") +
  xlab("") + 
  ylab("") +
  gtheme

freq3$type <- factor(freq3$type, levels = c("Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre"))
ggplot(freq3, aes(x=type, y=.)) + 
  geom_bar(stat = "identity", fill = "#973232") + 
  ggtitle("Mois d'ouverture des festivals d'été") +
  xlab("") + 
  ylab("") +
  gtheme

ggplot(data = dplyr::arrange(bret, desc(durée))[1:25,], aes(x = reorder(Titre, durée), y = durée, fill = Département)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  scale_fill_manual(values = palette) + 
  xlab("") +
  ylab("") + 
  ggtitle("Les 25 festivals les plus longs")+ 
  gtheme

ggplot(bret, aes(x = durée)) +
  geom_bar(fill = "#973232") + 
  xlab("") +
  ylab("") + 
  ggtitle("Durée en jours des festivals")+ 
  gtheme

bret$mois <- factor(bret$mois, levels = c("Mars", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre"))
map <- ggmap::get_openstreetmap(bbox = c(left = -4.8, bottom = 47.3, right = -1, top = 48.9))
maptheme <-   theme(title=element_text(size=18),
                    plot.title=element_text(margin=margin(0,0,20,0), size=18),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    legend.text=element_text(size = 12),
                    plot.margin=margin(20,20,20,20), 
                    panel.background = element_rect(fill = "white"), 
                    panel.grid.major = element_line(colour = "grey")) 

ggmap(map)  +
  geom_point(data = bret, aes(x = as.numeric(lon), y = as.numeric(lat), col = mois), size = 4) +
  scale_color_manual(values = palette) + 
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  labs(color = "Mois") + 
  xlab("") +
  ylab("") +
  maptheme

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Mars"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[1], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Mars", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Mai"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[2], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Mai", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Juin"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[3], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Juin", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Juillet"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[4], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Juillet", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Août"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[5], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Aout", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Septembre"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[6], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Septembre", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme 

ggmap(map)  +
  geom_point(data = subset(bret, mois == "Octobre"), aes(x = as.numeric(lon), y = as.numeric(lat)), col = palette[8], size = 10) +
  ggtitle("Les festivals d'été en Bretagne, en fonction de leur mois d'ouverture") +
  annotate(geom = "text", x = -4.3, y = 47.6, label = "Octobre", size = 30, hjust = 0, family = "Roboto", alpha = 0.5) +
  xlab("") +
  ylab("") +
  maptheme 
