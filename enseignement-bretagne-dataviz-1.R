library(magrittr)
library(ggplot2)
library(dplyr)
library(rgeoapi)

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

palette <- c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01")

#Primaire et secondaire
empl <- read.csv2("https://www.data.gouv.fr/s/resources/adresse-et-geolocalisation-des-etablissements-denseignement-du-premier-et-second-degres/20160526-143453/DEPP-etab-1D2D.csv", stringsAsFactors = FALSE, encoding = "latin1")
bret <- empl %>% 
  filter(grepl(("22...|29...|35...|56..."), code_postal_uai))
nature <- sort(table(bret$nature_uai_libe), decreasing = TRUE) %>%
  as.data.frame() 
nature[,2] <- rownames(nature)
names(nature) <- c("Fréquence", "Type")

ggplot(nature, aes(x=reorder(Type, Fréquence), y=Fréquence)) + 
  geom_bar(stat = "identity", fill = "#973232") + 
  coord_flip() + 
  ggtitle("Type d'établissements d'enseignement du premier et second degrés") +
  xlab("") + 
  ylab("") +
  gtheme

bret$dep <- NA
bret$dep[grep("22...", bret$code_postal_uai)] <- "Côtes d'Armor"
bret$dep[grep("29...", bret$code_postal_uai)] <- "Finistère"
bret$dep[grep("35...", bret$code_postal_uai)] <- "Ille-et-Vilaine"
bret$dep[grep("56...", bret$code_postal_uai)] <- "Morbihan"
bret$nature_uai_libe[grep("Lycée", bret$nature_uai_libe)] <- "Lycée"
bret$nature_uai_libe[grep("élémentaire", bret$nature_uai_libe)] <- "Ecole élémentaire"
bret$nature_uai_libe[grep("maternelle", bret$nature_uai_libe)] <- "Ecole maternelle"
bret$nature_uai_libe[grep("Section", bret$nature_uai_libe)] <- "Section d'enseignement"

ggplot(bret, aes(x = nature_uai_libe, fill = dep)) +
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = palette) + 
  coord_flip() + 
  xlab("") + 
  ylab("") +
  ggtitle("Type d'établissement par département") +
  labs(fill = "Département") + 
  gtheme

bret$Enseignement <- NA
bret$Enseignement[grep("Lycée", bret$nature_uai_libe)] <- "Secondaire"
bret$Enseignement[grep("Collège", bret$nature_uai_libe)] <- "Secondaire"
bret$Enseignement[grep("élémentaire", bret$nature_uai_libe)] <- "Primaire"
bret$Enseignement[grep("maternelle", bret$nature_uai_libe)] <- "Primaire"
bret$Enseignement[grep("Section", bret$nature_uai_libe)] <- "Spécifique"
bret$Enseignement[grep("adapté", bret$nature_uai_libe)] <- "Spécifique"

ggplot(bret, aes(x = dep, fill = Enseignement)) +
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = palette) + 
  coord_flip() + 
  xlab("") + 
  ylab("") +
  ggtitle("Enseignement en fonction du département") +
  labs(fill = "Enseignement") + 
  gtheme

ggplot(bret, aes(x = dep, fill = secteur_public_prive_libe)) +
  geom_bar(position = "fill") + 
  scale_fill_manual(values = palette) + 
  coord_flip() + 
  xlab("") + 
  ylab("") +
  ggtitle("Établissements privés et publics") +
  labs(fill = " ") + 
  gtheme

ggplot(bret, aes(x = nature_uai_libe, fill = secteur_public_prive_libe)) +
  geom_bar(position = "fill") + 
  scale_fill_manual(values = palette) + 
  coord_flip() + 
  xlab("") + 
  ylab("") +
  ggtitle("Établissements privés et publics par enseignement") +
  labs(fill = " ") + 
  gtheme

#Supérieur ====
sup <- read.csv2("http://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-principaux-etablissements-enseignement-superieur/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
bretsup <- filter(sup, Région == "Bretagne")

ggplot(bretsup, aes(x = Département)) + 
  geom_bar(fill = "#973232") + 
  coord_flip() + 
  xlab("") + 
  ylab(" ") +
  ggtitle("Établissements d'enseignement supérieur") + 
  gtheme

ggplot(bretsup, aes(x = Commune, fill = Secteur.d.établissement)) + 
  geom_bar() +
  scale_fill_manual(values = palette) +
  coord_flip() + 
  xlab("") + 
  ylab(" ") +
  labs(fill = "Secteur") +
  ggtitle("Établissements d'enseignement supérieur par ville") + 
  gtheme

ggplot(bretsup, aes(x = type.d.établissement, fill = Département)) + 
  geom_bar() +
  scale_fill_manual(values = palette) +
  coord_flip() + 
  xlab("") + 
  ylab(" ") +
  labs(fill = "Secteur") +
  ggtitle("Établissements d'enseignement supérieur par ville") + 
  gtheme

bretsup <- tidyr::separate(bretsup, Géolocalisation, into = c("Y", "X"), sep = ",")
map <- ggmap::qmap('Bretagne', zoom = 7, maptype = "roadmap")
map + 
  geom_point(data = bretsup, aes(x = as.numeric(X), y = as.numeric(Y), color = Secteur.d.établissement), size = 4) +
  scale_color_manual(values = palette) + 
  ggtitle("Établissements d'enseignement supérieur") +
  xlim(-4.8, -1) + 
  ylim(47.3,48.8) +
  labs(color = "Secteur") + 
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey")) 

brest <- ggmap::qmap('Brest', zoom = 11, maptype = "roadmap") + 
  geom_point(data = bretsup, aes(x = as.numeric(X), y = as.numeric(Y), color = Secteur.d.établissement), size = 4) +
  scale_color_manual(values = palette) + 
  ggtitle("Brest") +
  labs(color = "Secteur") + 
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey")) 

lorient <- ggmap::qmap('Lorient', zoom = 11, maptype = "roadmap")+ 
  geom_point(data = bretsup, aes(x = as.numeric(X), y = as.numeric(Y), color = Secteur.d.établissement), size = 4) +
  scale_color_manual(values = palette) + 
  ggtitle("Lorient") +
  labs(color = "Secteur") + 
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey")) 

rennes <- ggmap::qmap('Rennes', zoom = 11, maptype = "roadmap") +
  geom_point(data = bretsup, aes(x = as.numeric(X), y = as.numeric(Y), color = Secteur.d.établissement), size = 4) +
  scale_color_manual(values = palette) + 
  ggtitle("Rennes") +
  labs(color = "Secteur") + 
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey")) 
