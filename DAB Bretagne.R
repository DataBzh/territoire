library(ggplot2)
library(ggmap)

cotes <- read.csv("pack.master.cotes-darmor.money_atm.csv", header = TRUE, stringsAsFactors = FALSE,encoding = "UTF-8")
morb <- read.csv("pack.master.morbihan.money_atm.csv", header = TRUE, stringsAsFactors = FALSE,encoding = "UTF-8")
ille <- read.csv("pack.master.ille-et-vilaine.money_am.csv", head = TRUE, stringsAsFactors = FALSE,encoding = "UTF-8")
fin <- read.csv ("pack.master.finistere.money_atm.csv", head = TRUE, stringsAsFactors = FALSE,encoding = "UTF-8")

#Création d'une fonction qui : 
#prend un tableau df, un chiffre i et un chiffre j 
#boucle sur toutes les lignes du tableau
#verifie si la colonne en i est vide 
#si elle est vide, copie le contenu de la colone j
#si j est vide, copie le contenu de k 
#si k est également vide, créé un "Non Disponible" 

banque <- function(df, i, j, k){
  for(x in 1:nrow(df)){
    if (nchar(df[x,i]) == 0){
      if (nchar(df[x,j]) != 0) {
        df[x,i] <- df[x,j]
      } else if(nchar(df[x,k]) != 0) {
        df[x,i] <- df[x,k]
      } else {
        df[x,i] <- "Non Disponible" 
      }
    }
  }
  return(df)
}

fin <- banque(fin, 6,4,5)
ille <- banque(ille, 7, 4,5)
morb <- banque(morb,5,4,4)
cotes <- banque(cotes, 5, 4, 4)
cotes$dep <- "Cotes d'Armor"
fin$dep <- "Finistère"
ille$dep <- "Ille-et-Vilaine"
morb$dep <- "Morbihan"

cotes <- cotes[,c("lat","lng","tag__name","dep")]
fin <- fin[,c("lat","lng","tag__name","dep")]
morb <- morb[,c("lat","lng","tag__name","dep")]
ille <- ille[,c("lat","lng","tag__name","dep")]

bret <- rbind(cotes, fin, ille, morb)

sort(unique(bret$tag__name))
bret$tag__name <- tolower(bret$tag__name)
bret$tag__name <- gsub("caisse d'epargne", "caisse d'épargne", bret$tag__name)
bret$tag__name <- gsub("^banque postale$", "la banque postale", bret$tag__name)
bret$tag__name <- gsub("^la poste$", "la banque postale", bret$tag__name)
bret$tag__name <- gsub("caisse epargne", "caisse d'épargne", bret$tag__name)
bret$tag__name <- gsub("credit agricole", "crédit agricole", bret$tag__name)
bret$tag__name <- gsub("crédit agricoleisse d'epargne", "crédit agricoleisse d'épargne", bret$tag__name)
bret$tag__name <- gsub("crédit agricoleisse epargne", "crédit agricoleisse d'épargne", bret$tag__name)
bret$tag__name <- gsub("dab cmb", "crédit mutuel de bretagne", bret$tag__name)
bret$tag__name <- gsub("cmb", "crédit mutuel de bretagne", bret$tag__name)
bret$tag__name <- gsub("^crédit mutuel$", "crédit mutuel de bretagne", bret$tag__name)
bret$tag__name <- gsub("^ca$", "crédit agricole", bret$tag__name)
bret$tag__name <- gsub("banque populaire de l'ouest", "banque populaire", bret$tag__name)









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

#DAB par département

ggplot(bret, aes(x=dep)) +
  geom_bar(fill = "#973232") +
  xlab("Département") + 
  ylab("Volume de DAB") + 
  ggtheme

#DAB par banque 

freqbanque <- as.data.frame(table(bret$tag__name))

ggplot(freqbanque, aes(x=reorder(Var1,Freq), y = Freq)) +
  geom_bar(fill = "#973232", stat = "identity") + 
  coord_flip() +
  xlab("") + 
  ylab("Volume de DAB") + 
  ggtitle("DAB par Banque") +
  ggtheme

table(bret$tag__name)
table(bret)

map <- ggmap::get_openstreetmap(bbox = c(left = -4.8, bottom = 47.3, right = -1, top = 48.9))
ggmap(map) + 
  geom_point(data = bret, aes(x = lng, y = lat, col = dep), size = 3) + 
  scale_color_manual(values = palette) +
  xlab("") + 
  ylab("") +
  labs(col = "Département") + 
  ggtitle("Les DAB en Bretagne")

ggmap(map) + 
  geom_point(data = bret, aes(x = lng, y = lat, col = tag__name), size = 3) + 
  scale_color_manual(values = palette) +
  xlab("") + 
  ylab("") +
  labs(col = "Banque") + 
  ggtitle("Les DAB en Bretagne")
