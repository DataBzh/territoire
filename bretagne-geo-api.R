#Get the data

library(rgeoapi)
bret <- ComByReg(53) 

length(unique(bret$codeInsee)) 
sum(bret$population)
summary(bret)
sd(bret$population)

#Map the data

library(ggplot2)
library(dplyr)
library(ggmap)

palette <- c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01")

#Les habitants de Bretagne

ggplot(bret, aes(population)) + 
  geom_histogram(bins = 50, fill = "#973232") + 
  geom_text(data = arrange(bret, desc(population))[1:3,], aes(label= name), check_overlap = TRUE) +
  xlab("Population") + 
  ylab("Compte") + 
  ggtitle("Population des villes bretonnes") +
  theme(legend.position="none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20))

#La population est-elle influencée par la surface ?

ggplot(bret, aes(x=population, y=surface, color = codeDepartement, alpha = 0.5)) + 
  geom_point() + 
  geom_text(data = arrange(bret, desc(population))[1:3,], aes(label= name), check_overlap = TRUE, nudge_y = -300) +
  geom_text(data = arrange(bret, desc(surface))[1:3,], aes(label= name), check_overlap = TRUE, nudge_y = -300) + 
  scale_colour_manual(values = palette, name = "Département") +
  xlab("Population") + 
  ylab("Surface") + 
  ggtitle("Surface et population des villes bretonnes") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20))

#Les 10 villes les plus peuplées

qmap('Bretagne', zoom = 8, maptype = "terrain", color = "bw") + 
  geom_point(data = arrange(bret, desc(population))[1:10,], aes(x = long, y = lat, size = population, color = codeDepartement)) + 
  scale_colour_manual(values = palette, name = "Département") +
  ggtitle("Les 10 villes les plus peuplées de Bretagne") +
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        plot.margin=margin(20,20,20,20))

ggplot() + 
  geom_bar(data = arrange(bret, desc(population))[1:10,], aes(x = reorder(name, -population), y = population, fill = codeDepartement), stat = "identity") + 
  scale_fill_manual(values = palette, name = "Département") +
  xlab("Ville") + 
  ylab("Population") + 
  ggtitle("Les 10 villes les plus peuplées de Bretagne") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey"))

#Les 10 villes les plus grandes de Bretagne

qmap('Bretagne', zoom = 8, maptype = "terrain", color = "bw") + 
  geom_point(data = arrange(bret, desc(surface))[1:10,], aes(x = long, y = lat, size = surface, color = codeDepartement)) + 
  scale_colour_manual(values = palette, name = "Département") +
  ggtitle("Les 10 villes les plus grandes de Bretagne") +
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        plot.margin=margin(20,20,20,20))

ggplot() + 
  geom_bar(data = arrange(bret, desc(surface))[1:10,], aes(x = reorder(name, -surface), y = surface, fill = codeDepartement), stat = "identity") + 
  scale_fill_manual(values = palette, name = "Département") +
  xlab("Ville") + 
  ylab("Surface") + 
  ggtitle("Les 10 villes les plus grandes de Bretagne") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey"))

#Densité de population en Bretagne

bret$densite <- bret$population / (bret$surface * 0.01)

ggplot(bret, aes(densite)) + 
  geom_histogram(bins = 150, fill = "#973232") + 
  xlab("Densité de population") + 
  ylab("Compte") + 
  ggtitle("Villes bretonnes selon leur densité au km2") +
  theme(legend.position="none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20))

qmap('Bretagne', zoom = 8, maptype = "terrain", color = "bw") + 
  geom_point(data = arrange(bret, desc(densite))[1:10,], aes(x = long, y = lat, size = densite, color = codeDepartement)) + 
  scale_colour_manual(values = palette, name = "Département") +
  ggtitle("Les 10 villes les plus denses de Bretagne") +
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        plot.margin=margin(20,20,20,20))

ggplot() + 
  geom_bar(data = arrange(bret, desc(densite))[1:10,], aes(x = reorder(name, -densite), y = densite, fill = codeDepartement), stat = "identity") + 
  scale_fill_manual(values = palette, name = "Département") +
  xlab("Ville") + 
  ylab("Habitants / km2") + 
  ggtitle("Les 10 villes les plus denses de Bretagne") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        legend.text=element_text(size = 12),
        plot.margin=margin(20,20,20,20), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey"))

#En résumé : les villes remarquables de Bretagne

remarq <- rbind(arrange(bret, desc(surface))[1:10,],arrange(bret, desc(population))[1:10,], arrange(bret, desc(densite))[1:10,])
remarq$Remarquable[1:10] <- "Surface" 
remarq$Remarquable[11:20] <- "Population" 
remarq$Remarquable[21:30] <- "Densité" 

remarq <- arrange(remarq, name)
remarq[1:2,10] <- "Population/Densité"
remarq[5:6,10] <- "Population/Densité"
remarq[15:16,10] <- "Population/Densité"
remarq[22:23,10] <- "Surface/Population"
remarq[24:25,10] <- "Population/Densité"
remarq[26:27,10] <- "Population/Densité"
remarq <- unique(remarq)

qmap('Bretagne', zoom = 8, maptype = "terrain", color = "bw") + 
  geom_point(data = remarq, aes(x = long, y = lat, color = Remarquable, position = "jitter", size = 3)) + 
  scale_colour_manual(values = palette, name = "Remarquable pour") +
  scale_size_continuous(guide=FALSE) +
  ggtitle("Villes remarquables en Bretagne") +
  theme(title=element_text(size=18),
        plot.title=element_text(margin=margin(0,0,20,0), size=18),
        plot.margin=margin(20,20,20,20))
