library(tidyverse)
library(rgdal)
source("/home/colin/Dropbox/R/misc/data-bzh-tools-master/main.R")
rennes <- read_csv2("https://data.rennesmetropole.fr/explore/dataset/centres-de-vote/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
rennes <- rennes %>%
  separate(`Geo Point`, into = c("long","lat"), sep = ",")

table(rennes$c_nom) %>%
  sort()

sum(rennes$burx_nb)

rennes %>%
  group_by(c_nom) %>%
  summarise(somme = n()) %>%
  arrange(desc(somme)) %>%
ggplot(aes(x = reorder(c_nom, somme), y = somme))+ 
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  coord_flip() + 
  xlab("") +
  ylab(" ") +
  labs(title = "Centres de vote sur Rennes Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  

ggplot(rennes, aes(burx_nb)) + 
  geom_bar(fill = databzh$colour2) + 
  xlab("") +
  ylab(" ") +
  labs(title = "Bureaux de vote par centre sur Rennes Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()


roj <- readOGR(dsn=".", layer="emprise-de-rennes-metropole")
wmap_df <- fortify(roj)
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "#e4e4e4") + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group), color="grey50") + 
  geom_point(data=rennes, aes(as.numeric(lat), as.numeric(long), group = NULL), color = databzh$colours[2]) + 
  scale_size(range = c(1,12)) +
  xlab("") +
  ylab(" ") +
  labs(title = "Centres de vote sur Rennes Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  theme(title=element_text(),
        plot.title=element_text(margin=margin(0,0,20,0), size=18, hjust = 0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major= element_line("grey50", linetype = "dashed"), 
        panel.background= element_blank()) 
