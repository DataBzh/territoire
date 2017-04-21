library(tidyverse)
library(rgdal)
source("/home/colin/Dropbox/R/misc/data-bzh-tools-master/main.R")
saintma <- read_csv2("https://data.stmalo-agglomeration.fr/explore/dataset/emplacement-des-bureaux-de-vote-saint-malo/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
saintma <- saintma %>%
  separate(`Geo Point`, into = c("long","lat"), sep = ",")

table(saintma$CANTON) %>%
  sort()


roj <- readOGR(dsn=".", layer="quartiers-de-saint-malo")
wmap_df <- fortify(roj)
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "antiquewhite") + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group), color="grey50") + 
  geom_point(data=saintma, aes(as.numeric(lat), as.numeric(long), group = NULL), color = databzh$colour3, size = 3) + 
  scale_size(range = c(1,12)) +
  xlab("") +
  ylab(" ") +
  labs(title = "Centres de vote sur Saint Malo", 
       subtitle = "Données via : Open Data Saint Malo",
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
