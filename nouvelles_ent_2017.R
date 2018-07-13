# Entreprises immatriculées en 2017
library(tidyverse)
source("data-bzh-tools-master/")
bret <- read_csv2("https://bretagne.territoires.opendatasoft.com/explore/dataset/entreprises-immatriculees-en-2017/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")

library(knitr)
library(clipr)
bret %>% 
  count(Département) %>% 
  kable() %>% 
  write_clip()

bret %>% 
  count(Ville, sort = TRUE) %>%
  top_n(10) %>% 
  ggplot() + 
  aes(reorder(Ville, n), n) +
  geom_col(fill = databzh$colour1) + 
  coord_flip() +
  labs(x = "Ville", 
       y = "", 
       title = "Villes où l'on compte le plus de crétion \n d'entreprise en 2017", 
       subtitle = "Données via bretagne.territoires.opendatasoft.com",
       caption = "http://data-bzh.fr") +
  databzhTheme()

library(rgdal)
roj <- readOGR(dsn=".", layer="R53_dep")
wmap_df <- fortify(roj)

bret_n <- bret %>% 
  separate(Geolocalisation, into = c("lat", "long"), sep = ", ") %>%
  mutate_at(vars(long, lat), as.numeric)
wmap_df %>%
  ggplot(aes(long,lat, group=group)) + 
  geom_polygon() + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group), color="grey50") +
  geom_point(data = bret_n, aes(long, lat, group = NULL, color = Département)) + 
  scale_color_manual(values = databzh$colours[1:4]) +
  coord_map() +
  labs(x = "", 
       y = "", 
       title = "Localisation des nouvelles entreprises\n créées en Bretagne en 2017", 
       subtitle = "Données via bretagne.territoires.opendatasoft.com",
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

bret %>% 
  count(`Forme Juridique`, sort = TRUE) %>% 
  top_n(10) %>% 
  kable() %>% 
  write_clip()

bret %>% 
  count(`Secteur d'activité`, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot() + 
  aes(reorder(`Secteur d'activité` , n), n) +
  geom_col(fill = databzh$colour2) + 
  coord_flip() +
  labs(x = "Secteur d'activité", 
       y = "", 
       title = "Secteurs d'activités où l'on compte le plus de crétion \n d'entreprise en 2017", 
       subtitle = "Données via bretagne.territoires.opendatasoft.com",
       caption = "http://data-bzh.fr") +
  databzhTheme()

bret %>% 
  count(Greffe, sort = TRUE) %>% 
  top_n(10) %>% 
  kable() %>% 
  write_clip()

ggplot(bret) + 
  aes(`Date immatriculation`) + 
  geom_histogram(bins = 52, fill = databzh$colour3) +
  labs(x = "Secteur d'activité", 
       y = "", 
       title = "Date d'immatriculation des entreprises \n créées en 2017", 
       subtitle = "Données via bretagne.territoires.opendatasoft.com",
       caption = "http://data-bzh.fr") +
  databzhTheme()
