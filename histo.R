library(tidyverse)
source("data-bzh-tools-master/main.R")
rennes <- read.csv2("https://data.rennesmetropole.fr/explore/dataset/immeubles-proteges-au-titre-des-monuments-historiques/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
glimpse(rennes)
library(knitr)
library(clipr)
library(magrittr)

rennes %>% 
  group_by(Commune) %>%
  count() %>%
  rename(Volume = n) %>%
  arrange(desc(Volume)) %>%
  kable() %>%
  write_clip()

clean_dat <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- tolower(text)
  text <- gsub("['`^~\"]", "", text)
  text <- gsub("^ ", "", text)
  text <- gsub(" $", "", text)
  text <- gsub("^ *$", NA, text)
  return(text)
}

rennes_bis <- rennes %>%
  separate(Statut, into = c("A","B"), sep = ";") %>%
  mutate(A= clean_dat(A), 
         B=clean_dat(B))
table(c(rennes_bis$A, rennes_bis$B)) %>%
  as.data.frame() %>%
  ggplot() +
  aes(reorder(Var1, Freq), Freq) +
  geom_col(fill = databzh$colour1) + 
  coord_flip() +
  labs(title = "Status des batîments",
       x = "Statut", 
       y = "Fréquence", 
       subtitle = "Données via Rennes Métropole Open Data", 
       caption = "data-bzh.fr") + 
  databzhTheme()

library(tidytext)

a <- rennes_bis %>% 
  mutate(appelation  = as.character(Appellation.courante)) %>%
  unnest_tokens(output = word, input = appelation) %>%
  count(word) %>%
  anti_join(proustr::proust_stopwords()) %>%
  rename(Volume = n) %>%
  arrange(desc(Volume)) %>%
  top_n(10) 

ggplot(a) +
  aes(reorder(word, Volume), Volume) +
  geom_col(fill = databzh$colour2) + 
  coord_flip() +
  labs(title = "Noms des batîments",
       x = "Mot dans l'appelation", 
       y = "Fréquence", 
       subtitle = "Données via Rennes Métropole Open Data", 
       caption = "data-bzh.fr") +
  databzhTheme()

library(rgdal)
roj <- readOGR(dsn=".", layer="emprise-de-rennes-metropole")
wmap_df <- fortify(roj)
rennes %<>% separate(Coordonnées.INSEE, into = c("lon", "lat"), sep = ",")

ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "#e4e4e4") + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group), color="grey50") + 
  geom_point(data=rennes, aes(as.numeric(lat), as.numeric(lon), group = NULL, color = Statut)) + 
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

