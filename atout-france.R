library(tidyverse)
library(magrittr)
library(tidytext)
source("/home/colin/R/misc/data-bzh-tools-master/main.R")

filt_bret <- function(data, type){
  data %>%
    filter(grepl(("22...|29...|35...|56..."), `CODE POSTAL`)) %>%
    mutate(TYPE = type)
}

#parcs residentiels 

parc_resid_bret <- data.table::fread("https://www.data.gouv.fr/s/resources/parcs-residentiels-de-loisirs-classes-en-france/20170623-162249/Classement_Parcs_20170623.csv", encoding = "Latin-1") %>%
  filt_bret("Parcs résidentiels de loisirs classés")

#villages residentiels 

villages_bret <- data.table::fread("https://www.data.gouv.fr/s/resources/villages-de-vacances-classes-en-france/20170623-162434/Classement_Villages_20170623.csv", encoding = "Latin-1") %>%
  filt_bret("Villages de vacances classés")

#residences 

residences_bret <- data.table::fread("https://www.data.gouv.fr/s/resources/residences-de-tourisme-classees-en-france/20170623-162647/Classement_Residences_20170623.csv", encoding = "Latin-1") %>%
  filt_bret("Résidences de tourisme classées")

#hotel 

hotel_bret <- data.table::fread("https://www.data.gouv.fr/s/resources/hotels-classes-en-france/20170623-161122/Classement_Hotels_170623.csv", encoding = "Latin-1") %>%
  filt_bret("Hôtels classés")

#camping 

camp_class_bret <- data.table::fread("https://www.data.gouv.fr/s/resources/campings-classes-en-france/20170623-163427/Classement_Campings_20170623.csv", encoding = "Latin-1")%>%
  filt_bret("Campings classés")

#Full 

full <- rbind(camp_class_bret, hotel_bret, parc_resid_bret, residences_bret, villages_bret) %>%
  mutate(DEPARTEMENT = substr(.$`CODE POSTAL`, 1, 2), 
         `DATE DE CLASSEMENT` = lubridate::dmy(`DATE DE CLASSEMENT`), 
         `DATE DE PUBLICATION DE L'ETABLISSEMENT` = lubridate::dmy(`DATE DE PUBLICATION DE L'ETABLISSEMENT`)) 

# Par dép 

table(full$TYPE, full$DEPARTEMENT)
full %>%
  ggplot(aes(DEPARTEMENT, fill = TYPE)) + 
  geom_bar() + 
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Établissements par département et par type", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

# Par dép 

table(full$TYPE, full$DEPARTEMENT)
full %>%
  ggplot(aes(DEPARTEMENT, fill = TYPE)) + 
  geom_bar() + 
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Établissements par département et par type", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

#Classement

full %>%
  ggplot(aes(CLASSEMENT)) + 
  geom_bar(fill = databzh$colour1) + 
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Établissements par classement", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

table(full$CLASSEMENT, full$DEPARTEMENT)

full %>%
  ggplot(aes(DEPARTEMENT, fill = CLASSEMENT)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Établissements par classement et par département", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr", 
       y = "") + 
  databzhTheme()

#Commune 

full %>%
  group_by(COMMUNE) %>%
  summarise(Volume = n()) %>%
  arrange(desc(Volume)) %>%
  slice(1:15) %>%
  as.data.frame()

bret <- read_csv2("http://data-bzh.fr/data/bretagne.csv")
full$COMMUNE <- tolower(full$COMMUNE)
bret$name <- tolower(bret$name)
full <- merge(full, bret, by.x = "COMMUNE", by.y = "name")

# Etablissements par nombre d'habitant 

full %>%
  group_by(COMMUNE,DEPARTEMENT) %>%
  summarize(ETABLISSEMENTS = n(), 
            POPULATION = mean(population)) %>%
  mutate(`ETAB/POP` = ETABLISSEMENTS/POPULATION) %>%
  arrange(desc(`ETAB/POP`)) %>%
  .[1:15,] %>%
  as.data.frame() %>%
  print() %>%
  ggplot(aes(x = reorder(COMMUNE, `ETAB/POP`), y = `ETAB/POP`, fill = DEPARTEMENT)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = databzh$colours) +
  coord_flip() + 
  labs(title = "15 villes avec le plus haut rapport Etablissement / Population", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  databzhTheme()

# Carte 
library(rgdal)
wmap_df <- readOGR(dsn=".", layer="R53_dep") %>%
  fortify()

full_map <- full %>%
  group_by(`CODE POSTAL`, DEPARTEMENT) %>%
  summarize(compte = n(), 
            lat = mean(lat), 
            long = mean(long))
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "grey") + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group, fill=NULL), color="grey50") +
  geom_point(data = full_map, aes(x = as.numeric(long), y = as.numeric(lat), group = NULL, size = compte, col = DEPARTEMENT)) + 
  scale_color_manual(values = databzh$colours, name = "") +
  labs(title = "Établissements recensés par Atout France en Bretagne", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr") + 
  databzhTheme() + 
  theme(title=element_text(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major= element_line("grey50", linetype = "dashed"), 
        panel.background= element_blank()) 

# Capacité d'accueil 

full %<>% mutate(`CAPACITÉ D'ACCUEIL (PERSONNES)` = as.numeric(`CAPACITÉ D'ACCUEIL (PERSONNES)`))

full %>%
  ggplot(aes(`CAPACITÉ D'ACCUEIL (PERSONNES)`)) + 
  geom_histogram(fill = databzh$colour2, bins = 100) + 
  labs(title = "Capacité d'accueil des établissements", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr", 
       y = "") + 
  databzhTheme()


full %>%
  filter(CLASSEMENT != "Aire naturelle") %>%
  ggplot(aes(`CAPACITÉ D'ACCUEIL (PERSONNES)`, fill = DEPARTEMENT)) + 
  geom_histogram(bins = 50) + 
  scale_fill_manual(values = databzh$colours) +
  facet_grid(DEPARTEMENT~ CLASSEMENT) + 
  labs(title = "Capacité d'accueil des établissements par département et par classement", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bzh.fr", 
       y = "") + 
  databzhTheme()

# Nom des établissements 

tibble(text = full$`NOM COMMERCIAL`) %>%
  unnest_tokens(output = "word", input = "text") %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% c(stopwords::stopwords_iso$fr, "camping", "hôtel", "restaurant", "résidence", "parc","loisirs")) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = databzh$colour3) + 
  coord_flip() + 
  labs(title = "15 mots les plus récurrents dans les noms", 
       subtitle = "Données via Atout France", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()
