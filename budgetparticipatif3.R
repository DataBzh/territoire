# Budget participatif de la ville de Rennes 
library(tidyverse)
source("data-bzh-tools-master/main.R")

data <- read_csv2("https://data.rennesmetropole.fr/explore/dataset/2018-02-12_budget-participatif-3_depot-des-projets-2/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")glimpse(data)
count(data, Auteur) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>%
  rename(compte = n) %>%
  ggplot() +
  aes(reorder(Auteur, compte), compte) +
  geom_col(fill = databzh$colour1) +
  coord_flip() +
  labs(title = "Rennais ayanté déposé un projet pour le\nBudget participatif - Ville de Rennes - Saison 3", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr", 
       x = "Auteur", 
       y = "Nombre de projets déposés") + 
  databzhTheme()

count(data, Catégorie) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>%
  rename(compte = n) %>%
  ggplot() +
  aes(reorder(Catégorie, compte), compte) +
  geom_col(fill = databzh$colour4) +
  coord_flip() +
  labs(title = "Catégorie des projets déposés pour le\nBudget participatif - Ville de Rennes - Saison 3", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr", 
       x = "Catégorie", 
       y = "Nombre de projets déposés") + 
  databzhTheme()
  
  
count(data, Auteur) %>% 
  summarize(mean(n), median(n))

count(data, `Type de profil`) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>%
  rename(compte = n) %>%
  knitr::kable() %>%
  clipr::write_clip()
  

ggplot(data) +
  aes(Création) +
  geom_density(fill = databzh$colour2) +
  labs(title = "Date de dépôt pour le\nBudget participatif - Ville de Rennes - Saison 3", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr", 
       x = "Date", 
       y = "Densité de projets déposés") + 
  databzhTheme()

count(data, `Dernier statut`) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>%
  rename(compte = n) %>%
  knitr::kable() %>%
  clipr::write_clip()

library(scales)

ggplot(data) +
  aes(`Estimation du coût`) +
  geom_density(fill = databzh$colour3) +
  scale_x_continuous(labels = comma) +
  labs(title = "Estimation du budgets des projets pour le\nBudget participatif - Ville de Rennes - Saison 3", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr", 
       x = "Estimation du coup", 
       y = "Densité de projets déposés") + 
  databzhTheme()

data %>% 
  select(`Estimation du coût`) %>%
  na.omit() %>%
  summarize(mean(`Estimation du coût`), median(`Estimation du coût`))

data %>% 
  select(Titre, `Estimation du coût`) %>%
  arrange(`Estimation du coût`) %>%
  top_n(- 2) %>%
  knitr::kable() %>%
  clipr::write_clip()

count(data, `Coup de coeur`) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  rename(compte = n) %>%
  knitr::kable() %>%
  clipr::write_clip()


count(data, `Zone géographique`) %>% 
  arrange(desc(n)) %>% 
  rename(compte = n) %>%
  ggplot() +
  aes(reorder(`Zone géographique`, compte), compte) +
  geom_col(fill = databzh$colour5) +
  coord_flip() +
  labs(title = "Zone géographique des projets déposés pour le\nBudget participatif - Ville de Rennes - Saison 3", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr", 
       x = "Zone géographique", 
       y = "Nombre de projets déposés") + 
  databzhTheme()

geo <- data %>%
  separate(geo, into = c("x", "y"), sep = ",")

# Emplacement des parkings
library(rgdal)
wmap_df <- readOGR(dsn=".", layer="perimetres-des-45-sous-quartiers-de-la-ville-de-rennes") %>% 
  fortify()

ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "#e4e4e4") + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group), color="grey50") + 
  geom_point(data=geo, aes(as.numeric(y), as.numeric(x), group = NULL), size = 3, color = databzh$colour8) + 
  scale_color_manual(values = databzh$colours) +
  xlab("") +
  ylab(" ") +
  labs(title = "Zone géographique des projets déposés pour le\nBudget participatif - Ville de Rennes - Saison 3", 
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

library(tidytext)
library(proustr)
data %>%
  unnest_tokens(word, Description) %>%
  select(word) %>%
  anti_join(proust_stopwords(), by = c(word = "word")) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot() + 
  aes(reorder(word, n), n) + 
  geom_col(fill = databzh$colour8) +
  coord_flip() +
  labs(title = "Termes fréquents dans les descriptions des projets déposés pour le\nBudget participatif - Ville de Rennes - Saison 3", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr", 
       x = "Terme", 
       y = "Volume") + 
  databzhTheme()

