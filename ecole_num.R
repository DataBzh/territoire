library(tidyverse)
library(rgdal)
library(knitr)
source("data-bzh-tools-master/main.R")

bret <- read_csv2("https://data.education.gouv.fr/explore/dataset/fr-en-ecoles-colleges-numeriques/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true") %>% 
  filter(Région == "BRETAGNE")

bret %>% 
  count(`Denomination principale`) %>%
  arrange(desc(n)) %>%
  rename(compte = n) %>%
  ggplot() + 
  aes(reorder(`Denomination principale`, compte), compte) + 
  geom_col(fill = databzh$colour1) + 
  coord_flip() + 
  labs(title = "Dénomination principal des établissements", 
       subtitle = "données via Éducation Nationale Open Data", 
       y = "Compte", 
       x = "Dénomination", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

bret %>% 
  count(`Secteur`) %>%
  arrange(desc(n)) %>%
  rename(compte = n) %>%
  kable() %>%
  clipr::write_clip()

bret %>% 
  count(`Nature`) %>%
  arrange(desc(n)) %>%
  rename(compte = n) %>%
  kable() %>%
  clipr::write_clip()

bret %>% 
  count(Département) %>%
  arrange(desc(n)) %>%
  rename(compte = n) %>%
  ggplot() + 
  aes(reorder(Département, compte), compte) + 
  geom_col(fill = databzh$colour2) + 
  coord_flip() + 
  labs(title = "Départements des établissements", 
       subtitle = "données via Éducation Nationale Open Data", 
       y = "Compte", 
       x = "Départements", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

bret %>% 
  count(`Localite acheminement`) %>%
  arrange(desc(n)) %>%
  rename(compte = n) %>%
  top_n(10) %>%
  ggplot() + 
  aes(reorder(`Localite acheminement`, compte), compte) + 
  geom_col(fill = databzh$colour3) + 
  coord_flip() + 
  labs(title = "Localité des établissements", 
       subtitle = "données via Éducation Nationale Open Data", 
       y = "Compte", 
       x = "Localité", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()