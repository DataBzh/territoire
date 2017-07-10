library(tidyverse)
library(scales)
source("/home/colin/R/misc/data-bzh-tools-master/main.R")
pop <- read_csv2("https://bretagne.territoires.opendatasoft.com/explore/dataset/population-active-25-54-ans-par-categorie-socio-professionnelle-et-activite-1968/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")

#Total 

pop %>%
  group_by(Année) %>% 
  summarize(Somme = sum(value, na.rm = TRUE)) %>% 
  ggplot(aes(Année, Somme)) + 
  geom_bar(stat = "identity", fill = databzh$colour1) +
  scale_y_continuous(labels = comma) + 
  labs(title = "Évolution de la population active en Bretagne", 
       subtitle = "Données via Open Data Territoire", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()


# Par département 

pop %>%
  mutate(Département = as.factor(Département)) %>% 
  group_by(Année, Département) %>% 
  summarize(Somme = sum(value, na.rm = TRUE)) %>% 
  ggplot(aes(Année, Somme, group = Département, col = Département)) + 
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Évolution de la population active par département", 
       subtitle = "Données via Open Data Territoire", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

# Par CSP, actifs

pop %>%
  mutate(Département = as.factor(Département)) %>% 
  .[grep("Actifs", .$`CSP - Activité`),] %>% 
  mutate(`CSP - Activité` = stringr::str_replace_all(`CSP - Activité`, "Ayant Un Emploi", "")) %>% 
  group_by(Année, `CSP - Activité`) %>% 
  summarize(Somme = sum(value, na.rm = TRUE)) %>% 
  ggplot(aes(Année, Somme, group = `CSP - Activité`, col = `CSP - Activité`)) + 
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Évolution de la population active en Bretagne", 
       subtitle = "Données via Open Data Territoire", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

# Par CSP, actifs

pop %>%
  mutate(Département = as.factor(Département)) %>% 
  .[grep("Chômeurs", .$`CSP - Activité`),] %>% 
  group_by(Année, `CSP - Activité`) %>% 
  summarize(Somme = sum(value, na.rm = TRUE)) %>% 
  ggplot(aes(Année, Somme, group = `CSP - Activité`, col = `CSP - Activité`)) + 
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Évolution de la population de chômeurs en Bretagne", 
       subtitle = "Données via Open Data Territoire", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()


