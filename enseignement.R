library(tidyverse)
source("data-bzh-tools-master/main.R")

bretagne <- read_csv2("https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-enseignants-titulaires-esr-public-national/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true") %>%
  filter(Région == "Bretagne")

# Effectif total ====

bretagne %>% 
  group_by(Rentrée) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif)) +
  geom_bar(stat = "identity", fill = databzh$colour1) +
  #ylim(c(2500,4000)) +
  labs(title = "Évolution de l'effectif", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Types d'établissement ====

bretagne %>% 
  group_by(Rentrée, `Type établissement`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif, fill = `Type établissement`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Évolution de l'effectif par type d'établissement", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Catégorie de personnels ==== 

bretagne %>% 
  group_by(Rentrée, `Categorie de personnels`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif, col = `Categorie de personnels`, group = `Categorie de personnels`)) +
  geom_line(size = 3) +
  scale_color_manual(values = databzh$colours) +
  ylim(c(0, 2000)) +
  labs(title = "Évolution de l'effectif par catégorie de personnels", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Grandes disciplines ====

bretagne %>% 
  group_by(Rentrée, `Grande discipline`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif, col = `Grande discipline`, group = `Grande discipline`)) +
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours) +
  #ylim(c(500, 2000)) +
  labs(title = "Évolution de l'effectif par grandes disciplines", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Effectif sexe ====

bretagne %>% 
  group_by(Rentrée, Sexe) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif, fill = Sexe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Évolution de l'effectif par sexe", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Sexe et type d'étac
# Sexe et types d'établissement ====

bretagne %>% 
  group_by(`Type établissement`, Sexe) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Type établissement`, Effectif, fill = Sexe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip()+
  labs(title = "Répartition par sexe par type d'établissement", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Sexe et catégorie de personnels ==== 

bretagne %>% 
  group_by(Sexe, `Categorie de personnels`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Categorie de personnels`, Effectif, fill = Sexe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip() +
  labs(title = "Répartition par sexe et par catégorie de personnels", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Sexe et Grandes disciplines ====

bretagne %>% 
  group_by(Sexe, `Grande discipline`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Grande discipline`, Effectif, fill = Sexe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip() +
  labs(title = "Répartition par sexe et par grandes disciplines", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Sexe et Classe âge ====

bretagne %>% 
  group_by(Sexe, `Classe âges`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Classe âges`, Effectif, fill = Sexe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip() +
  labs(title = "Répartition par sexe et par classes d'âges", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Effectif nationalité ====

bretagne %>% 
  group_by(Rentrée, Nationalité) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif, fill = Nationalité)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Évolution de l'effectif par nationalité", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Sexe et type d'étac
# Nationalité et types d'établissement ====

bretagne %>% 
  group_by(`Type établissement`, Nationalité) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Type établissement`, Effectif, fill = Nationalité)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Répartition par nationalité et par type d'établissement", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Nationalité et catégorie de personnels ==== 

bretagne %>% 
  group_by(Nationalité, `Categorie de personnels`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Categorie de personnels`, Effectif, fill = Nationalité)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Répartition par nationalité et par catégorie de personnels", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Nationalité et Grandes disciplines ====

bretagne %>% 
  group_by(Nationalité, `Grande discipline`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Grande discipline`, Effectif, fill = Nationalité)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Répartition par nationalité et par grandes disciplines", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Nationalité et Classe âge ====

bretagne %>% 
  group_by(Nationalité, `Classe âges`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Classe âges`, Effectif, fill = Nationalité)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Répartition par nationalité et par classes d'âges", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()



# Effectif classe d'âge ====

bretagne$`Classe âges` <- factor(bretagne$`Classe âges`)
bretagne$`Classe âges` <- factor(bretagne$`Classe âges`, levels=levels(bretagne$`Classe âges`)[order(levels(bretagne$`Classe âges`), decreasing = TRUE)])

bretagne %>% 
  group_by(Rentrée, `Classe âges`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(Rentrée, Effectif, fill = `Classe âges`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = databzh$colours) +
  labs(title = "Évolution de l'effectif par classes d'âges", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# `Classe âges` et types d'établissement ====

bretagne %>% 
  group_by(`Type établissement`, `Classe âges`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Type établissement`, Effectif, fill = `Classe âges`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip() +
  labs(title = "Répartition par classes d'âges et par type d'établissement", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()


# `Classe âges` et catégorie de personnels ==== 

bretagne %>% 
  group_by(`Classe âges`, `Categorie de personnels`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Categorie de personnels`, Effectif, fill = `Classe âges`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip() +
  labs(title = "Répartition par classe d'âge et par catégorie de personnels", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

# `Classe âges` et Grandes disciplines ====

bretagne %>% 
  group_by(`Classe âges`, `Grande discipline`) %>% 
  summarise(Effectif = sum(effectif)) %>% 
  ggplot(aes(`Grande discipline`, Effectif, fill = `Classe âges`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  coord_flip()+
  labs(title = "Répartition par classes d'âges et par grandes disciplines", 
       subtitle = "Données via Open Data Enseignement Supérieur", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
