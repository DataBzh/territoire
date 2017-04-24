library(tidyverse)
source("/Users/colinfay/Downloads/data-bzh-tools-master/main.R")

dep_cand <- read_csv2("prs-2017-candidat-departement.csv")
dep_type <- read_csv2("prs-2017-type-departement.csv")
reg_cand <- read_csv2("prs-2017-candidat-region.csv")
reg_type <- read_csv2("prs-2017-type-region.csv")

# Région

reg_type <- reg_type %>%
  mutate(ratio = Nombre / 2453682 * 100) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimés"))) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))

reg_type %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  geom_label(aes(label = pourc),size = 5) +
  #coord_flip(expand = F) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du premier tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via elections.interieur.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


reg_cand %>%
  ggplot(aes(reorder(Candidats, Voix), Voix)) +
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  geom_label(aes(label = Voix), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Premier tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via elections.interieur.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


# Par département

dep_type$ratio <- NA 
dep_type$pourc <- NA
dep_type[1:6,] <- dep_type[1:6,] %>%
  mutate(ratio = Nombre / 455958 * 100) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))
dep_type[7:12,] <- dep_type[7:12,] %>%
  mutate(ratio = Nombre / 690570 * 100) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))
dep_type[13:18,] <- dep_type[13:18,] %>%
  mutate(ratio = Nombre / 730142 * 100) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))
dep_type[19:24,] <- dep_type[19:24,] %>%
  mutate(ratio = Nombre / 577012 * 100) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))

dep_type %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
  "Votants","Blancs","Nuls", "Exprimés"))) %>%
  ggplot(aes(Type, ratio, fill = Département)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = databzh$colours[2:6]) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du premier tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via elections.interieur.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


dep_cand %>%
  ggplot(aes(reorder(Candidats, `% Exprimés`), `% Exprimés`, fill = Département)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = databzh$colours[2:6]) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Premier tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via elections.interieur.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

dep_type %>%
  filter(Département == "Côtes-d'Armor (22)") %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", "Exprimés"))) %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colours[2]) +
  geom_label(aes(label = pourc),size = 5) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du premier tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via elections.interieur.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


dep_cand %>%
  filter(Département == "Côtes-d'Armor (22)") %>%
  ggplot(aes(reorder(Candidats, Voix), Voix)) +
  geom_bar(stat = "identity", fill = databzh$colours[2]) + 
  scale_fill_manual(values = databzh$colours[2]) +  
  geom_label(aes(label = Voix), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Premier tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via elections.interieur.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()
