library(tidyverse)
source("data-bzh-tools-master/main.R")

rennes <- read_csv2("https://data.rennesmetropole.fr/explore/dataset/resultats_p17/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")

ville <- rennes %>%
  filter(NIVEAU_DETAIL == "vi")
ville$NB_INSCRITS
ca <- rennes %>%
  filter(NIVEAU_DETAIL == "ca")
sum(ca$NB_INSCRITS)
ci <- rennes %>%
  filter(NIVEAU_DETAIL == "ci")
sum(ca$NB_INSCRITS)

#Ville

b <- ville %>%
  gather(type, volume, c(14:18,85)) %>%
  mutate(ratio = volume / 116084 * 100) %>%
  mutate(type = factor(type, levels = c("NB_INSCRITS", "NB_EMARGEMENTS", 
                                        "NB_BULLETINS","NB_NULS","NB_EXPRIMES", 
                                        "NB_BLANC"))) 

b %>%
  ggplot(aes(type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  geom_label(aes(label = round(ratio,2)),size = 5) +
  #coord_flip(expand = F) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du premier tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") +
  databzhTheme()


ville <- ville %>%
  mutate(`DUPONT-AIGNAN Nicolas` = NB_VOIX_1) %>%
  mutate(`LE PEN Marine` = NB_VOIX_2) %>%
  mutate(`MACRON Emmanuel` = NB_VOIX_3) %>%
  mutate(`HAMON Benoît` = NB_VOIX_4) %>%
  mutate(`ARTHAUD Nathalie` = NB_VOIX_5) %>%
  mutate(`POUTOU Philippe` = NB_VOIX_6) %>%
  mutate(`CHEMINADE Jacques` = NB_VOIX_7) %>%
  mutate(`LASSALLE Jean` = NB_VOIX_8) %>%
  mutate(`MÉLENCHON Jean-Luc` = NB_VOIX_9) %>%
  mutate(`ASSELINEAU François` = NB_VOIX_10) %>%
  mutate(`FILLON François` = NB_VOIX_11) %>%
  gather(candidat, voix, 86:96)


ville %>%
  ggplot(aes(reorder(candidat, voix), voix)) +
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  geom_label(aes(label = voix), size = 5) +
  coord_flip() +
  labs(x = "", 
      y = "", 
      title = "Premier tour des présidentielles 2017 à Rennes", 
      subtitle = "Données via Rennes Open Data",
      caption = "http://data-bzh.fr") +
  databzhTheme()

# Circonscription

ggplot(ci, aes(NOM_CIRCONSCRIPTION, as.numeric(POURCENTAGE_PARTICIPATION))) +
  geom_bar(stat = "identity", fill = databzh$colour2) + 
  ylim(0,100) +
  coord_flip() + 
  labs(x = "", 
       y = "", 
       title = "Taux de participation par circonscription, 1er tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

ggplot(ci, aes(NOM_CIRCONSCRIPTION, NB_BLANC)) +
  geom_bar(stat = "identity", fill = databzh$colour3) + 
  coord_flip() + 
  labs(x = "", 
       y = "", 
       title = "Nombre de votes blancs par circonscription, 1er tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

ci <- ci %>%
  mutate(`DUPONT-AIGNAN Nicolas` = NB_VOIX_1) %>%
  mutate(`LE PEN Marine` = NB_VOIX_2) %>%
  mutate(`MACRON Emmanuel` = NB_VOIX_3) %>%
  mutate(`HAMON Benoît` = NB_VOIX_4) %>%
  mutate(`ARTHAUD Nathalie` = NB_VOIX_5) %>%
  mutate(`POUTOU Philippe` = NB_VOIX_6) %>%
  mutate(`CHEMINADE Jacques` = NB_VOIX_7) %>%
  mutate(`LASSALLE Jean` = NB_VOIX_8) %>%
  mutate(`MÉLENCHON Jean-Luc` = NB_VOIX_9) %>%
  mutate(`ASSELINEAU François` = NB_VOIX_10) %>%
  mutate(`FILLON François` = NB_VOIX_11) %>%
  gather(candidat, voix, 86:96)


ci %>%
  ggplot(aes(reorder(candidat, voix), voix, fill = NOM_CIRCONSCRIPTION)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = databzh$colours, guide = FALSE) +
  geom_label(aes(label = voix), size = 2, fill = "#ffffff") +
  coord_flip() +
  facet_grid(NOM_CIRCONSCRIPTION ~. ) +
  labs(x = "", 
       y = "", 
       title = "Résultats par circonscription, 1er tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Canton

ggplot(ca, aes(NOM_CANTON, as.numeric(POURCENTAGE_PARTICIPATION))) +
  geom_bar(stat = "identity", fill = databzh$colour2) + 
  ylim(0,100) +
  coord_flip() + 
  labs(x = "", 
       y = "", 
       title = "Taux de participation par canton, 1er tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

ggplot(ca, aes(NOM_CANTON, NB_BLANC)) +
  geom_bar(stat = "identity", fill = databzh$colour3) + 
  coord_flip() + 
  labs(x = "", 
       y = "", 
       title = "Nombre de votes blancs par canton, 1er tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

ca <- ca %>%
  mutate(`DUPONT-AIGNAN Nicolas` = NB_VOIX_1) %>%
  mutate(`LE PEN Marine` = NB_VOIX_2) %>%
  mutate(`MACRON Emmanuel` = NB_VOIX_3) %>%
  mutate(`HAMON Benoît` = NB_VOIX_4) %>%
  mutate(`ARTHAUD Nathalie` = NB_VOIX_5) %>%
  mutate(`POUTOU Philippe` = NB_VOIX_6) %>%
  mutate(`CHEMINADE Jacques` = NB_VOIX_7) %>%
  mutate(`LASSALLE Jean` = NB_VOIX_8) %>%
  mutate(`MÉLENCHON Jean-Luc` = NB_VOIX_9) %>%
  mutate(`ASSELINEAU François` = NB_VOIX_10) %>%
  mutate(`FILLON François` = NB_VOIX_11) %>%
  gather(candidat, voix, 86:96)


ca %>%
  ggplot(aes(reorder(candidat, voix), voix, fill = NOM_CANTON)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = databzh$colours, guide = FALSE) +
  geom_label(aes(label = voix), size = 2, fill = "#ffffff") +
  coord_flip() +
  facet_grid(NOM_CANTON ~. ) +
  labs(x = "", 
       y = "", 
       title = "Résultats par canton, 1er tour des présidentielles 2017 à Rennes", 
       subtitle = "Données via Rennes Open Data",
       caption = "http://data-bzh.fr") +
  databzhTheme()
