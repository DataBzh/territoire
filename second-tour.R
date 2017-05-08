library(tidyverse)
library(readxl)
source("/home/colin/R/misc/data-bzh-tools-master/main.R")
reg <- read_csv2("Presidentielle_2017_Resultats_Communes_Tour_2.csv")
reg <- reg %>%
  filter(`Libellé du département` == "Côtes-d'Armor" | 
          `Libellé du département` == "Finistère" | 
          `Libellé du département` == "Ille-et-Vilaine" | 
          `Libellé du département` == "Morbihan")

reg_type <- reg %>%
  select(`Libellé du département`, Inscrits, Abstentions, Votants, 
         Blancs, Nuls, Exprimés)

sum(reg$Inscrits)
sum(reg$Abstentions) / sum(reg$Inscrits) * 100
sum(reg$Exprimés)
sum(reg$Blancs)

reg_type_ratio <-  reg_type %>%
  mutate(Abstentions = Abstentions / Inscrits * 100, 
         Votants = Votants / Inscrits * 100,
         Blancs = Blancs / Inscrits * 100, 
         Nuls = Nuls / Inscrits * 100, 
         Exprimés = Exprimés / Inscrits * 100, 
         Inscrits = 100)

reg_type_ratio %>%
  gather(key = Type, value = ratio, 2:7) %>%
  group_by(Type) %>%
  summarise(ratio = round(mean(ratio),2)) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                         "Votants","Blancs","Nuls", 
                                         "Exprimés"))) %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  geom_label(aes(label = ratio),size = 5) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

reg_cand <- reg %>%
  select(c(2, 20:24, 26:29)) %>%
  unite(Macron, Nom, Prénom, sep = " ") %>%
  unite(LePen, Nom_1, Prénom_1, sep = " ")
x <- sum(reg_cand$Voix) + sum(reg_cand$Voix_1) 
sum(reg_cand$Voix) / x * 100

reg_type_ratio %>%
  gather(key = Type, value = ratio, 2:7) %>%
  group_by(`Libellé du département`, Type) %>%
  summarise(ratio = round(mean(ratio),2)) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimés"))) %>%
  ggplot(aes(Type, ratio, fill = `Libellé du département`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = databzh$colours) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

reg_cand %>%
  group_by(`Libellé du département`) %>%
  summarise(tot_vot = sum(Voix) + sum(Voix_1),
            `Emmanuel Macron` = sum(Voix) / tot_vot * 100, 
            `Marine Le Pen` = sum(Voix_1) / tot_vot*100, 
            verif = `Emmanuel Macron` + `Marine Le Pen`) %>%
  gather(key = Candidat, value = Pourc, c(3,4)) %>%
  ggplot(aes(`Libellé du département`, Pourc, fill = Candidat)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = databzh$colours) +
  labs(x = "", 
       y = "", 
       title = "Second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

reg_type_ratio %>%
  gather(key = Type, value = ratio, 2:7) %>%
  group_by(`Libellé du département`, Type) %>%
  summarise(ratio = round(mean(ratio),2)) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimés"))) %>%
  filter(`Libellé du département` == "Côtes-d'Armor") %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", position = "dodge", fill = databzh$colour2) +
  geom_label(aes(label = ratio),size = 5, fill = "#FFFFFF") +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

reg_type_ratio %>%
  gather(key = Type, value = ratio, 2:7) %>%
  group_by(`Libellé du département`, Type) %>%
  summarise(ratio = round(mean(ratio),2)) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimés"))) %>%
  filter(`Libellé du département` == "Finistère") %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", position = "dodge", fill = databzh$colour3) +
  geom_label(aes(label = ratio),size = 5, fill = "#FFFFFF") +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

reg_type_ratio %>%
  gather(key = Type, value = ratio, 2:7) %>%
  group_by(`Libellé du département`, Type) %>%
  summarise(ratio = round(mean(ratio),2)) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimés"))) %>%
  filter(`Libellé du département` == "Ille-et-Vilaine") %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", position = "dodge", fill = databzh$colour4) +
  geom_label(aes(label = ratio),size = 5, fill = "#FFFFFF") +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

reg_type_ratio %>%
  gather(key = Type, value = ratio, 2:7) %>%
  group_by(`Libellé du département`, Type) %>%
  summarise(ratio = round(mean(ratio),2)) %>%
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimés"))) %>%
  filter(`Libellé du département` == "Morbihan") %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", position = "dodge", fill = databzh$colour5) +
  geom_label(aes(label = ratio),size = 5, fill = "#FFFFFF") +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des présidentielles 2017 en Bretagne", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

library(rgdal)
roj <- readOGR(dsn=".", layer="R53_dep")
wmap_df <- fortify(roj)
villes <- read_csv2("http://data-bzh.fr/data/bretagne.csv")
villes <- merge(x = reg, y = villes, by.x = "Libellé de la commune", by.y = "name")
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon() + 
  coord_map() +
  geom_path(data=wmap_df, aes(long, lat, group=group), color="grey50") +
  geom_point(data = villes, aes(long, lat, col = `% Voix/Exp_1`, group = NULL),size = 4) + 
  scale_color_gradient(low = databzh$colour2, high = databzh$colour1) +
  coord_map() +
  labs(x = "", 
       y = "", 
       title = "Résultats de M. Le Pen au second tour en Bretagne", 
       subtitle = "Données via data.gouv",
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
