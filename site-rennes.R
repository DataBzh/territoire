library(tidyverse)
library(lubridate)
site_rennes <- read_csv2("https://data.rennesmetropole.fr/explore/dataset/statistiques-de-frequentation-du-site-de-rennes-ville-et-metropole/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
site_rennes <- arrange(site_rennes, Date)
names(site_rennes)[3] <- "Pages_vues"

ggplot(site_rennes2, aes(Utilisateurs, Pages_vues)) + 
  geom_point(col = databzh$colour1) + 
  geom_smooth() + 
  xlab("Volume d'utilisateurs") +
  ylab("Volume de pages vues") +
  labs(title = "Pages vues ~ nombre de visiteurs — site de Rennes Ville et Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_rennes <- site_rennes %>%
  filter(Date >= ymd("2016-01-01") & Date <= ymd("2016-12-31")) %>%
  mutate(Site = "Rennes_Met", 
         Page_par_visiteur = Pages_vues / Utilisateurs, 
         Mois = format(.$Date, "%Y-%m"), 
         Jour = factor(weekdays(.$Date), level = c("lundi", "mardi","mercredi", "jeudi","vendredi","samedi","dimanche"))) %>%
  gather("Type","Volume", 2:3)

sum(subset(site_rennes, Type == "Pages_vues")$Volume)

site_rennes %>%
  group_by(Type) %>%
  summarize(mean = mean(Volume), 
              sd = sd(Volume), 
              median = median(Volume), 
              IQR = IQR(Volume))

ggplot(site_rennes, aes(Date, Volume, col = Jour, group = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Statistiques de fréquentation quotidienne du site de Rennes Ville et Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

subset(site_rennes, Type == "Pages_vues") %>%
  ggplot(aes(Date, Volume, col = Jour, group = Jour)) +
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Pages vues au quotidien — Site de Rennes Ville et Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_rennes %>%
  group_by(Jour) %>%
  summarise(Volume = mean(Volume)) %>%
  ggplot(aes(Jour, Volume)) +
  geom_bar(stat = "identity", fill = databzh$colour1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Jour de la semaine") +
  ylab("Visites moyennes") +
  labs(title = "Fréquentation moyenne du site de Rennes Ville et Métropole en fonction du jour de la semaine", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_rennes %>%
  group_by(Type, Mois) %>%
  summarise(Volume = sum(Volume)) %>%
  ggplot(aes(Mois, Volume, col = Type, group = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Statistiques de fréquentation mensuelle du site de Rennes Ville et Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

subset(site_rennes, Type == "Pages_vues") %>%
  mutate(Num = format(.$Date, "%d")) %>%
ggplot(aes(Num, Mois, fill = Volume)) +
  geom_tile() + 
  scale_fill_gradient(low = databzh$colour5, high = databzh$colour1) + 
  xlab("Jour de la semaine") +
  ylab("Mois") +
  labs(title = "Heatmap de la fréquentation mensuelle du site de Rennes Ville et Métropole", 
       subtitle = "Données via : Open Data Rennes",
       caption = "http://data-bzh.fr") + 
  databzhTheme()
