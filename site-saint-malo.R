library(tidyverse)
library(lubridate)
site_stmalo <- read_csv2("https://data.stmalo-agglomeration.fr/explore/dataset/frequentation-du-site-saint-malofr/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true") %>%
  na.omit()
site_stmalo <- arrange(site_stmalo, Jour)
site_stmalo2016 <- site_stmalo %>%
  filter(Jour >= ymd("2016-01-01") & Jour <= ymd("2016-12-31")) 

ggplot(site_stmalo2016, aes(`Visiteurs uniques`, `Pages vues`)) + 
  geom_point(col = databzh$colour1) + 
  geom_smooth() + 
  xlab("Volume d'utilisateurs") +
  ylab("Volume de pages vues") +
  labs(title = "Pages vues ~ nombre de visiteurs — site de Saint-Malo", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_stmalo2016 <- site_stmalo2016 %>%
  mutate(Site = "stmalo_Met", 
         Page_par_visiteur = `Pages vues` / `Visiteurs uniques`, 
         Mois = format(.$Jour, "%Y-%m"), 
         Day = factor(weekdays(Jour), level = c("lundi", "mardi","mercredi", "jeudi","vendredi","samedi","dimanche"))) %>%
  gather("Type","Volume", 3:4)

site_stmalo2016 %>%
  filter(Type == "Visiteurs uniques") %>%
  summarise(somme = sum(Volume))
site_stmalo2016 %>%
  filter(Type == "Pages vues") %>%
  summarise(somme = sum(Volume))

site_stmalo2016 %>%
  group_by(Type) %>%
  summarize(mean = mean(Volume, na.rm = TRUE), 
            sd = sd(Volume, na.rm = TRUE), 
            median = median(Volume, na.rm = TRUE), 
            IQR = IQR(Volume, na.rm = TRUE))

ggplot(site_stmalo2016, aes(Jour, Volume, col = Type, group = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Statistiques de fréquentation quotidienne du site de Saint-Malo", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

subset(site_stmalo2016, Type == "Pages vues") %>%
  ggplot(aes(Jour, Volume, col = Day, group = Day)) +
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Pages vues au quotidien — Site de Saint-Malo", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_stmalo2016 %>%
  group_by(Day) %>%
  summarise(Volume = mean(Volume, na.rm = TRUE)) %>%
  ggplot(aes(Day, Volume)) +
  geom_bar(stat = "identity", fill = databzh$colour1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Jour de la semaine") +
  ylab("Visites moyennes") +
  labs(title = "Fréquentation moyenne du site de Saint-Malo en fonction du jour de la semaine", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_stmalo2016 %>%
  group_by(Type, Mois) %>%
  summarise(Volume = sum(Volume)) %>%
  ggplot(aes(Mois, Volume, col = Type, group = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Statistiques de fréquentation mensuelle du site de Saint-Malo", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

subset(site_stmalo2016, Type == "Pages vues") %>%
  #mutate(Num = format(.$Jour, "%d")) %>%
  ggplot(aes(Day, Mois, fill = Volume)) +
  geom_tile() + 
  scale_fill_gradient(low = databzh$colour5, high = databzh$colour1) + 
  xlab("Jour de la semaine") +
  ylab("Mois") +
  labs(title = "Heatmap de la fréquentation mensuelle du site de Saint-Malo", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()

site_stmalo$year <- year(site_stmalo$Jour)
site_stmalo$Mois <- format(site_stmalo$Jour, "%B")
site_stmalo$jourmois <- format(site_stmalo$Jour, "%d-%m") %>%
  as.factor()
site_stmalo %>%
  #group_by(year, jourmois) %>%
  #summarise(Volume = sum(Volume)) %>%
  ggplot(aes(Mois, Visites, col = as.factor(year), group = as.factor(year))) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours) +
  xlab("Date") +
  ylab("Visite") +
  labs(title = "Statistiques de fréquentation mensuelle du site de Saint-Malo Ville et Métropole", 
       subtitle = "Données via : Open Data Saint-Malo",
       caption = "http://data-bzh.fr") + 
  databzhTheme()
