devtools::install_github("ThinkRstat/legislatives2017")
library(tidyverse)
source("data-bzh-tools-master/main.R")

# Donnees ====

bret <- legislatives2017::second_tour %>% 
  filter(code_dpt %in% c("22","29","35","56"))

num_circ <- bret %>% 
  group_by(code_dpt, num_circ) %>% 
  summarise(Inscrits = mean(Inscrits),
            Abstentions = mean(Abstentions),
            Votants = mean(Votants), 
            Blancs = mean(Blancs),
            Nuls = mean(Nuls), 
            Exprimes = mean(Exprimes))


dep <- num_circ %>% 
  group_by(code_dpt) %>% 
  summarise(Inscrits = sum(Inscrits),
            Abstentions = sum(Abstentions),
            Votants = sum(Votants), 
            Blancs = sum(Blancs),
            Nuls = sum(Nuls), 
            Exprimes = sum(Exprimes))

reg <- dep %>% 
  summarise(Inscrits = sum(Inscrits),
            Abstentions = sum(Abstentions),
            Votants = sum(Votants), 
            Blancs = sum(Blancs),
            Nuls = sum(Nuls), 
            Exprimes = sum(Exprimes))

reg_type <- reg %>%
  mutate(Inscrits = Inscrits/2348427*100,
         Abstentions = Abstentions/2348427*100,
         Votants = Votants/2348427*100, 
         Blancs = Blancs/2348427*100,
         Nuls = Nuls/2348427*100, 
         Exprimes = Exprimes/2348427*100) %>%
  gather(key = Type, value = ratio, 1:6) %>% 
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimes"))) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))


num_circ_nuances <- bret %>% 
  group_by(code_dpt, num_circ, Nuances) %>% 
  summarise(Voix = mean(Voix),
            p_inscrits = mean(p_inscrits),
            p_exprimes = mean(p_exprimes), 
            resultat = head(resultat, 1))


dep_nuances <- num_circ_nuances %>% 
  group_by(code_dpt, Nuances) %>% 
  summarise(Voix = mean(Voix),
            p_inscrits = mean(p_inscrits),
            p_exprimes = mean(p_exprimes))

reg_nuances <- dep_nuances %>% 
  group_by(Nuances) %>% 
  summarise(Voix = mean(Voix),
            p_inscrits = mean(p_inscrits),
            p_exprimes = round(mean(p_exprimes), 2))


# Anatomie Region ====

reg_type %>%
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  geom_label(aes(label = pourc),size = 5) +
  #coord_flip(expand = F) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des législatives 2017 en Bretagne", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Nuances Region ====

reg_nuances %>%
  ggplot(aes(reorder(Nuances, p_exprimes), p_exprimes)) +
  geom_bar(stat = "identity", fill = databzh$colour2) + 
  geom_label(aes(label = p_exprimes), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Second tour des législatives 2017 en Bretagne", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Anatomie Dep

dep_type <- dep %>%
  mutate(Abstentions = Abstentions/Inscrits*100,
         Votants = Votants/Inscrits*100, 
         Blancs = Blancs/Inscrits*100,
         Nuls = Nuls/Inscrits*100, 
         Exprimes = Exprimes/Inscrits*100, 
         Inscrits = Inscrits/Inscrits*100) %>%
  gather(key = Type, value = ratio, 2:7) %>% 
  mutate(Type = factor(Type, levels = c("Inscrits", "Abstentions", 
                                        "Votants","Blancs","Nuls", 
                                        "Exprimes"))) %>%
  mutate(pourc = paste0(round(ratio, 2), " %"))

dep_type %>%
  ggplot(aes(Type, ratio, fill = code_dpt)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  #geom_label(aes(label = pourc),size = 5) +
  scale_fill_manual(values = databzh$colours, name = "Département") +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des législatives 2017 par département", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

dep_nuances %>%
  ggplot(aes(reorder(Nuances, p_exprimes), p_exprimes, fill = code_dpt)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = databzh$colours, name = "Département") +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Premier tour des législatives 2017 en Bretagne", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Anato Cotes armor ====

dep_type %>%
  filter(code_dpt == "22") %>% 
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour3) + 
  geom_label(aes(label = pourc),size = 5) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des législatives 2017 dans les Côtes d'Armor",
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Nuances Cotes Armor ====

dep_nuances %>%
  filter(code_dpt == "22") %>% 
  ggplot(aes(reorder(Nuances, p_exprimes), p_exprimes)) +
  geom_bar(stat = "identity", fill = databzh$colour3) + 
  geom_label(aes(label = p_exprimes), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Second tour des législatives 2017 dans les Côtes d'Armor", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Anato Finistere ====

dep_type %>%
  filter(code_dpt == "29") %>% 
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour4) + 
  geom_label(aes(label = pourc),size = 5) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des législatives 2017 dans le Finistère",
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Nuances Finistere ====

dep_nuances %>%
  filter(code_dpt == "29") %>% 
  ggplot(aes(reorder(Nuances, p_exprimes), p_exprimes)) +
  geom_bar(stat = "identity", fill = databzh$colour4) + 
  geom_label(aes(label = round(p_exprimes, 2)), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Second tour des législatives 2017 dans le Finistère", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Anato Ille et Vilaine ====

dep_type %>%
  filter(code_dpt == "35") %>% 
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour5) + 
  geom_label(aes(label = pourc),size = 5) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des législatives 2017 en Ille-et-Vilaine",
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Nuances Ille et Vilaine ====

dep_nuances %>%
  filter(code_dpt == "35") %>% 
  ggplot(aes(reorder(Nuances, p_exprimes), p_exprimes)) +
  geom_bar(stat = "identity", fill = databzh$colour5) + 
  geom_label(aes(label = round(p_exprimes, 2)), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Second tour des législatives 2017 en Ille-et-Vilaine", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Anato Morbihan ====

dep_type %>%
  filter(code_dpt == "56") %>% 
  ggplot(aes(Type, ratio)) + 
  geom_bar(stat = "identity", fill = databzh$colour6) + 
  geom_label(aes(label = pourc),size = 5) +
  labs(x = "", 
       y = "", 
       title = "Anatomie du second tour des législatives 2017 dans le Morbihan",
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()

# Nuances Morbihan ====

dep_nuances %>%
  filter(code_dpt == "56") %>% 
  ggplot(aes(reorder(Nuances, p_exprimes), p_exprimes)) +
  geom_bar(stat = "identity", fill = databzh$colour6) + 
  geom_label(aes(label = round(p_exprimes,2)), size = 5) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Second tour des législatives 2017 dans le Morbiah", 
       subtitle = "Données via github.com/ThinkRstat/legislatives2017",
       caption = "http://data-bzh.fr") +
  databzhTheme()
