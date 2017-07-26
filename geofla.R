library(tidyverse)
library(magrittr)
library(tidytext)
library(ggrepel)
source("/home/colin/R/misc/data-bzh-tools-master/main.R")
browseURL("https://inspire.data.gouv.fr/datasets/7d1bed3c00184f9cfd8b7f985cb68ff82797fa18")
pop <- read_csv2("https://datanova.legroupe.laposte.fr/explore/dataset/geoflar-communes-2015/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
pop %<>% filter(NOM_REG == "BRETAGNE")

# Communes par département 

pop %>% 
  ggplot(aes(x = NOM_DEPT, fill = NOM_DEPT)) + 
  geom_bar() +
  coord_flip() + 
  labs(title = "Communes par département", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()

pop %>%
  group_by(NOM_DEPT) %>%
  summarize(n())

pop %>% 
  ggplot(aes(x = STATUT, fill = NOM_DEPT)) + 
  geom_bar(position = "dodge") +
  coord_flip() + 
  labs(title = "Typologie des communes par département", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()


# Superficies par communes 

pop %>% 
  ggplot(aes(x = SUPERFICIE, fill = NOM_DEPT)) + 
  geom_histogram() +
  facet_grid(~ NOM_DEPT) + 
  labs(title = "Superficies des communes bretonnes", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()

pop %>%
  group_by(NOM_DEPT) %>%
  summarize(Superficie_moyenne = mean(SUPERFICIE), 
            Superficie_médiane = median(SUPERFICIE), 
            Ecart_type = sd(SUPERFICIE)) %>%
  knitr::kable()

pop %>%
  group_by(NOM_DEPT) %>%
  summarize(Superficie_moyenne = mean(SUPERFICIE)) %>%
  ggplot(aes(x = NOM_DEPT, y = Superficie_moyenne, fill = NOM_DEPT)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Superficie moyenne par département", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()

pop %>%
  arrange(desc(SUPERFICIE)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(NOM_COM, SUPERFICIE), y = SUPERFICIE, fill = NOM_DEPT)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "15 villes les plus grandes de Bretagne", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, name = "Département") + 
  databzhTheme()

pop %>%
  arrange(desc(SUPERFICIE)) %>%
  slice(1:15) %>%
  select(NOM_COM, SUPERFICIE) %>%
  knitr::kable()

# Population par communes 

pop %>% 
  ggplot(aes(x = POPULATION, fill = NOM_DEPT)) + 
  geom_histogram() +
  facet_grid(~ NOM_DEPT) + 
  labs(title = "Population des communes bretonnes", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()

pop %>%
  group_by(NOM_DEPT) %>%
  summarize(Population_moyenne = mean(POPULATION), 
            Population_médiane = median(POPULATION), 
            Ecart_type = sd(POPULATION)) %>%
  knitr::kable()

pop %>%
  group_by(NOM_DEPT) %>%
  summarize(Population_moyenne = mean(POPULATION)) %>%
  ggplot(aes(x = NOM_DEPT, y = Population_moyenne, fill = NOM_DEPT)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Population moyenne par commune", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()

pop %>%
  arrange(desc(POPULATION)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(NOM_COM, POPULATION), y = POPULATION, fill = NOM_DEPT)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "15 villes les plus peuplées de Bretagne", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, name = "Département") + 
  databzhTheme()

pop %>%
  arrange(desc(POPULATION)) %>%
  slice(1:15) %>%
  select(NOM_COM, POPULATION) %>%
  knitr::kable()

# Noms de communes 

tibble(text = pop$NOM_COM) %>%
  unnest_tokens(output = "word", input = "text") %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% stopwords::stopwords_iso$fr) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = databzh$colour1) + 
  coord_flip() + 
  labs(title = "15 mots les plus récurrents dans les noms de commune", 
       subtitle = "Données Geofla", 
       caption = "http://data-bhz.fr", 
       x = "", 
       y = "") + 
  scale_fill_manual(values = databzh$colours, position = NULL) + 
  guides(fill = FALSE) +
  databzhTheme()

tibble(text = pop$NOM_COM) %>%
  unnest_tokens(output = "word", input = "text") %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% stopwords::stopwords_iso$fr) %>%
  slice(1:15) %>%
  knitr::kable()
