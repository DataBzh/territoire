library(tidyverse)
library(magrittr)
source("data-bzh-tools-master/main.R")
st_malo <- read_csv2("https://data.stmalo-agglomeration.fr/explore/dataset/certificats-deconomies-denergie/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
glimpse(st_malo)
st_malo %<>% mutate(`Date début` = lubridate::ymd(`Date début`), 
                    `Date fin` = lubridate::ymd(`Date fin`))


st_malo %>% 
  group_by(Année) %>%
  count() %>%
  rename(Volume = n) %>%
  knitr::kable()

st_malo %>% 
  group_by(Année) %>%
  count() %>%
  rename(Volume = n) %>%
  ggplot() + 
  aes(Année, Volume) + 
  geom_col(fill = databzh$colour1) +
  labs(title = "CEE à Saint-Malo par an", 
       subtitle = "données via Saint Malo Open Data", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

st_malo %>% 
  group_by(`Type d'opérations`) %>%
  count(sort = TRUE) %>%
  rename(Volume = n) %>%
  head(5) %>%
  knitr::kable()

library(tidytext)
library(proustr)
st_malo %>%
  unnest_tokens(Type,`Type d'opérations`, lang = "french") %>%
  select(Type) %>%
  anti_join(proust_stopwords(), by = c(Type = "word")) %>%
  pr_stem_words(Type) %>%
  count(Type, sort = TRUE) %>%
  slice(1:10) %>%
  ggplot() + 
  aes(reorder(Type, n), n) + 
  geom_col(fill = databzh$colour2) +
  coord_flip() +
  labs(title = "Types de travaux de CEE à Saint-Malo", 
       subtitle = "données via Saint Malo Open Data", 
       caption = "http://data-bzh.fr", 
       x = "Type", 
       y = "Volume") + 
  databzhTheme()

st_malo %>% 
  group_by(Année) %>%
  summarize(`kWh moyen` = mean(`kWh cumac`)) %>%
  knitr::kable()

st_malo %>% 
  group_by(Année) %>%
  summarize(`kWh moyen` = mean(`kWh cumac`))%>%
  ggplot() + 
  aes(Année, `kWh moyen`) + 
  geom_col(fill = databzh$colour3) +
  labs(title = "kWh des CEE à Saint-Malo par an", 
       subtitle = "données via Saint Malo Open Data", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

st_malo %>% 
  count(Adresse, sort = TRUE) %>%
  top_n(5) %>%
  ggplot() + 
  aes(reorder(Adresse,n),n) + 
  geom_col(fill = databzh$colour4) +
  coord_flip() +
  labs(title = "Adresses des CEE à Saint-Malo par an", 
       subtitle = "données via Saint Malo Open Data", 
       caption = "http://data-bzh.fr", 
       x = "Adresse", 
       y = "volume") + 
  databzhTheme()


