source("https://raw.githubusercontent.com/DataBzh/data-bzh-tools/master/theme.R")

# Institutions culturelles productrices de ressources pédagogiques en ligne

library(tidyverse)
cult_fr <- read_csv2("https://data.culturecommunication.gouv.fr/explore/dataset/liste-des-institutions-culturelles-francaise-produisant-des-ressources-pedagogiq/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
bret <- cult_fr %>%
  filter(Région == "Bretagne") %>%
  mutate(Département = stringr::str_replace_all(.$`CP Ville`, "(..)...", "\\1"))

# Par département 
library(pander)
bret %>% group_by(Département) %>% count() %>% pander()

ggplot(bret, aes(Département)) + 
  geom_bar(fill = databzh$colour1) + 
  labs(title = "Établissements par département", 
       subtitle = "données via data.culturecommunication.gouv.fr", 
       caption = "http://data-bzh.fr", 
       y = "Effectif") + 
  databzhTheme()

# 5 villes les plus présentes 

bret %>%
  group_by(Ville) %>%
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(5) %>%
  ggplot(aes(reorder(Ville, n), n)) + 
  geom_col(fill = databzh$colour2) + 
  coord_flip() + 
  labs(title = "5 villes les plus représentées", 
       subtitle = "données via data.culturecommunication.gouv.fr", 
       caption = "http://data-bzh.fr", 
       y = "Effectif", 
       x = "") + 
  databzhTheme()

# Scraper les sites 

get_web_info <- function(url_base){
  Sys.sleep(0.5)
  default <-   data.frame(keywords = 404, 
                          title = 404, 
                          desc = 404,
                          h1 = 404,
                          url = url_base)
  if(httr::GET(url_base)$status_code != 200){
    return(default)
  }
  url <- read_html(url_base)
  keywords <- url %>%
    html_node("head") %>%
    html_nodes("meta[name=keywords]") %>%
    html_attr("content")
  title <- url %>%
    html_node("head") %>%
    html_nodes("meta[name=title]") %>%
    html_attr("content")
  if(length(title) == 0){
    title <- url %>%
      html_node("title") %>%
      html_text()
  }
  desc <- url %>%
    html_node("head") %>%
    html_nodes("meta[name=description]") %>%
    html_attr("content")
  h1 <- url %>%
    html_node("h1") %>%
    html_text()
  data.frame(keywords = ifelse(length(keywords) == 0, NA, keywords), 
             title = ifelse(length(title) == 0, NA, title), 
             desc = ifelse(length(desc) == 0, NA, desc),
             h1 = ifelse(length(h1) == 0, NA, desc),
             url = url_base, 
             stringsAsFactors = FALSE)
}

site <- map_df(.x = bret$site, ~tidytrycatch(get_web_info(.x)))
success <- site %>% filter(type == "success")
success <- do.call(rbind, success$value)
success %<>% 
  mutate(words = paste(success$keywords, success$title, success$desc, success$h1)) %>% 
  mutate(words = gsub("NA", "", .$words)) %>%
  mutate(words = gsub("404", "", .$words))

library(tidytext)
tidy_df <- unnest_tokens(success, words, words) %>%
  count(words) %>%
  anti_join(proustr::proust_stopwords()) %>%
  arrange(desc(n))
tidy_df %>%
  top_n(15) %>%
  ggplot(aes(reorder(words, n), n)) +
  geom_col(fill = databzh$colour3) + 
  coord_flip() +
  labs(title = "Web scraping des pages d'accueil", 
       subtitle = "données via data.culturecommunication.gouv.fr", 
       caption = "http://data-bzh.fr", 
       x = "", 
       y = "") + 
  databzhTheme()

get_p <- function(url_base){
  Sys.sleep(0.5)
  default <-   data.frame(text = 404,
                          url = url_base)
  if(httr::GET(url_base)$status_code != 200){
    return(default)
  }
  url <- read_html(url_base)
  p <- url %>%
    html_nodes("p") %>%
    html_text() %>%
    paste0()
  data.frame(text = p, 
             url = url_base, 
             stringsAsFactors = FALSE)
}

site <- map_df(.x = bret$site, ~tidytrycatch(get_p(.x)))
success <- site %>% filter(type == "success")
success <- do.call(rbind, success$value)
success %<>% 
  mutate(words = gsub("NA", "", .$text)) %>%
  mutate(words = gsub("404", "", .$text))

tidy_df <- unnest_tokens(success, words, words) %>%
  count(words) %>%
  anti_join(proustr::proust_stopwords()) %>%
  arrange(desc(n))
tidy_df %>%
  top_n(15) %>%
  ggplot(aes(reorder(words, n), n)) +
  geom_col(fill = databzh$colour4) + 
  coord_flip() +
  labs(title = "Web scraping des text des pages d'accueil", 
       subtitle = "données via data.culturecommunication.gouv.fr", 
       caption = "http://data-bzh.fr", 
       x = "", 
       y = "") + 
  databzhTheme()
