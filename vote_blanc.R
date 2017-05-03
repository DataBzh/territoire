library(tidyverse)
setwd("~/R/PRESIDENTIELLES_1965-2012-csv")
source("/home/colin/R/misc/data-bzh-tools-master/main.R")
# ELEC ====

l <- list.files(pattern = ".*\\csv")
elec <- tibble()
nom <- c("code_dep","départ","circ","inscrits",
         "votants","exprimes","blancs_nuls","election")
for(i in seq_along(l)){
  df <- read_csv(l[i]) %>%
    .[,1:7] %>%
    mutate(election = l[i])
  names(df) <- nom
  elec <- rbind(elec, df)
}
glimpse(elec)

elec_clean <- elec %>%
  mutate(election = gsub("cdsp_presi", "", .$election)) %>%
  mutate(election = gsub("_circ.csv", "", .$election)) %>%
  separate(election, into = c("an","tour"), sep = "t") %>%
  group_by(code_dep, an,tour) %>%
  summarise(votants_tot = sum(votants), 
            blancs_nuls_tot = sum(blancs_nuls), 
            pourc = blancs_nuls_tot / votants_tot * 100)


elec_bret <- elec_clean %>%
  filter(code_dep == 22 | code_dep == 29 | code_dep == 35 | code_dep == 56)
elec_france <- anti_join(elec_clean, elec_bret)

# 2017 ====

deuxdixsept <- read_csv("https://github.com/datactivist/presidentielle2017/raw/master/Presidentielle_2017_Resultats_BV_T1_clean_def.csv")

deuxdixsept_clean <- deuxdixsept[,c("CodeDepartement", "Département","CodeCirco", 
                                   "Votants", "Exprimés", "Blancs","Nuls")] %>%
  mutate(an = "2017", 
         tour = "1", 
         blancs_nuls = Blancs + Nuls) %>%
  .[,c("CodeDepartement", "Département","CodeCirco", 
       "Votants", "Exprimés", "blancs_nuls","an","tour")] %>%
  mutate(Département = toupper(Département)) %>%
  mutate(code_dep = CodeDepartement) %>%
  group_by(code_dep, an,tour) %>%
  summarise(votants_tot = sum(Votants), 
            blancs_nuls_tot = sum(blancs_nuls), 
            pourc = blancs_nuls_tot / votants_tot * 100)

deuxdixsept_bret <- deuxdixsept_clean %>%
  filter(code_dep == 22 | code_dep == 29 | code_dep == 35 | code_dep == 56)
deuxdixsept_france <- anti_join(deuxdixsept_clean, deuxdixsept_bret)

# Bretagne ====

full_bret_clean <- rbind(deuxdixsept_bret, elec_bret)

#Dans toute la Bretagne ====

full_bret_clean %>%
  group_by(an, tour) %>%
  summarise(votants_tot = sum(votants_tot), 
            blancs_nuls_tot = sum(blancs_nuls_tot), 
            pourc = blancs_nuls_tot / votants_tot * 100) %>%
  ggplot(aes(an, pourc, col = tour, group = tour)) + 
  geom_line(size = 2) + 
  scale_color_manual(values = databzh$colours, name = "Tour") +
  labs(x = "", 
       y = "", 
       title = "Pourcentage de votes blancs & nuls en Bretagne - 1965/2017", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

mean(full_bret_clean$pourc)
full_bret_clean %>%
  group_by(tour) %>%
  summarise(m = mean(pourc))
full_bret_clean %>%
  group_by(an, tour) %>%
  summarise(m = mean(pourc)) %>%
  arrange(desc(m)) %>%
  head()

#full_bret_clean %>%
  group_by(an) %>%
  summarise(votants_tot = sum(votants_tot), 
            blancs_nuls_tot = sum(blancs_nuls_tot), 
            pourc = blancs_nuls_tot / votants_tot * 100,
            s =diff(pourc)) %>%
  filter(an != 2017) %>%
  ggplot(aes(an, s)) + 
  geom_bar(stat = "identity", fill = databzh$colour3) +
  scale_fill_manual(values = databzh$colours[7:8]) +
  labs(x = "", 
       y = "", 
       title = "Différences de pourcentage entre les deux tours", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


full_bret_clean %>%
  group_by(an, tour) %>%
  summarise(votants_tot = sum(votants_tot), 
            blancs_nuls_tot = sum(blancs_nuls_tot), 
            pourc = blancs_nuls_tot / votants_tot * 100) %>%
  ggplot(aes(an, votants_tot, col = tour, group = tour)) + 
  geom_line(size = 2) + 
  scale_color_manual(values = databzh$colours, name = "Tour") +
  labs(x = "", 
       y = "", 
       title = "Nombre de votants en Bretagne - 1965/2017", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


#Dans toute la Bretagne comparé à la france ====

full_elec <- rbind(deuxdixsept_france, elec_france) %>%
  mutate(scope = "France (hors Bretagne)") 

full_bret <- rbind(deuxdixsept_bret, elec_bret) %>%
  mutate(scope = "Bretagne")

full_full <- rbind(full_bret, full_elec) %>%
  na.omit() %>%
  group_by(an, tour, scope) %>%
  summarise(votants_tot = sum(votants_tot), 
            blancs_nuls_tot = sum(blancs_nuls_tot), 
            pourc = blancs_nuls_tot / votants_tot * 100)


ggplot(full_full, aes(an, pourc, col = scope, group = scope)) + 
  geom_line(size = 2) + 
  scale_color_manual(values = databzh$colours, name = "Périmètre") +
  facet_grid(tour~.) +
  labs(x = "", 
       y = "", 
       title = "Évolution du vote blanc et nul en France - 1965/2017", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

full_full_2 <- rbind(full_elec, full_bret)

ggplot(filter(full_full_2, scope != "Bretage"), aes(an, pourc, col = scope)) + 
  geom_point(size = 4) + 
  scale_color_manual(values = databzh$colours[4:5], name = "Périmètre") +
  facet_grid(~tour) +
  labs(x = "", 
       y = "", 
       title = "Votes blancs et nuls par département, France - 1965/2017", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()

#Par département

ggplot(full_bret_clean, aes(an, pourc, col = code_dep, group = code_dep)) + 
  geom_line(size =1) + 
  scale_color_manual(values = databzh$colours, name = "Département") +
  facet_grid(tour ~ ., scales = "free_y") +
  labs(x = "", 
       y = "", 
       title = "Votes blancs et nuls par département - 1965/2017", 
       subtitle = "Données via data.gouv",
       caption = "http://data-bzh.fr") +
  databzhTheme()


  