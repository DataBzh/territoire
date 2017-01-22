##Chargement et tri base 

```{r}
source("data-bzh-tools-master/main.R")
library(tidyverse)
name <- read.table("/home/colin/Téléchargements/dpt2015.txt", stringsAsFactors = FALSE, sep = "\t", col.names = c("sexe","preusuel","annais","dpt","nombre"), encoding = "latin1")
name <- name[-1,]
name$nombre <- as.numeric(name$nombre)
name$annais <- as.Date(name$annais, "%Y")
bret <- name %>% 
  filter(grepl(("22|29|35|56"), dpt))
```


##Toute la Bretagne

```{r}
full_bret <- bret %>%
  group_by(preusuel) %>%
  summarize(somme = sum(nombre)) %>%
  arrange(desc(somme))

mean(full_bret$somme)
median(full_bret$somme)
sd(full_bret$somme)

#Attribution des prénoms 
ggplot(full_bret, aes(somme)) +
  geom_freqpoly(bins= 50, col = databzh$colour1, size = 2) +
  xlab("Volume d'attributions du prénom") +
  ylab("Volume de prénoms") +
  labs(title = "Attributions des prénoms, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```


###5 prénoms les plus donnés

```{r}
five <- bret %>%
  filter(grepl("^MARIE$|^JEAN$|^PIERRE$|^ANNE$|^JOSEPH$", preusuel)) %>%
  group_by(preusuel, annais) %>%
  summarise(somme = sum(nombre))

ggplot(five, aes(annais, somme, group = preusuel, col = preusuel)) +
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours, guide = guide_legend(title = "Prénoms")) +
  xlab("") +
  ylab(" ") +
  labs(title = "5 prénoms les plus donnés en Bretagne, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```

###Nombre de prénoms par an 

```{r}
name_year <- bret %>%
  group_by(annais) %>%
  summarize(somme = sum(n()))

ggplot(name_year, aes(annais,somme)) +
  geom_bar(stat = "identity", fill = databzh$colour2) +
  xlab("") +
  ylab("Nombre de prénoms") +
  labs(title = "Nombre de prénoms par an, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```

###Taille des prénoms

```{r}
bret$taille <- nchar(bret$preusuel)
  

taille <- bret %>%
  filter(preusuel != "_PRENOMS_RARES") %>%
  group_by(taille, annais) %>%
  summarize(somme = sum(nombre))
taille <- taille[-c(1434), ]

taille %>%
  filter(taille <6) %>%
  ggplot(aes(annais,somme, color = as.factor(taille))) +
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours, guide = guide_legend(title = "Nombre de caractères")) +
  xlab("") +
  ylab("") +
  labs(title = "Occurrences des prénoms de 5 caractères et moins, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

taille %>%
  filter(taille >= 12) %>%
  ggplot(aes(annais,somme, color = as.factor(taille))) +
  geom_line(size = 2) +
  scale_color_manual(values = databzh$colours, guide = guide_legend(title = "Nombre de caractères")) +
  xlab("") +
  ylab("") +
  labs(title = "Occurrences des prénoms de 12 caractères et plus, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

```

###Prénoms composés

```{r}
compo <- bret %>%
  filter(grepl(("-"), preusuel)) %>%
  group_by(annais) %>%
  summarize(somme = sum(nombre)) 

ggplot(compo, aes(annais,somme)) +
  geom_bar(stat = "identity", fill = databzh$colour4) +
  xlab("") +
  ylab("") +
  labs(title = "Nombre de prénoms composés par an, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```


##Prénoms Rares

```{r}
rare <- bret %>%
  filter(grepl(("_PRENOMS_RARES"), preusuel)) %>%
  group_by(annais) %>%
  summarize(somme = sum(nombre)) 
rare <- rare[-117,]

ggplot(rare, aes(annais,somme)) +
  geom_bar(stat = "identity", fill = databzh$colour5) +
  scale_fill_manual(values = databzh$colours) +
  xlab("") +
  ylab("") +
  labs(title = "Nombre de prénoms rares par an, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()

rare2 <- bret %>%
  filter(grepl(("_PRENOMS_RARES"), preusuel)) %>%
  group_by(annais, dpt) %>%
  summarize(somme = sum(nombre)) 
rare <- rare[-117,]

ggplot(rare2, aes(annais,somme, fill = dpt)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = databzh$colours) +
  xlab("") +
  ylab("") +
  labs(title = "Nombre de prénoms rares par an, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```

##Hommes

```{r}
men <- name %>%
  filter(sexe == 1) %>%
  group_by(preusuel) %>%
  summarize(somme = sum(nombre)) %>%
  arrange(desc(somme)) %>%
  head()

mean(men$somme)
median(men$somme)
sd(men$somme)

ggplot(men[1:10,], aes(reorder(preusuel, somme), somme)) +
  geom_bar(stat = "identity", fill = databzh$colour3) +
  geom_text(data = men[1:10,], aes(label= somme), size = 5) + 
  coord_flip() + 
  xlab("") +
  ylab(" ") +
  labs(title = "Prénoms masculins les plus donnés en Bretagne, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```

##Femmes

```{r}
femmes <- bret %>%
  filter(sexe == 2) %>%
  group_by(preusuel) %>%
  summarize(somme = sum(nombre)) %>%
  arrange(desc(somme))

mean(femmes$somme)
median(femmes$somme)
sd(femmes$somme)

ggplot(femmes[1:10,], aes(reorder(preusuel, somme), somme)) +
  geom_bar(stat = "identity", fill = databzh$colour6) +
  geom_text(data = femmes[1:10,], aes(label= somme), size = 5) + 
  coord_flip() + 
  xlab("") +
  ylab(" ") +
  labs(title = "Prénoms féminins les plus donnés en Bretagne, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```

#Naissance par année

```{r}
year <- bret %>% 
  group_by(annais, sexe) %>%
  summarize(somme = sum(nombre))
year$annais <- as.Date(year$annais, "%Y")

ggplot(year, aes(annais, somme, group = sexe, col = sexe)) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours[10:11], labels = c("Homme", "Femme")) +
  xlab("") +
  ylab(" ") +
  labs(title = "Naissances en Bretagne, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```

```{r}
dep <- bret %>% 
  group_by(annais, dpt) %>%
  summarize(somme = sum(nombre))

ggplot(dep, aes(annais, somme, group = as.factor(dpt), col = as.factor(dpt))) +
  geom_line(size = 1) +
  scale_color_manual(values = databzh$colours, guide = guide_legend(title = "Département")) +
  xlab("") +
  ylab(" ") +
  labs(title = "Naissances en Bretagne par département, 1900-2015", 
       caption = "http://data-bzh.fr") +
  databzhTheme()
```
