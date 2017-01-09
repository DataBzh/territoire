source("gendeR.R")

# Référentiel ----
tranche.age <- data.frame(
  code = c("0-14", "15-29", "30-44", "45-59", "60-74", "75+"),
  min = c(0, 15, 30, 45, 60, 75),
  max = c(14, 29, 44, 59, 74, 999),
  stringsAsFactors = F
)

csp <- list(
  list(
    code = "cs1",
    nom = "Agriculteurs exploitants",
    pattern = c("agriculteur")
  ),
  list(
    code = "cs2",
    nom = "Artisans, Commerçants, Chefs d'entreprise",
    pattern = c("artisan", "commerçant", "chef entreprise", "administrateur")
  ),
  list(
    code = "cs3",
    nom = "Cadres et Professions intellectuelles supérieures",
    pattern = c("directeur", "directrice", "cadre", "ingénieur", "médecin", "catégorie A")
  ),
  list(
    code = "cs4",
    nom = "Professions intermédiaires",
    pattern = c("conseiller", "enseignant", "enseignement", "professeur", "catégorie B", "représentant", "social", "industriel")
  ),
  list(
    code = "cs5",
    nom = "Employés",
    pattern = c("employé", "assistant", "catégorie C")
  ),
  list(
    code = "cs6",
    nom = "Ouvriers",
    pattern = c("ouvrier")
  ),
  list(
    code = "cs7",
    nom = "Retraités",
    pattern = c("retraité")
  ),
  list(
    code = "cs8",
    nom = "Autres sans activité professionnelle",
    pattern = c("*")
  )
)

getCSP <- function(profession) {
  .getCSP <- function(prof) {
    for (i in 1:length(csp)) {
      n <- sapply(csp[[i]]$pattern, function(pattern) { length(grep(pattern, prof, ignore.case = T)) })
      if (sum(n) > 0) {
        return(csp[[i]]$code)
      }
    }
  }
  
  return(sapply(profession, .getCSP))
}

# Chargement ----
## Données de recensement ----
## Source : https://www.insee.fr/fr/statistiques/2386737
recens <- read.delim("data/base-ic-evol-struct-pop-2013.tsv", as.is = T)
names(recens) <- tolower(names(recens))

recens35 <- recens[recens$dep == "35",]
comment(recens35) <- "Recensement Ille & Vilaine en 2013"

recens35.agg <- as.data.frame(
  lapply(
    recens35[,c(# Population totale
      "p13_pop",
      # Nombre d'hommes/femmes
      "p13_poph", "p13_popf",
      # Nombre d'hommes/femmes de 15 ans ou plus
      "c13_h15p", "c13_f15p", 
      # Hommes par tranches d'âges
      "p13_h0014", "p13_h1529", "p13_h3044", "p13_h4559", "p13_h6074", "p13_h75p",
      # Femmes par tranches d'âges
      "p13_f0014", "p13_f1529", "p13_f3044", "p13_f4559", "p13_f6074", "p13_f75p",
      # Hommes par CSP
      paste0("c13_h15p_cs", 1:8),
      # Femmes par CSP
      paste0("c13_f15p_cs", 1:8)
    )],
    sum
  )
)

unpivot <- function(df, sexe, prefix.length) {
  df <- as.data.frame(t(df))
  df$code <- substring(row.names(df), prefix.length)
  df$sexe <- sexe
  names(df)[1] <- "count"
  row.names(df) <- NULL
  
  return(df)
}

# Hommes/femmes par tranches d'âges
recens35.age <- rbind(
  unpivot(recens35.agg[,6:11], "homme", 6),
  unpivot(recens35.agg[,12:17], "femme", 6)
)
# Hommes/femmes par CSP
recens35.csp <- rbind(
  unpivot(recens35.agg[,18:25], "homme", 10),
  unpivot(recens35.agg[,26:33], "femme", 10)
)

## Données élus ----
## Source : http://www.illeetvilaine.fr/fr/elus
elus35 <- read.delim("data/elus.tsv", as.is = T, fileEncoding = "UTF-8")
comment(elus35) <- "Elus départementaux d'Ille & Vilaine en 2016"

elus35$etiquette <- factor(elus35$etiquette)
levels(elus35$etiquette)[levels(elus35$etiquette) == ""] <- "Sans étiquette"
elus35$prenom <- sapply(strsplit(elus35$nom, " "), function(l) { l[1] })
elus35$nom <- substring(elus35$nom, nchar(elus35$prenom) + 2)
elus35$sexe <- factor(getGender(elus35$prenom, verbose = F))
levels(elus35$sexe) <- c("femme", "homme")
elus35$tranche.age <-
  factor(
    sapply(elus35$age, function(age) { tranche.age[age >= tranche.age$min & age <= tranche.age$max,]$code }),
    levels = tranche.age$code
  )
elus35$csp <- factor(getCSP(elus35$profession))
elus35$mandat_count <- 1 + ifelse(elus35$mandat_1 == "", 0, 1) + ifelse(elus35$mandat_2 == "", 0, 1)

# Sauvegarde ----
save(elus35,
     recens35,
     recens35.agg,
     recens35.age,
     recens35.csp,
     csp,
     tranche.age,
     file = "elus35.RData"
)
