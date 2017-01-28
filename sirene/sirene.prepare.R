library(data.table)

load(file = "data/naf.RData")

col.classes <- c(rep("character", 2), # siren, nic
                 rep("factor", 5),
                 "character", # libcom
                 rep("factor", 6),
                 rep("character", 2), # dcret, date_deb_etat_adm_et
                 rep("factor", 9),
                 "character", # depcomen
                 rep("factor", 6),
                 "character" #dcren
              )

system.time(
  sirene <- fread(
    "data/sirene_2017.csv",
    #"data/sirene_1000.csv",
    header = T,
    sep = ";",
    encoding = "UTF-8",
    data.table = F,
    stringsAsFactors = T
  )
)
names(sirene) <- tolower(names(sirene))

sirene$siren <- as.character(sirene$siren)
sirene$nic <- as.character(sirene$nic)
sirene$libcom <- as.character(sirene$libcom)
sirene$dcret <- as.character(sirene$dcret)
sirene$date_deb_etat_adm_et <- as.character(sirene$date_deb_etat_adm_et)
sirene$depcomen <- as.character(sirene$depcomen)
sirene$dcren <- as.character(sirene$dcren)

# Effectif ----
effectif.names <- c(
  "0", "1-2", "3-5", "6-9",
  "10-19", "20-49", "50-99",
  "100-199", "200-249", "250-499", "500-999",
  "1 000-1 999", "2 000-4 999", "5 000-9 999",
  "10 000+"
)

## Suppression du niveau NN (unité non employeuse)
sirene[sirene$tefen == "NN",]$tefen <- "00"
sirene$tefen <- factor(sirene$tefen)
levels(sirene$tefen) <- effectif.names

sirene[sirene$tefet == "NN",]$tefet <- "00"
sirene$tefet <- factor(sirene$tefet)
levels(sirene$tefet) <- effectif.names

.get_effectif_count <- function(effectif) {
  count <- c(
    0, mean(c(1, 2)), mean(c(3, 5)), mean(c(6, 9)),
    mean(c(10, 19)), mean(c(20, 49)), mean(c(50, 99)),
    mean(c(100, 199)), mean(c(200, 249)), mean(c(250, 499)), mean(c(500, 999)),
    mean(c(1000, 1999)), mean(c(2000, 4999)), mean(c(5000, 9999)),
    10000
  )
  
  return(count[as.integer(effectif)])
}

sirene$tefet.count <- .get_effectif_count(sirene$tefet)

# Catégorie ----

#' Détermine la catégorie d'un établissement en fonction de l'effectif.
#' PME = moins de 250 salariés.
#' ETI = entre 250 et 4999 salariés.
#' GE = au moins 5000 salariés.
#' Ne prend pas en compte le chiffre d'affaires.
#'
#' @param categorie catégorie de l'établissement
#' @param effectif effectif de l'établissement
#' @return catégorie de l'établissement (inchangée si renseignée en entrée).
#' @examples
#' .guess_categorie("", "02")
.guess_categorie <- Vectorize(
  function(categorie, effectif) {
    if (categorie != "")
      c <- as.character(categorie)
    else if (effectif %in% c("NN", "00", "01", "02", "03", "11", "12", "21", "22", "31"))
      c <- "PME"
    else if (effectif %in% c("32", "41", "42", "51"))
      c <- "ETI"
    else if (effectif %in% c("52", "53"))
      c <- "GE"
    else
      c <- ""
    
    return(c)
  }
)

sirene$categorie <- factor(
  .guess_categorie(sirene$categorie, sirene$tefet),
  c("PME", "ETI", "GE")
)

# NAF ----
sirene$naf.niv1.et <- naf[match(sirene$apet700, gsub("\\.", "", naf$niv5)), ]$niv1
sirene$naf.niv1.et <- as.factor(sirene$naf.niv1.et)

sirene$naf.niv1.en <- naf[match(sirene$apen700, gsub("\\.", "", naf$niv5)), ]$niv1
sirene$naf.niv1.en <- as.factor(sirene$naf.niv1.en)

# Sous-ensemble Bretagne ----
sirene.bzh <- sirene[sirene$depet %in% c("22", "29", "35", "56"),]
sirene.bzh$depet <- as.factor(as.character(sirene.bzh$depet))

etab.fr <- nrow(sirene)
ent.fr <- length(unique(sirene$siren))

# Géocoding Bretagne ----
source("../../Rgeo/Rgeo.R", chdir = T)
sirene.bzh[, c("lon", "lat")] <- get_coordinates(sirene.bzh$codpos, strict = F)

# Sauvegarde ----
save(sirene, file = "data/sirene_2017.RData")
save(sirene.bzh, etab.fr, ent.fr, file = "data/sirene_bzh_2017.RData")

# Purge ----
rm(list = ls())
gc()
