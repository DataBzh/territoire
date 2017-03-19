source("Utils.R")

gf <- read.csv2("data/parrainagestotal.csv", fileEncoding = "UTF-8", stringsAsFactors = F)
names(gf) <- c("civilite", "nom", "prenom", "mandat", "comm_cir", "dept_coll", "candidat", "publication")

# Nom de famille
gf$candidat.nom <- factor(gf$candidat)
levels(gf$candidat.nom) <- .capitalize(
  unlist(
    lapply(
      strsplit(sort(unique(gf$candidat)), " "),
      FUN = function(l) {
        paste(l[-length(l)], collapse = " ")
      }
    )
  )
)

# Initiales
gf$candidat.initiales <- factor(gf$candidat)
levels(gf$candidat.initiales) <- .initials(
  levels(gf$candidat.initiales)
)

gf$candidat2 <- factor(paste0(gf$candidat, " (", gf$candidat.initiales, ")"))
gf$candidat <- factor(gf$candidat)
gf$civilite <- factor(gf$civilite)
gf$mandat <- factor(gf$mandat)
gf$dept_coll <- factor(gf$dept_coll)

# Sous-ensemble Bretagne ----
gf.bzh <- gf[gf$dept_coll %in% c("Finistère", "Côtes-d'Armor", "Morbihan", "Ille-et-Vilaine"),]
gf.bzh$candidat <- factor(gf.bzh$candidat)
gf.bzh$candidat2 <- factor(gf.bzh$candidat2)
gf.bzh$candidat.nom <- factor(gf.bzh$candidat.nom)
gf.bzh$candidat.initiales <- factor(gf.bzh$candidat.initiales)
gf.bzh$mandat <- factor(gf.bzh$mandat)
gf.bzh$dept_coll <- factor(gf.bzh$dept_coll)

# Sauvegarde ----
save(gf, file = "data/parrainages.RData")
save(gf.bzh, file = "data/parrainages_bzh.RData")

# Nettoyage ----
rm(list = ls())

