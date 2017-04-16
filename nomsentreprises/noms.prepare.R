library(data.table)

load(file = "../Sirene/data/naf.RData")

system.time(
  noms <- fread(
    "data/noms.csv",
    header = T,
    sep = ";",
    encoding = "UTF-8",
    data.table = F
  )
)
names(noms) <- tolower(names(noms))

# Sous-ensemble Bretagne ----
noms.bzh <- noms[noms$depet %in% c("22", "29", "35", "56"),]
noms.bzh$depet <- as.factor(as.character(noms.bzh$depet))

# Nom normalisé ----
# l_normalisee = l1_normalisee + " " + l2_normalisee si l2_normalisee est renseigné et qu'il n'est pas présent dans l1_normalisee.
noms.bzh$l_normalisee <- sapply(
  1:nrow(noms.bzh),
  FUN = function(x) {
    nom.bzh <- noms.bzh[x,]
    if (nom.bzh$l2_normalisee == ""
       | grepl(nom.bzh$l2_normalisee, nom.bzh$l1_normalisee, ignore.case = T)) {
      return(nom.bzh$l1_normalisee)
    }
    else {
      return(paste0(nom.bzh$l1_normalisee, " ", nom.bzh$l2_normalisee))
    }
  }
)

noms.bzh$l1_normalisee <- NULL
noms.bzh$l2_normalisee <- NULL

# NAF ----
noms.bzh$naf.niv1.et <- naf[match(noms.bzh$apet700, gsub("\\.", "", naf$niv5)), ]$niv1
noms.bzh$naf.niv1.et <- as.factor(noms.bzh$naf.niv1.et)
noms.bzh$apet700 <- NULL

# Géocoding Bretagne ----
#source("../../Rgeo/Rgeo.R", chdir = T)
#noms.bzh[, c("lon", "lat")] <- get_coordinates(noms.bzh$codpos, strict = F)

# Sauvegarde ----
save(noms.bzh, file = "data/noms.RData")

# Purge ----
rm(list = ls())
gc()
