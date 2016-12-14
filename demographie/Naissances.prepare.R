# Chargement ----
library(foreign)
n <- read.dbf(file = "data/etatcivil2015_nais2015_dbase/nais2015.dbf", as.is = T)
comment(n) <- "Naissances en France en 2015"

# Recodage ----
# Sexe de l'enfant
n$sexe <- factor(n$sexe, labels = c("Garçon", "Fille"))
# Département de naissance
n$depnais <- factor(n$depnais)

# Année/trimestre/mois/saison de naissance de l'enfant
n$anais <- as.integer(n$anais)
n$mnais <- factor(n$mnais)
n$qnais <- n$mnais
levels(n$qnais) <- as.character(rep(1:4, each = 3))
n$snais <- n$qnais
levels(n$snais) <- c("hiver", "printemps", "été", "automne")

# Trimestre/mois/saison de conception de l'enfant
n$mconc <- factor(sprintf("%02d", unlist(Map(function(x) { ifelse(x > 9, x - 9, x + 3) }, as.integer(n$mnais)))))
n$qconc <- n$mconc
levels(n$qconc) <- as.character(rep(1:4, each = 3))
n$sconc <- n$qconc
levels(n$sconc) <- c("hiver", "printemps", "été", "automne")

# Année de mariage des parents
n$amar <- as.integer(n$amar)
n$amar[n$amar == 0] <-  NA

# Age de la mère dans l’année de naissance de l’enfant
n$agemere <- as.integer(n$agemere)

# Age du père dans l’année de naissance de l’enfant
n$agepere <- as.integer(n$agepere)

# Année/mois/jour de reconnaissance conjointe des parents
n$arecc <- as.integer(n$arecc)
n$arecc[n$arecc == 0] <- NA
n$mrecc <- factor(n$mrecc)
n$mrecc[n$mrecc == "00"] <- NA
n$jrecc <- as.integer(n$jrecc)
n$jrecc[n$jrecc == 0] <- NA

# Année/mois/jour de reconnaissance de la mère
n$arecm <- as.integer(n$arecm)
n$arecm[n$arecm == 0] <- NA
n$mrecm <- factor(n$mrecm)
n$mrecm[n$mrecm == "00"] <- NA
n$jrecm <- as.integer(n$jrecm)
n$jrecm[n$jrecm == 0] <- NA

# Année/mois/jour de reconnaissance du père
n$arecp <- as.integer(n$arecp)
n$arecp[n$arecp == 0] <- NA
n$mrecp <- factor(n$mrecp)
n$mrecp[n$mrecp == "00"] <- NA
n$jrecp <- as.integer(n$jrecp)
n$jrecp[n$jrecp == 0] <- NA

# Département de domicile de la mère
n$depdom <- factor(n$depdom)

# Nombre d’enfants issus de l’accouchement
n$nbenf <- as.integer(n$nbenf)

# Indicateur de nationalité de la mère et du père
n$indnatm <- factor(n$indnatm, labels = c("Français", "Etranger"))
n$indnatp <- factor(n$indnatp, labels = c("Français", "Etranger"))

# Indicateur du lieu de naissance de la mère et du père
n$indlnm <- factor(n$indlnm, labels = c("France métropolitaine", "DOM", "COM", "Etranger"))
n$indlnp <- factor(n$indlnp, labels = c("France métropolitaine", "DOM", "COM", "Etranger"))

# Variables inutilisées
n$accouchr <- NULL
n$agexactm <- NULL
n$agexactp <- NULL
n$originom <- NULL
n$situatmr <- NULL
n$situatpr <- NULL
n$tucom <- NULL
n$tudom <- NULL

# Filtrage sur la Bretagne ----
n.bzh <- n[n$depnais %in% c(22, 29, 35, 56),]
comment(n.bzh) <- "Naissances en Bretagne en 2015"

# Accouchements ----
# Traitement des accouchements à N enfants
.get.childbirth.err <- function(data, nbenf = 2) {
  df <- data[data$nbenf == nbenf,]
  keep.rows <- vector()
  err.rows <- vector()
  err.rownames <- vector()
  cibling.count <- 0
  
  # Traitement des naissances manquantes pour l'accouchement
  .add.childbirth.err <- function(i, cibling.count, last = F) {
    i.err <- i + ifelse(last, 1 , 0) - 1:cibling.count
    err.rows <<- append(err.rows, i.err)
    err.rownames <<- append(err.rownames, row.names(df[i.err,]))
  }
  
  .pick.first <- function(i) {
    first <- df[i,]
    cibling.count <<- 1
    keep.rows <<- append(keep.rows, i)
    
    return(first)
  }
  
  for (i in 1:nrow(df)) {
    if (cibling.count == 0) {
      first <- .pick.first(i)
    } else {
      twin <- df[i,]
      if (first$depnais != twin$depnais |
          first$mnais != twin$mnais |
          first$agemere != twin$agemere |
          first$agepere != twin$agepere) {
        .add.childbirth.err(i, cibling.count)
        # Rupture sur cette itération (anticipée)
        first <- .pick.first(i)
      } else {
        cibling.count <- cibling.count + 1
      }
      
      if (cibling.count == nbenf) {
        # Rupture à la prochaine itération
        cibling.count <- 0
      }
    }
  }
  
  if (cibling.count != 0) {
    .add.childbirth.err(i, cibling.count, T)
  }
  
  return(
    list(
      subset = df,
      keep.rows = keep.rows,
      err.rows = err.rows,
      err.rownames = err.rownames,
      nbenf = nbenf
    )
  )
}


# Sous-ensemble de vérification d'une ligne en erreur retournée par get.nbenf.err().
# Sont inclus les 'nbenf' précédents et suivants.
.get.check.childbirth <- function(err, rowname) {
  idx <- which(row.names(err$subset) == rowname)
  return(err$subset[(idx - err$nbenf):min(nrow(err$subset), idx + err$nbenf),])
}


# Affichage des sous-ensembles de vérification des lignes en erreur retournées par get.nbenf.err().
.view.check.childbirths <- function(err) {
  for (i in 1:length(err$err.rownames)) {
    View(
      .get.check.childbirth(err, err$err.rownames[i]),
      title = err$err.rownames[i]
    )
  }
}


# Construction d'un data frame ne contenant que les accouchements.
get.childbirth.data.frame <- function(df) {
  # Tri par nbenf, depnais, mnais, agemere, agepere.
  .prepare.childbirth.df <- function(df) {
    idx <- order(df$nbenf, df$depnais, df$mnais, df$agemere, df$agepere)
    df.ordered <- df[idx,]
    return(df.ordered)
  }
  
  df.w <- .prepare.childbirth.df(df)
  
  # Accouchements 1 enfant
  df.1 <- df[df$nbenf == 1,]
  
  # Accouchements 2 enfants
  err.2 <- .get.childbirth.err(df.w, 2)
  df.2 <- err.2$subset[err.2$keep.rows,]
  
  # Accouchements 3 enfants
  err.3 <- .get.childbirth.err(df.w, 3)
  df.3 <- err.3$subset[err.3$keep.rows,]
  
  return(rbind(df.1, df.2, df.3))
}

a.bzh <- get.childbirth.data.frame(n.bzh)
comment(a.bzh) <- "Accouchements en Bretagne en 2015"
  
# Sauvegarde ----
save(n, n.bzh, a.bzh, file = "naissances.RData")
