library(rvest)

# Fréquence des lettres dans les noms d'entreprise ----
lc0.bzh <- read.delim("data/lc_civ0_bzh.tsv", header = F, stringsAsFactors = F, col.names = c("letter", "count"))

lc0.bzh$freq <- lc0.bzh$count / sum(lc0.bzh$count)

lc1.bzh <- read.delim("data/lc_civ1_bzh.tsv", header = F, stringsAsFactors = F, col.names = c("letter", "count"))

lc1.bzh$freq <- lc1.bzh$count / sum(lc1.bzh$count)

# Fréquence des lettres dans la langue française ----

wiki_url <- "https://fr.wikipedia.org/wiki/Fréquence_d%27apparition_des_lettres_en_français"

letters.freq <- wiki_url %>%
  read_html %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table') %>%
  html_table(dec = ",")
letters.freq <- letters.freq[[1]]

names(letters.freq) <- c("rank", "letter", "count", "freq")
letters.freq$count <- as.integer(sapply(letters.freq$count, function(x) { gsub("[^0-9]", "", x)}))

## Lettres des A à Z ----
letters.freq.az <- letters.freq[letters.freq$letter %in% letters,]

letters.freq.az$letter <- unclass(letters.freq.az$letter)
letters.freq.az$rank <- NULL
letters.freq.az$freq <- letters.freq.az$count / sum(letters.freq.az$count)

letters.freq.az <- letters.freq.az[order(letters.freq.az$letter),]
row.names(letters.freq.az) <- NULL

## Noms composés d'une seule lettre ----
load(file = "data/noms.RData")

one.letter <- Vectorize(
  function(name) {
    name.char <- strsplit(name, split = "", useBytes = T)
    return(length(unique(name.char[[1]])) == 1)
  }
)

noms.bzh.one.letter <- noms.bzh[one.letter(noms.bzh$l_normalisee),]

## Sauvegarde ----
save(lc0.bzh, lc1.bzh, letters.freq, letters.freq.az, noms.bzh.one.letter, file = "data/letters-freq.RData")
