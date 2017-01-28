naf <- read.csv2("data/naf2008_5_niveaux.csv", colClasses = "character")

naf.niv1 <- read.csv2("data/naf2008_liste_n1.csv", colClasses = "character", fileEncoding = "utf-8")
names(naf.niv1) <- tolower(names(naf.niv1))

naf.niv2 <- read.csv2("data/naf2008_liste_n2.csv", colClasses = "character", fileEncoding = "utf-8")
names(naf.niv2) <- tolower(names(naf.niv2))

naf.niv3 <- read.csv2("data/naf2008_liste_n3.csv", colClasses = "character", fileEncoding = "utf-8")
names(naf.niv3) <- tolower(names(naf.niv3))

naf.niv4 <- read.csv2("data/naf2008_liste_n4.csv", colClasses = "character", fileEncoding = "utf-8")
names(naf.niv4) <- tolower(names(naf.niv4))

naf.niv5 <- read.csv2("data/naf2008_liste_n5.csv", colClasses = "character", fileEncoding = "utf-8")
names(naf.niv5) <- tolower(names(naf.niv5))

naf$lib1 <- naf.niv1[match(naf$NIV1, naf.niv1$Code),]$Libelle
naf$lib2 <- naf.niv2[match(naf$NIV2, naf.niv2$Code),]$Libelle
naf$lib3 <- naf.niv3[match(naf$NIV3, naf.niv3$Code),]$Libelle
naf$lib4 <- naf.niv4[match(naf$NIV4, naf.niv4$Code),]$Libelle
naf$lib5 <- naf.niv5[match(naf$NIV5, naf.niv5$Code),]$Libelle

names(naf) <- tolower(names(naf))

save(naf, naf.niv1, naf.niv2, naf.niv3, naf.niv4, naf.niv4, naf.niv5, file = "data/naf.RData")
