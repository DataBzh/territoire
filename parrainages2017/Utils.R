.capitalize <- Vectorize(
  function(x) {
    sep = " "
    s <- strsplit(x, sep)[[1]]
    if (length(s) == 1) {
      sep = "-"
      s <- strsplit(x, sep)[[1]]
    }
    paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = sep)
  }
)

.initials <- Vectorize(
  function(x) {
    lastname <- regmatches(x, gregexpr("([A-Z]{2,})", x))
    firstname <- regmatches(x, gregexpr("([A-Z][a-zà-ÿ]+)", x))
    
    paste0(
      sapply(
        unlist(c(firstname, lastname)),
        function(x) { substr(x, 1, 1) }
      ),
      collapse = ""
    )
  }
)
