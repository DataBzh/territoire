library(RCurl)
library(jsonlite)
source("config.R")

# Utilitaires ----
epochToDate <- function(ms_since_epoch) {
  return(as.Date(as.POSIXct(ms_since_epoch / 1000, origin = "1970-01-01", tz = "GMT")))
}

cleanJSON <- function(json) {
  return(gsub("\n", "", json))
}

checkReponseHeaders <- function(headers, url) {
  http.status <- headers$value()["status"]
  if (http.status != "200") {
    message(url)
    stop(paste0("status = ", http.status))
  }
  if (http.status == "429") {
    message(url)
    stop("API was throttled")
  }
  
  # https://www.meetup.com/fr-FR/meetup_api/docs/#limits
  # X-RateLimit-Limit: The maximum number of requests that can be made in a window of time
  # X-RateLimit-Remaining: The remaining number of requests allowed in the current rate limit window
  # X-RateLimit-Reset: The number of seconds until the current rate limit window resets
  message(
    sprintf(
      "X-RateLimit-Limit = %s, X-RateLimit-Remaining = %s, X-RateLimit-Reset = %s",
      headers$value()["X-RateLimit-Limit"],
      headers$value()["X-RateLimit-Remaining"],
      headers$value()["X-RateLimit-Reset"]
    )
  )

  if (headers$value()["X-RateLimit-Remaining"] < 1) {
    x.rate.reset <- as.integer(headers$value()["X-RateLimit-Reset"]) + 1
    message(sprintf("Sys.sleep(%s)", x.rate.reset))
    Sys.sleep(x.rate.reset)
  }
}

# Groupes ----
get.groups.raw <- function(zip = "35000", radius = 7) {
  url <- sprintf(
    "https://api.meetup.com/find/groups?country=FR&zip=%s&radius=%d&fields=topics,last_event&order=members&sign=true&key=%s",
    zip,
    radius,
    MeetupAPIKey
  )
  
  responses <- vector()
  
  while (url != "") {
    message(url)
    header <- basicHeaderGatherer()
    response <- getURI(url, headerfunction = header$update)
    checkReponseHeaders(header, url)
    
    response.json <- fromJSON(cleanJSON(response))
    if (length(response.json) > 0) {
      responses <- append(responses, toJSON(response.json))
    }
    
    url <- "" # Pas de pagination pour les groupes
    #url <- response.json$meta$`next`
  }

  return(responses)
}

.groups.fields <- c("id", "name", "link", "country", "city", "lon", "lat", "members", "created", "topics", "last_event")

get.groups.df <- function(zip = "35000", radius = 7, responses = NULL) {
  if (is.null(responses)) {
    responses <- get.groups.raw(zip, radius)
  }
  
  # JSON compacté sur une ligne
  responses <- paste(responses, collapse = "")

  groups <- data.frame()

  for (i in 1:length(responses)) {
    response <- fromJSON(cleanJSON(responses[i]))
    response <- response[, .groups.fields]
    response$topics.id <- sapply(response$topics, function(topic) { paste(topic$id, collapse = "|") })
    response$topics.name <- sapply(response$topics, function(topic) { paste(topic$name, collapse = "|") })
    response$topics <- NULL
    groups <- rbind(groups, response)
  }

  groups$created <- epochToDate(groups$created)
  groups$last_event <- epochToDate(groups$last_event$time)
  
  return(groups)
}

# Membres ----
get.group.members.raw <- function(group.id) {
  url <- sprintf(
    "https://api.meetup.com/2/members?group_id=%s&sign=true&key=%s",
    group.id,
    MeetupAPIKey
  )
  
  responses <- vector()
  
  while (url != "") {
    message(url)
    header <- basicHeaderGatherer()
    response <- getURI(url, headerfunction = header$update)
    checkReponseHeaders(header, url)
    
    # JSON compacté sur une ligne
    response <- paste(response, collapse = "")
    
    response.json <- fromJSON(response)
    if (length(response.json$results) > 0) {
      responses <- append(responses, toJSON(response.json$results))
    }
    
    url <- response.json$meta$`next`
  }
  
  return(responses)
}

.group.members.fields <- c("id", "name", "link", "country", "city", "lon", "lat", "topics")

get.group.members.df <- function(group.id, responses = NULL) {
  if (is.null(responses)) {
    responses <- get.group.members.raw(group.id)
  }

  gm <- data.frame()
  
  for (i in 1:length(responses)) {
    response <- fromJSON(responses[i])
    response <- response[, .group.members.fields]
    response$topics.id <- sapply(response$topics, function(topic) { paste(topic$id, collapse = "|") })
    response$topics <- NULL
    gm <- rbind(gm, response)
  }
  
  gm$group.id <- group.id

  return(gm)
}

get.group.members.file <- function(gp.id, gp.name = NULL) {
  group.members.responses <- get.group.members.raw(gp.id)
  
  if (length(group.members.responses) == 0) {
    message(paste0("Empty group members set (", gp.id, ")"))
  } else {
    suffix <- ifelse(is.null(gp.name), gp.id, gp.name)
    write(group.members.responses, sprintf("data/members/group.members.%s.json", suffix))
    
    group.members <- get.group.members.df(gp.id, group.members.responses)
    write.table(group.members, sprintf("data/members/group.members.%s.tsv", suffix), sep = "\t", row.names = F)
  }
  
  return(T)
}
