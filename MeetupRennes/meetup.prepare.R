source("utils.R")
source("graph.utils.R")

# Membres ----
## Fusion des fichiers contenant les membres des groupes (1 fichier par groupe)
ids <- gsub(".tsv", "", gsub("group.members.", "", list.files(path = "data/members", pattern = "*.tsv")))
filenames <- paste0("data/members/group.members.", ids, ".tsv")

members <- data.frame()
member_to_group.df <- data.frame()

for (filename in filenames) {
  if (!file.exists(filename)) {
    message(paste0("File not found: ", filename))
  } else {
    members.current <- read.table(filename, header = T, sep = "\t", stringsAsFactors = F)
    
    # Ajout des membres non présents
    append.ids <- setdiff(members.current$id, members$id)
    members <- rbind(members, members.current[members.current$id %in% append.ids,])
    
    member_to_group.df <- rbind(member_to_group.df, data.frame(group.id = members.current$group.id, member.id = members.current$id))
  }
}

member.df <- members[, c("id", "country", "city", "lon", "lat")]
member.df$name <- NA
member.df[member.df$id == MeetupID,]$name <- "Michel CARADEC"
attr(member.df, "description") <- "Liste des membres de tous les groupes"

attr(member_to_group.df, "description") <- "membre=>groupe"

# Groupes ----
groups <- read.table("data/groups/groups.tsv", header = T, sep = "\t", stringsAsFactors = F)
group.df <- groups[, c("id", "name", "link", "country", "city", "lon", "lat", "topics.id", "topics.name", "created", "last_event")]

## Nombre de membres par groupe
gpby.group <- aggregate(member.id ~ group.id, data = member_to_group.df, length)
names(gpby.group) <- c("group.id", "count")
group.df$member.count <- sapply(group.df$id, function(id) { ifelse(id %in% gpby.group$group.id, gpby.group[gpby.group$group.id == id,]$count, NA) })

attr(group.df, "description") <- "Liste des groupes"

# Topics ----
saf <- getOption("stringsAsFactors"); options(stringsAsFactors = F) # Désactivation temporaire de l'option stringsAsFactors (default.stringsAsFactors utilisé par rbind)
topic.df <- do.call(
  rbind,
  lapply(
    1:nrow(group.df),
    function(r) {
      return(
        data.frame(
          topic.id = unlist(strsplit(group.df[r,]$topics.id, "|", fixed = T)),
          topic.name = unlist(strsplit(group.df[r,]$topics.name, "|", fixed = T)),
          group.id = group.df[r,]$id,
          group.name = group.df[r,]$name
        )
      )
    }
  )
)
options(stringsAsFactors = saf)

topic_to_group.df <- topic.df[, c("topic.id", "group.id")]
attr(topic_to_group.df, "description") <- "topic=>groupe"

topic.df$group.count <- 0
topic.df <- aggregate(group.count ~ topic.id + topic.name, topic.df, length)
attr(topic.df, "description") <- "Liste des topics de tous les groupes"

# Sauvegarde ----
save(
  group.df,
  member.df,
  topic.df,
  member_to_group.df,
  topic_to_group.df,
  file = "data/MeetupRennes.RData"
)
