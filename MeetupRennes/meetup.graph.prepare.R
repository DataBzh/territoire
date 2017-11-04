library(igraph)
source("graph.utils.R")
load(file = "data/MeetupRennes.RData")

# Graphe topic=>groupe ----
topic_to_group.dg <-
  graph.data.frame(
    topic_to_group.df[, c("topic.id", "group.id")],
    directed = F
  )
attr(topic_to_group.dg, "description") <- "Graphe topic=>groupe"

V(topic_to_group.dg)$type <-
  ifelse(
    V(topic_to_group.dg)$name %in% unique(topic_to_group.df$group.id),
    "g",
    "t"
  )

V(topic_to_group.dg)[type == "g"]$label <-
  sapply(
    V(topic_to_group.dg)[type == "g"]$name,
    function(id) {
      group.df[group.df$id == id,]$name
    }
  )
V(topic_to_group.dg)[type == "t"]$label <-
  sapply(
    V(topic_to_group.dg)[type == "t"]$name,
    function(id) {
      topic.df[topic.df$topic.id == id,]$topic.name
    }
  )

## Projection ----
proj <- bipartite_projection(
  topic_to_group.dg,
  types = V(topic_to_group.dg)$type == "g"
)

## Graphe topic=>topic ----
topic_to_topic.dg <- proj$proj1
attr(topic_to_topic.dg, "description") <- "Graphe topic=>topic"

## Graphe groupe=>groupe liés par topics ----
group_to_group_via_topic.dg <- proj$proj2
attr(group_to_group_via_topic.dg, "description") <- "Graphe groupe=>groupe via topic"

# Graphe membre=>groupe ----
member_to_group.dg <-
  graph.data.frame(
    member_to_group.df,
    directed = F
  )
attr(member_to_group.dg, "description") <- "Graphe membre=>groupe"

V(member_to_group.dg)$type <-
  ifelse(
    V(member_to_group.dg)$name %in% unique(member_to_group.df$group.id),
    "g",
    "m"
  )

V(member_to_group.dg)[type == "g"]$label <-
  sapply(
    V(member_to_group.dg)[type == "g"]$name,
    function(id) {
      group.df[group.df$id == id,]$name
    }
  )
V(member_to_group.dg)[type == "m"]$label <-
  sapply(
    V(member_to_group.dg)[type == "m"]$name,
    function(id) {
      group.df[group.df$id == id,]$name
    }
  )

## Projection ----
proj <- bipartite_projection(
  member_to_group.dg,
  types = V(member_to_group.dg)$type == "g"
)

## Graphe membre=>membre ----
member_to_member.dg <- proj$proj1
attr(member_to_member.dg, "description") <- "Graphe membre=>membre"

## Graphe groupe=>groupe liés par membres ----
group_to_group_via_member.dg <- proj$proj2
attr(group_to_group_via_member.dg, "description") <- "Graphe groupe=>groupe via membres"

# Sauvegarde ----
save(
  topic_to_group.dg,
  member_to_group.dg,
  topic_to_topic.dg,
  group_to_group_via_topic.dg,
  group_to_group_via_member.dg,
  file = "data/MeetupRennes.Graph.RData"
)

save(
  member_to_member.dg,
  file = "data/MeetupRennes.Graph.Members.RData"
)
