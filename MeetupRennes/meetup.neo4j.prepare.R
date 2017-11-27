library(igraph)
source("graph.utils.R")
load(file = "data/MeetupRennes.RData")

# Nodes ----
## Members ----
write.table(member.df[, 1:5], "neo4j/data/members.nodes.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

## Groups ----
write.table(group.df[, c("id", "name", "link", "country", "city", "lon", "lat", "created", "last_event")], "neo4j/data/groups.nodes.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

## Topics ----
t.df <- topic.df[, 1:2]
names(t.df) <- c("id", "name")
write.table(t.df, "neo4j/data/topics.nodes.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

# Relationships ----
## Member -> Group
write.table(member_to_group.df, "neo4j/data/member_to_group.rel.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

## Topic -> Group ----
write.table(topic_to_group.df, "neo4j/data/topic_to_group.rel.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")
