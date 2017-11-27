library(igraph)
load(file = "data/MeetupRennes.RData")
load(file = "data/MeetupRennes.Graph.RData")

# Meetups ----
## Edges ----
e.df <- member_to_group.df
names(e.df) <- c("Source", "Target")
write.table(e.df, "gephi/data/member_to_group.edges.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

## Vertices ----
v.df <- as_data_frame(member_to_group.dg, "vertices")[, c("name", "label", "type")]
names(v.df) <- c("Id", "Label", "Type")

v.df$Caption <- ""
v.df[v.df$Type == "g",]$Caption <- sapply(v.df[v.df$Type == "g",]$Label, function(l) { l[[1]] })
v.df$Label <- NULL

write.table(v.df, "gephi/data/member_to_group.vertices.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

# Topics ----
## Edges ----
e.df <- as_data_frame(topic_to_group.dg, "edges")
names(e.df) <- c("Source", "Target")

write.table(e.df, "gephi/data/topic_to_group.edges.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")

## Vertices ----
v.df <- as_data_frame(topic_to_group.dg, "vertices")[, c("name", "label", "type")]
names(v.df) <- c("Id", "Label", "Type")
write.table(v.df, "gephi/data/topic_to_group.vertices.tsv", sep = "\t", quote = F, row.names = F, fileEncoding = "UTF-8")
