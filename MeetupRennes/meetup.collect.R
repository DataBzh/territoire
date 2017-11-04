source("utils.R")

# Groupes ----
groups <- get.groups.df()
write.table(groups, "data/groups/groups.tsv", sep = "\t", row.names = F, fileEncoding = "utf-8")

# Membres ----
for (group.id in groups$id) {
  message(group.id)
  
  result <- F
  while (result == F) {
    result <- tryCatch(
      get.group.members.file(group.id),
      error = function(e) { F }
    )
  }
}
