# install.packages("igraph")

library(igraph)


winners <- read_excel("T20_match_winners.xlsx")
View(winners)
summary(T20_match_winners)

scores <- read_excel("T20_Scores.xlsx")
View(scores)
summary(T20_Scores)



# Get unique striker-match pairs
edges_striker <- unique(data.frame(
  player = scores$striker,
  match = as.character(scores$match_id)
))

# Optional: Add bowler-match relationships too
edges_bowler <- unique(data.frame(
  player = scores$bowler,
  match = as.character(scores$match_id)
))

# Combine edge lists
edges <- rbind(edges_striker, edges_bowler)




g <- graph_from_data_frame(edges, directed = FALSE)

# Assign node types: TRUE = player, FALSE = match
V(g)$type <- !V(g)$name %in% unique(edges$match)

# Check structure
table(V(g)$type)  # Should show counts of players vs. matches






plot(
  g,
  vertex.label = NA,
  vertex.size = 5,
  layout = layout_as_bipartite(g),
  main = "Player-Match Bipartite Network"
)

# Plotting the network
png("bipartite_plot.png", width = 1600, height = 1200)
plot(
  g,
  vertex.label = NA,
  vertex.size = 5,
  layout = layout_as_bipartite(g),
  main = "Player-Match Bipartite Network"
)
dev.off()



