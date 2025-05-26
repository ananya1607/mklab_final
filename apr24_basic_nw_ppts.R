g<-g_wt0
vcount(g)
ecount(g)
edge_density(g)

components_g <- components(g)
max(components_g$csize)
largest_comp <- induced_subgraph(g, which(components_g$membership == which.max(components_g$csize)))
average.path.length(largest_comp, directed = FALSE)
# diameter(g, directed = FALSE, unconnected = T)
# diameter()
diameter(g, directed = FALSE, unconnected = TRUE)
comp <- components(g)
g_largest <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
diameter(g_largest, directed = FALSE,unconnected = F)
# why is diameter coming in pointa
transitivity(g, type = "global",isolates  ="zero")
comm <- cluster_louvain(g)
modularity(comm)


cluster_classification_g<-cluster_classification_wt0
V(g)$membership<-components_g_wt0$membership
V(g)$cluster_category <- cluster_classification_g$cluster_type[match(V(g)$membership, cluster_classification_g$Cluster)]
categories <- unique(V(g)$cluster_category)

density_df <- data.frame()

for (comp_id in unique(V(g)$membership)) {
  sub_nodes <- V(g)[membership == comp_id]
  sub_g <- induced_subgraph(g, sub_nodes)
  ed_density <- edge_density(sub_g)
  
  # Get dominant cluster category in this component
  cat_table <- table(V(sub_g)$cluster_category)
  dominant_cat <- names(cat_table)[which.max(cat_table)]
  
  density_df <- rbind(density_df, data.frame(
    component = comp_id,
    edge_density = ed_density,
    cluster_category = dominant_cat
  ))
}


density_df <- lapply(categories, function(cat) {
  nodes_in_cat <- V(g)[cluster_category == cat]
  sub_g <- induced_subgraph(g, vids = nodes_in_cat)
  data.frame(cluster_category = cat, edge_density = edge_density(sub_g))
}) %>% bind_rows()

ggplot(density_df, aes(x = cluster_category, y = edge_density, fill = cluster_category)) +
  geom_boxplot() +
  labs(title = "Edge Density per Cluster Category", x = "Cluster Category", y = "Edge Density") +
  theme_minimal()

