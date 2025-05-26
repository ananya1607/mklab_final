# impact of node removal

g<-g_wt0
components_g<-components(g)
cluster_classification_g<-cluster_classification_wt0
as_long_data_frame(g) %>% head()
cluster_sizes<-data.frame(cluster_no=1:length(components_g$csize), csize=components_g$csize) 
cluster_classification_g<-cluster_classification_g %>% left_join(cluster_sizes, by=c("Cluster"="cluster_no"))
V(g)$component_no<-components_g$membership
V(g)$degree<-degree(g)

g_df<-as_long_data_frame(g)
g_df %>% head()

g_df<-
  g_df %>% 
  left_join(cluster_classification_g, by=c("from_component_no"="Cluster")) %>% 
  rename("from_cluster_type"="cluster_type") %>%
  left_join(cluster_classification_g, by=c("to_component_no"="Cluster")) %>% 
  rename("to_cluster_type"="cluster_type") %>% 
  select(c(1:13, 16,17,20,21)) %>% unique()

g_df_psete<-g_df %>% filter(from_cluster_type=="P-SE-TE")
g_df_psete %>% head()
# this has only the nodes for PSETE graphs
a<-g_df_psete %>% select(from_component_no,from_name,from_LA1_type, from_degree)
b<-g_df_psete %>% select(to_component_no,to_name,to_LA1_type, to_degree)
colnames(a)<-colnames(b)
psete_nodes<-rbind(a,b) %>% unique()

component_type_counts<-psete_nodes %>% group_by(to_component_no, to_LA1_type) %>% summarise(count=n())


component_type_wide <- component_type_counts %>%
  pivot_wider(names_from = to_LA1_type, values_from = count, values_fill = 0)


ggplot(component_type_counts, aes(x = factor(to_component_no), y = count, fill = to_LA1_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Component", y = "Count", fill = "Node Type",
       title = "Composition of Node Types in Each Component") +
  theme_minimal()

component_type_counts %>% head()
component_type_counts %>% select(1) %>% unique

component_type_counts <- component_type_counts %>%
  group_by(to_component_no) %>%
  mutate(cluster_order = cur_group_id()) %>%
  ungroup()

# Set the order: Promoter first, then SE constituent, then Typical Enhancer
# component_type_counts <- component_type_counts %>%
  # mutate(to_LA1_type = factor(to_LA1_type, levels = c("typical enhancer","super enhancer","promoter")))

ggplot(component_type_counts, 
       aes(x = factor(cluster_order), y = count, fill = to_LA1_type)) +
  geom_bar(stat = "identity") +  # no border, clean bars
  scale_fill_manual(values = c(
    "super enhancer" = "#c7afd5",
    "typical enhancer" = "#8bcac5",
    "promoter" = "#90c1e0"
  )) +
  scale_y_continuous(expand = c(0, 0)) +  # <-- IMPORTANT: makes bars stick to bottom
  labs(
    x = "Component",
    y = "Count",
    fill = "Node Type",
    title = "Composition of Node Types in Each Component"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

ggsave("psete_clusters_comonents.svg", dpi=1200, width=10, height=8)
psete_nodes %>% head()
se_mix_deg<-psete_nodes %>% filter(to_LA1_type=="super enhancer") 
(max(components_g_wt0$csize))

# Extract the membership and cluster sizes
membership <- components_g_wt0$membership
cluster_sizes <- components_g_wt0$csize

# Identify the cluster ID with the largest size
largest_cluster_id <- which.max(cluster_sizes)

# Get the names of enhancers that belong to this cluster
enhancers_in_largest_cluster <- names(membership[membership == largest_cluster_id])

# View result
enhancers_in_largest_cluster

str(components_g_wt0)

se_mix_deg<-se_mix_deg %>% arrange(desc(se_mix_deg$to_degree)) %>% pull(to_name)

te_mix_deg<-psete_nodes %>% filter(to_LA1_type=="typical enhancer") 
te_mix_deg<-te_mix_deg %>% arrange(desc(te_mix_deg$degree)) %>% pull(to_name)
se_mix_deg %>% unique() %>% length()
te_mix_deg %>% length()
results_typical <- data.frame(nodes_removed = integer(), diameter = numeric(), avg_path_length = numeric(), clusters = integer())
results_super <- data.frame(nodes_removed = integer(), diameter = numeric(), avg_path_length = numeric(), clusters = integer())


graph_temp <- g

for (i in seq_along(te_mix_deg)){
  graph_temp<-delete_vertices(graph_temp, te_mix_deg[i])
  diameter_value<-diameter(graph_temp, directed = FALSE, unconnected = TRUE)
  avg_path_length <- mean_distance(graph_temp, directed = FALSE)
  num_clusters <- components(graph_temp)$no
  results_typical <- rbind(results_typical, data.frame(nodes_removed = i, diameter = diameter_value, avg_path_length = avg_path_length, clusters = num_clusters))
}

graph_temp <- g

for (i in seq_along(se_mix_deg)){
  graph_temp<-delete_vertices(graph_temp, se_mix_deg[i])
  diameter_value<-diameter(graph_temp, directed = FALSE, unconnected = TRUE)
  avg_path_length <- mean_distance(graph_temp, directed = FALSE)
  num_clusters <- components(graph_temp)$no
  results_super <- rbind(results_super, data.frame(nodes_removed = i, diameter = diameter_value, avg_path_length = avg_path_length, clusters = num_clusters))
}

#####
# this is the graphs according to degree
# Diameter plot
results_typical$diameter
ggplot() +
  geom_line(data = results_typical, aes(x = nodes_removed, y = diameter, color = "Typical Enhancers"), size = 1) +
  geom_line(data = results_super, aes(x = nodes_removed, y = diameter, color = "Super Enhancers"), size = 1) +
  labs(title = "Diameter vs. Number of Nodes Removed", x = "Number of Nodes Removed", y = "Diameter") +
  scale_color_manual(values = c("Typical Enhancers" = "", "Super Enhancers" = "#c7afd5")) +
  theme_minimal()

results_typical$diameter

plot(results_typical$diameter, col='red',ylim=c(0,10000),xlim=c(0,100))  # Red for degree-based deletion
points(results_super$diameter, col='blue')  # Blue for random deletion
legend("topright", legend=c("typical enhancers","super enhancers"), col=c("red","blue"), lty = 1)

# Average Path Length plot
results_typical$avg_path_length

results_super %>% head()
results_super<-results_super %>% slice(1:45)
ggplot() +
  geom_line(data = results_typical, aes(x = nodes_removed, y = avg_path_length, color = "Typical Enhancers"), size = 1) +
  geom_line(data = results_super, aes(x = nodes_removed, y = avg_path_length, color = "Super Enhancers"), size = 1) +
  labs(title = "Average Path Length vs. Number of Nodes Removed", x = "Number of Nodes Removed", y = "Average Path Length") +
  scale_color_manual(values = c("Typical Enhancers" = "#5ab4ac", "Super Enhancers" = "#af8dc3")) +
  theme_minimal()
getwd()
ggsave("node_removal_apl.svg", dpi=1200, width=6, height=4)

plot(results_typical$avg_path_length, col='red',ylim=c(10,11.25),xlim=c(0,45))  # Red for degree-based deletion
points(results_super$avg_path_length, col='blue')  # Blue for random deletion
legend("bottomright", legend=c("typical enhancers","super enhancers"), col=c("red","blue"), lty = 1)


# Number of Clusters plot
ggplot() +
  geom_line(data = results_typical, aes(x = nodes_removed, y = clusters, color = "Typical Enhancers"), size = 1) +
  geom_line(data = results_super, aes(x = nodes_removed, y = clusters, color = "Super Enhancers"), size = 1) +
  labs(title = "Number of Clusters vs. Number of Nodes Removed", x = "Number of Nodes Removed", y = "Number of Clusters") +
  scale_color_manual(values = c("Typical Enhancers" = "blue", "Super Enhancers" = "red")) +
  theme_minimal()

plot(results_typical$clusters, col='red',ylim=c(1500,1750),xlim=c(0,100))  # Red for degree-based deletion
points(results_super$clusters, col='blue')  # Blue for random deletion
legend("bottomright", legend=c("typical enhancers","super enhancers"), col=c("red","blue"), lty = 1)



# cluster_classification_wt0 %>% head()
# cluster_sizes<-data.frame(cluster_no=1:length(components_g$csize), csize=components_g$csize) 
# cluster_sizes %>% head()
# cluster_classification_wt0 %>% head()
# cluster_classification_g<-cluster_classification_g %>% left_join(cluster_sizes, by=c("Cluster"="cluster_no"))

# V(g)$component_no<-components_g$membership
g_df<-as_long_data_frame(g)
g_df %>% head()

cluster_classification_wt0 %>% filter(cluster_type=="P-SE-TE") %>% nrow() # there are 21 P-SE_TE clusters
# V(g)$c_size<-components_g$csize 
g_df<-g_df %>% left_join(cluster_classification_wt0, by=c("from_component_no"="Cluster")) %>% rename("from_cluster_type"="cluster_type") %>%
  left_join(cluster_classification_wt0, by=c("to_component_no"="Cluster")) %>% rename("to_cluster_type"="cluster_type") %>% select(c(1:11, 14,15,18,19)) %>% unique()

g_df_psete<-g_df %>% filter(from_cluster_type=="P-SE-TE")
g_df_psete %>% head()
# this has only the nodes for PSETE graphs
a<-g_df_psete %>% select(from_component_no,from_name,from_LA1_type)
b<-g_df_psete %>% select(to_component_no,to_name,to_LA1_type)
colnames(a)<-colnames(b)
psete_nodes<-rbind(a,b) %>% unique()

component_type_counts<-psete_nodes %>% group_by(to_component_no, to_LA1_type) %>% summarise(count=n())


component_type_wide <- component_type_counts %>%
  pivot_wider(names_from = to_LA1_type, values_from = count, values_fill = 0)


ggplot(component_type_counts, aes(x = factor(to_component_no), y = count, fill = to_LA1_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Component", y = "Count", fill = "Node Type",
       title = "Composition of Node Types in Each Component") +
  theme_minimal()
# ps
psete_g <- induced_subgraph(g, vids = psete_nodes$to_name)

deg_df <- data.frame(
  id = V(g)$name,
  degree = degree(g)
)
deg_df %>%left_join(psete_nodes,by=c("id"="to_name")) %>% arrange(desc(degree))

psete_g_degree<-degree(psete_g)
str(psete_g_degree)
psete_nodes %>% head() %>% mutate("degree"=psete_g_degree)
deg_df <- data.frame(
  id = V(g)$name,
  degree = degree(g)
)

degrees_psete_super<-
  
  components(psete_g_wt0)$no # again we are getting 21 components only
psete_se<-psete_nodes %>% filter(to_LA1_type=="super enhancer") %>% pull(to_name)
psete_te<-psete_nodes %>% filter(to_LA1_type=="typical enhancer") %>% pull(to_name)





g_wt0_df %>% select(from_component_no,from_LA1_type, csize.x, from_cluster_type) %>% filter(from_cluster_type=="P-SE-TE")%>% group_by(from_component_no, csize.x, from_cluster_type) %>% summarise(count=n())
# select(c(1:11,14,17)) %>%  filter(from_cluster_type=="P-SE-TE") %>% head()
# cluster_classification_wt360 %>% head()

# P SE TE cluster size distribution
g<-g_wt0
library(igraph)

# Assuming your graph object is named g
clusters_info <- clusters(g)  # or cluster_louvain(g) / cluster_walktrap(g) if you're using community detection

# Get membership for each node
V(g)$cluster <- clusters_info$membership

library(dplyr)

node_df <- data.frame(
  name = V(g)$name,
  type = V(g)$LA1_type,
  cluster = V(g)$cluster
)

type_counts <- node_df %>%
  count(cluster, type) %>%
  tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
  mutate(cluster_size = rowSums(across(where(is.numeric))))
library(ggplot2)
library(tidyr)

type_counts_long <- type_counts %>%
  pivot_longer(cols = -c(cluster, cluster_size), names_to = "type", values_to = "count")

type_counts_long %>% head()
type_counts_long %>% select(type) %>% unique()

ggplot(type_counts_long, aes(x = factor(cluster), y = count, fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.3, size = 3) +
  labs(x = "Cluster ID", y = "Count", fill = "Node Type",
       title = "Node Composition in Each Enhancer-Promoter Cluster") +
  theme_minimal()


