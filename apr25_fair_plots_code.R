setwd("~/Documents/Ananya/Thesis_4/may15plots/")

# finalised plots  # final final

ggplot(df1_wt0, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  scale_fill_manual(
    values = c(
      "P-SE" = "#af8dc3",
      "P-TE" = "#5ab4ac",
      "P-SE-TE" = "#84a0b8"
    ),
    na.value = "lightgray"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # <- Keep bars flush, add space above
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "none",       
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

ggplot(df1_wt0, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  scale_fill_manual(
    values = c(
      "P-SE" = "#af8dc3",
      "P-TE" = "#5ab4ac",
      "P-SE-TE" = "#84a0b8"
    ),
    na.value = "lightgray"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(face = "bold", size = 14),  # <- Make y-axis labels bold and larger
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
ggplot(df1_wt0, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  scale_fill_manual(
    values = c(
      "P-SE" = "#af8dc3",
      "P-TE" = "#5ab4ac",
      "P-SE-TE" = "#84a0b8"
    ),
    na.value = "lightgray"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
ggplot(df1_wt0, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  scale_fill_manual(
    values = c(
      "P-SE" = "#af8dc3",
      "P-TE" = "#5ab4ac",
      "P-SE-TE" = "#84a0b8"
    ),
    na.value = "lightgray"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(face = "bold"),     # <- Bold x-axis labels
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )


ggsave("plot1_wt0.svg", width = 6, height = 5, dpi=1200)
df_wt0 %>% select(char) %>% pull()

ggplot(df_wt0, aes(x = char, y = Count, fill = char)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +  # Slightly lift labels above bars
  labs(
    title = "Counts of the nodes",
    x = "",
    y = "Count"
  ) +
  scale_fill_manual(values = c(
    "SE constituent" = "#af8dc3",
    "Typical Enhancer" = "#5ab4ac",
    "Promoter" = "#C3AF8D"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # Bars touch x-axis; space above
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(face = "bold"),     # <- Bold x-axis labels
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
ggsave("plot2_wt0.svg", width = 6, height = 5, dpi=2400)

ggplot(df2_wt0, aes(x = cluster_type, y = cluster_size, fill = cluster_type)) +
  geom_boxplot() +
  labs(
    title = "Cluster Size Distribution by Cluster Category",
    x = "",
    y = "Cluster Size"
  ) +
  scale_fill_manual(values = c(
    "P-SE" = "#af8dc3",
    "P-TE" = "#5ab4ac",
    "P-SE-TE" = "#84a0b8"
  ),
  na.value = "lightgray"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(face = "bold"),     # <- Bold x-axis labels
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

ggsave("plot3_wt0.svg", width = 6, height = 5, dpi=1200)


wt0_degree<-a_wt0$degree
wt360_degree<-a_wt360$degree
wt4320_degree<-a_wt4320$degree

wt0_degree %>% head()
wt0_degree<-wt0_degree %>% mutate("data"="0hr")
wt360_degree<-wt360_degree %>% mutate("data"="6hr")
wt4320_degree<-wt4320_degree %>% mutate("data"="72hr")
degree_full<-rbind(wt0_degree,wt360_degree, wt4320_degree)
degree_full %>% select(cluster_type) %>% unique()

wt0_norm_degree<-a_wt0$norm_degree
wt360_norm_degree<-a_wt360$norm_degree
wt4320_norm_degree<-a_wt4320$norm_degree

wt0_norm_degree %>% head()
wt0_norm_degree<-wt0_norm_degree %>% mutate("data"="0hr")
wt360_norm_degree<-wt360_norm_degree %>% mutate("data"="6hr")
wt4320_norm_degree<-wt4320_norm_degree %>% mutate("data"="72hr")
norm_degree_full<-rbind(wt0_norm_degree,wt360_norm_degree, wt4320_norm_degree)
norm_degree_full %>% select(cluster_type) %>% unique()

wt0_strength<-a_wt0$strength
wt360_strength<-a_wt360$strength
wt4320_strength<-a_wt4320$strength

wt0_strength %>% head()
wt0_strength<-wt0_strength %>% mutate("data"="0hr")
wt360_strength<-wt360_strength %>% mutate("data"="6hr")
wt4320_strength<-wt4320_strength %>% mutate("data"="72hr")
strength_full<-rbind(wt0_strength,wt360_strength, wt4320_strength)
strength_full %>% head()

wt0_closeness<-a_wt0$closeness
wt360_closeness<-a_wt360$closeness
wt4320_closeness<-a_wt4320$closeness

wt0_closeness %>% head()
wt0_closeness<-wt0_closeness %>% mutate("data"="0hr")
wt360_closeness<-wt360_closeness %>% mutate("data"="6hr")
wt4320_closeness<-wt4320_closeness %>% mutate("data"="72hr")
closeness_full<-rbind(wt0_closeness, wt360_closeness, wt4320_closeness)
closeness_full %>% head()

wt0_norm_closeness<-a_wt0$norm_closeness
wt360_norm_closeness<-a_wt360$norm_closeness
wt4320_norm_closeness<-a_wt4320$norm_closeness

wt0_norm_closeness %>% head()
wt0_norm_closeness<-wt0_norm_closeness %>% mutate("data"="0hr")
wt360_norm_closeness<-wt360_norm_closeness %>% mutate("data"="6hr")
wt4320_norm_closeness<-wt4320_norm_closeness %>% mutate("data"="72hr")
norm_closeness_full<-rbind(wt0_norm_closeness,wt360_norm_closeness, wt4320_norm_closeness)
norm_closeness_full %>% head()

wt0_betweenness<-a_wt0$betweenness
wt360_betweenness<-a_wt360$betweenness
wt4320_betweenness<-a_wt4320$betweenness

wt0_betweenness %>% head()
wt0_betweenness<-wt0_betweenness %>% mutate("data"="0hr")
wt360_betweenness<-wt360_betweenness %>% mutate("data"="6hr")
wt4320_betweenness<-wt4320_betweenness %>% mutate("data"="72hr")
betweenness_full<-rbind(wt0_betweenness,wt360_betweenness, wt4320_betweenness)
betweenness_full %>% head()

wt0_norm_betweenness<-a_wt0$norm_betweenness
wt360_norm_betweenness<-a_wt360$norm_betweenness
wt4320_norm_betweenness<-a_wt4320$norm_betweenness

wt0_norm_betweenness %>% head()
wt0_norm_betweenness<-wt0_norm_betweenness %>% mutate("data"="0hr")
wt360_norm_betweenness<-wt360_norm_betweenness %>% mutate("data"="6hr")
wt4320_norm_betweenness<-wt4320_norm_betweenness %>% mutate("data"="72hr")
norm_betweenness_full<-rbind(wt0_norm_betweenness,wt360_norm_betweenness, wt4320_norm_betweenness)
norm_betweenness_full %>% head()

wt0_harmonic<-a_wt0$harmonic
wt360_harmonic<-a_wt360$harmonic
wt4320_harmonic<-a_wt4320$harmonic

wt0_harmonic %>% head()
wt0_harmonic<-wt0_harmonic %>% mutate("data"="0hr")
wt360_harmonic<-wt360_harmonic %>% mutate("data"="6hr")
wt4320_harmonic<-wt4320_harmonic %>% mutate("data"="72hr")
harmonic_full<-rbind(wt0_harmonic,wt360_harmonic, wt4320_harmonic)
harmonic_full %>% head()

wt0_norm_harmonic<-a_wt0$norm_harmonic
wt360_norm_harmonic<-a_wt360$norm_harmonic
wt4320_norm_harmonic<-a_wt4320$norm_harmonic

wt0_norm_harmonic %>% head()
wt0_norm_harmonic<-wt0_norm_harmonic %>% mutate("data"="0hr")
wt360_norm_harmonic<-wt360_norm_harmonic %>% mutate("data"="6hr")
wt4320_norm_harmonic<-wt4320_norm_harmonic %>% mutate("data"="72hr")
norm_harmonic_full<-rbind(wt0_norm_harmonic,wt360_norm_harmonic, wt4320_norm_harmonic)
norm_harmonic_full %>% head()

wt0_eigen<-eigen_centrality_wt0 %>% mutate("data"="0hr")
wt360_eigen<-eigen_centrality_wt360 %>% mutate("data"="6hr")
wt4320_eigen<-eigen_centrality_wt4320 %>% mutate("data"="72hr")

plot_func<-function(data_frame, metric_name, y_axis_name, by_inp, file_prefix){
  
  rel_data<-data_frame %>% filter(cluster_type %in% c("P-SE", "P-TE","P-SE-TE")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer"))
  
  print("Cluster - wise")
  print(rel_data %>% group_by(cluster_type, LA1_type) %>% summarise(Q1=quantile(.data[[metric_name]], 0.25, na.rm=T),
                                                                    median=quantile(.data[[metric_name]], 0.5, na.rm=T),
                                                                    Q3=quantile(.data[[metric_name]], 0.75, na.rm=T)))
  
  # First, calculate global whiskers
  whiskers <- rel_data %>%
    group_by(cluster_type, LA1_type) %>%
    summarise(
      Q1 = quantile(.data[[metric_name]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[metric_name]], 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_whisker = Q1 - 1.5 * IQR,
      upper_whisker = Q3 + 1.5 * IQR,
      .groups = "drop"
    )
  
  global_min <- min(whiskers$lower_whisker, na.rm = TRUE)
  global_max <- max(whiskers$upper_whisker, na.rm = TRUE)
  range_padding <- 0.05 * (global_max - global_min)
  
  y_lower <- global_min - range_padding
  y_upper <- global_max + range_padding
  
  rel_data$cluster_type <- factor(rel_data$cluster_type, levels = c("P-SE", "P-TE", "P-SE-TE"))
  
  # Now, the plot
  p1<-ggplot(rel_data, aes(x = cluster_type, y = .data[[metric_name]], fill = LA1_type)) +
    geom_boxplot(
      # width = 0.45,                            # Make boxes narrower
      # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c(
      "super enhancer" = "#af8dc3",
      "typical enhancer" = "#5ab4ac"
    ))+
    scale_x_discrete(labels = c(
      "super enhancer" = "Super Enhancers",
      "typical enhancer" = "Typical Enhancers"
    ))+
    coord_cartesian(ylim = c(0, y_upper)) +
    scale_y_continuous(
      breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
      # , expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal() +
    theme(legend.position = "none")+
    labs(x = "", y = y_axis_name, fill = "LA1 Type")+
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.6),
      axis.line.y = element_line(color = "black", size = 0.6),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
      axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
      axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  
  ggsave(paste0(file_prefix, "_p1.svg"),plot = p1, width = 4, height = 4, dpi = 1200)
  
  p2<-ggplot(rel_data, aes(x = cluster_type, y = .data[[metric_name]], fill = LA1_type)) +
    geom_boxplot(
      # width = 0.45,                            # Make boxes narrower
      # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c(
      "super enhancer" = "#af8dc3",
      "typical enhancer" = "#5ab4ac"
    ))+
    scale_x_discrete(labels = c(
      "super enhancer" = "Super Enhancers",
      "typical enhancer" = "Typical Enhancers"
    ))+
    coord_cartesian(ylim = c(0, y_upper)) +
    scale_y_continuous(
      breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
      # , expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal() +
    theme(legend.position = "none")+
    labs(x = "Cluster Type", y = metric_name, fill = "LA1 Type")+
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.6),
      axis.line.y = element_line(color = "black", size = 0.6),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
      axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
      axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  
 
  # plot the quantiles of the data 
  a<-quantile((rel_data %>% filter(LA1_type=="super enhancer") %>% select(metric_name) %>% pull(.data[[metric_name]])))
  print("super_enhancers = ")
  print(a)
  b<-quantile((rel_data %>% filter(LA1_type=="typical enhancer") %>% select(metric_name) %>% pull(.data[[metric_name]])))
  print("typical_enhancers = ")
  print(b)
  
  # First, calculate global whiskers
  whiskers <- rel_data %>%
    group_by(cluster_type) %>%
    summarise(
      Q1 = quantile(.data[[metric_name]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[metric_name]], 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_whisker = Q1 - 1.5 * IQR,
      upper_whisker = Q3 + 1.5 * IQR,
      .groups = "drop"
    )
  
  global_min <- min(whiskers$lower_whisker, na.rm = TRUE)
  global_max <- max(whiskers$upper_whisker, na.rm = TRUE)
  range_padding <- 0.05 * (global_max - global_min)
  
  y_lower <- global_min - range_padding
  y_upper <- global_max + range_padding
  
  # Now, the plot
  p3<-ggplot(rel_data, aes(x = LA1_type, y = .data[[metric_name]], fill = LA1_type)) +
    geom_boxplot(
      # width = 0.45,                            # Make boxes narrower
      # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c(
      "super enhancer" = "#af8dc3",
      "typical enhancer" = "#5ab4ac"
    ))+
    scale_x_discrete(labels = c(
      "super enhancer" = "Super Enhancers",
      "typical enhancer" = "Typical Enhancers"
    ))+
    coord_cartesian(ylim = c(0, y_upper)) +
    scale_y_continuous(
      breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
      # , expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal() +
    theme(legend.position = "none")+
    labs(x = "", y = y_axis_name, fill = "LA1 Type")+
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.6),
      axis.line.y = element_line(color = "black", size = 0.6),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
      axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
      axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  ggsave(paste0(file_prefix, "_p2.svg"), plot = p3, width = 4, height = 4, dpi = 1200)
  
  p4<-ggplot(rel_data, aes(x = LA1_type, y = .data[[metric_name]], fill = LA1_type)) +
    geom_boxplot(
      # width = 0.45,                            # Make boxes narrower
      # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c(
      "super enhancer" = "#af8dc3",
      "typical enhancer" = "#5ab4ac"
    ))+
    scale_x_discrete(labels = c(
      "super enhancer" = "Super Enhancers",
      "typical enhancer" = "Typical Enhancers"
    ))+
    coord_cartesian(ylim = c(0, y_upper)) +
    scale_y_continuous(
      breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
      # , expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal() +
    theme(legend.position = "none")+
    labs(x = "Cluster Type", y = metric_name, fill = "LA1 Type")+
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.6),
      axis.line.y = element_line(color = "black", size = 0.6),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
      axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
      axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  print(p1)
  # print(p2)
  print(p3)
  # print(p4)
  
  library(dplyr)
  
  # Overall comparison between super enhancer and typical enhancer
  cat("\n=== Overall Comparison: Super Enhancer vs Typical Enhancer ===\n")
  overall_test <- wilcox.test(
    rel_data %>% filter(LA1_type == "super enhancer") %>% pull(.data[[metric_name]]),
    rel_data %>% filter(LA1_type == "typical enhancer") %>% pull(.data[[metric_name]])
  )
  cat("W statistic: ", overall_test$statistic, "\n")
  cat("p-value: ", overall_test$p.value, "\n")
  
  cat("\n------------------------------------------------------------\n")
  
  # Cluster-wise comparisons
  cat("\n=== Cluster-wise Comparisons ===\n")
  
  # Comparison between P-SE and P-TE clusters
  cat("\n-- P-SE vs P-TE --\n")
  pse_pte_test <- wilcox.test(
    rel_data %>% filter(cluster_type == "P-SE") %>% pull(.data[[metric_name]]),
    rel_data %>% filter(cluster_type == "P-TE") %>% pull(.data[[metric_name]])
  )
  cat("W statistic: ", pse_pte_test$statistic, "\n")
  cat("p-value: ", pse_pte_test$p.value, "\n")
  
  # Comparison within P-SE-TE cluster between super and typical enhancers
  cat("\n-- Within P-SE-TE: Super Enhancer vs Typical Enhancer --\n")
  pse_te_test <- wilcox.test(
    rel_data %>% filter(cluster_type == "P-SE-TE", LA1_type == "super enhancer") %>% pull(.data[[metric_name]]),
    rel_data %>% filter(cluster_type == "P-SE-TE", LA1_type == "typical enhancer") %>% pull(.data[[metric_name]])
  )
  cat("W statistic: ", pse_te_test$statistic, "\n")
  cat("p-value: ", pse_te_test$p.value, "\n")
  
}

# rel_data %>% head()
plot_func(data_frame = wt0_closeness, metric_name = "closeness",y_axis_name = "Closeness", by_inp = 3,file_prefix = "wt0_closeness")

plot_func(data_frame = wt0_degree, metric_name = "degree",y_axis_name = "Degree", by_inp = 5, file_prefix = "wt0_degree")

plot_func(data_frame = wt0_strength, metric_name = "strength",y_axis_name = "Strength", by_inp = 20, file_prefix = "wt0_strength")

plot_func(data_frame = wt0_betweenness, metric_name = "betweenness",y_axis_name = "Betweenness", by_inp = 5,file_prefix = "wt0_betweenness")

plot_func(data_frame = wt0_eigen, metric_name = "eigen_centrality",y_axis_name = "Eigen Vector Centrality", by_inp = 0.6,file_prefix = "wt0_eigen")

plot_func(data_frame = wt0_harmonic, metric_name = "harmonic_centrality",y_axis_name = "Harmonic Centrality", by_inp = 50, file_prefix = "wt0_harmonic")

# normalised metrics
plot_func(data_frame = wt0_norm_closeness, metric_name = "closeness",y_axis_name = "Closeness", by_inp = 3,file_prefix = "wt0_norm_closeness")
plot_func(data_frame = wt0_norm_betweenness, metric_name = "betweenness",y_axis_name = "Betweenness", by_inp = 3,file_prefix = "wt0_norm_betweenness")

# ###############
# 
# data_frame = wt0_norm_closeness
# metric_name = "closeness"
# y_axis_name = "Closeness"
# by_inp = 3
# file_prefix = "wt0_closeness"
# 
# rel_data<-data_frame %>% filter(cluster_type %in% c("P-SE", "P-TE","P-SE-TE")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer"))
# 
# print("Cluster - wise")
# print(rel_data %>% group_by(cluster_type, LA1_type) %>% summarise(Q1=quantile(.data[[metric_name]], 0.25, na.rm=T),
#                                                                   median=quantile(.data[[metric_name]], 0.5, na.rm=T),
#                                                                   Q3=quantile(.data[[metric_name]], 0.75, na.rm=T)))
# 
# rel_data %>% head()
# # First, calculate global whiskers
# whiskers <- rel_data %>%
#   group_by(cluster_type, LA1_type) %>%
#   summarise(
#     Q1 = quantile(.data[[metric_name]], 0.25, na.rm = TRUE),
#     Q3 = quantile(.data[[metric_name]], 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1,
#     lower_whisker = Q1 - 1.5 * IQR,
#     upper_whisker = Q3 + 1.5 * IQR,
#     .groups = "drop"
#   )
# 
# global_min <- min(whiskers$lower_whisker, na.rm = TRUE)
# global_max <- max(whiskers$upper_whisker, na.rm = TRUE)
# range_padding <- 0.05 * (global_max - global_min)
# 
# y_lower <- global_min - range_padding
# y_upper <- global_max + range_padding
# 
# rel_data$cluster_type <- factor(rel_data$cluster_type, levels = c("P-SE", "P-TE", "P-SE-TE"))
# 
# # Now, the plot
# p1<-ggplot(rel_data, aes(x = cluster_type, y = .data[[metric_name]], fill = LA1_type)) +
#   geom_boxplot(
#     # width = 0.45,                            # Make boxes narrower
#     # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
#     outlier.shape = NA
#   ) +
#   scale_fill_manual(values = c(
#     "super enhancer" = "#af8dc3",
#     "typical enhancer" = "#5ab4ac"
#   ))+
#   scale_x_discrete(labels = c(
#     "super enhancer" = "Super Enhancers",
#     "typical enhancer" = "Typical Enhancers"
#   ))+
#   coord_cartesian(ylim = c(0, y_upper)) +
#   scale_y_continuous(
#     breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
#     # , expand = expansion(mult = c(0, 0))
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")+
#   labs(x = "", y = y_axis_name, fill = "LA1 Type")+
#   theme(
#     panel.background = element_rect(fill = "white", colour = NA),
#     plot.background = element_rect(fill = "white", colour = NA),
#     legend.background = element_rect(fill = "white", colour = NA),
#     legend.box.background = element_rect(fill = "white", colour = NA),
#     panel.grid = element_blank(),
#     axis.line.x = element_line(color = "black", size = 0.6),
#     axis.line.y = element_line(color = "black", size = 0.6),
#     axis.ticks.x = element_blank(),
#     axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
#     axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
#     axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
#     plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
#   )
# summary(rel_data[[metric_name]])
# 
# print(p1)
# ggsave(paste0(file_prefix, "_p1.svg"),plot = p1, width = 4, height = 4, dpi = 1200)
# 
# p2<-ggplot(rel_data, aes(x = cluster_type, y = .data[[metric_name]], fill = LA1_type)) +
#   geom_boxplot(
#     # width = 0.45,                            # Make boxes narrower
#     # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
#     outlier.shape = NA
#   ) +
#   scale_fill_manual(values = c(
#     "super enhancer" = "#af8dc3",
#     "typical enhancer" = "#5ab4ac"
#   ))+
#   scale_x_discrete(labels = c(
#     "super enhancer" = "Super Enhancers",
#     "typical enhancer" = "Typical Enhancers"
#   ))+
#   coord_cartesian(ylim = c(0, y_upper)) +
#   scale_y_continuous(
#     breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
#     # , expand = expansion(mult = c(0, 0))
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")+
#   labs(x = "Cluster Type", y = metric_name, fill = "LA1 Type")+
#   theme(
#     panel.background = element_rect(fill = "white", colour = NA),
#     plot.background = element_rect(fill = "white", colour = NA),
#     legend.background = element_rect(fill = "white", colour = NA),
#     legend.box.background = element_rect(fill = "white", colour = NA),
#     panel.grid = element_blank(),
#     axis.line.x = element_line(color = "black", size = 0.6),
#     axis.line.y = element_line(color = "black", size = 0.6),
#     axis.ticks.x = element_blank(),
#     axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
#     axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
#     axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
#     plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
#   )
# 
# 
# # plot the quantiles of the data 
# a<-quantile((rel_data %>% filter(LA1_type=="super enhancer") %>% select(metric_name) %>% pull(.data[[metric_name]])))
# print("super_enhancers = ")
# print(a)
# b<-quantile((rel_data %>% filter(LA1_type=="typical enhancer") %>% select(metric_name) %>% pull(.data[[metric_name]])))
# print("typical_enhancers = ")
# print(b)
# 
# # First, calculate global whiskers
# whiskers <- rel_data %>%
#   group_by(cluster_type) %>%
#   summarise(
#     Q1 = quantile(.data[[metric_name]], 0.25, na.rm = TRUE),
#     Q3 = quantile(.data[[metric_name]], 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1,
#     lower_whisker = Q1 - 1.5 * IQR,
#     upper_whisker = Q3 + 1.5 * IQR,
#     .groups = "drop"
#   )
# 
# global_min <- min(whiskers$lower_whisker, na.rm = TRUE)
# global_max <- max(whiskers$upper_whisker, na.rm = TRUE)
# range_padding <- 0.05 * (global_max - global_min)
# 
# y_lower <- global_min - range_padding
# y_upper <- global_max + range_padding
# 
# # Now, the plot
# p3<-ggplot(rel_data, aes(x = LA1_type, y = .data[[metric_name]], fill = LA1_type)) +
#   geom_boxplot(
#     # width = 0.45,                            # Make boxes narrower
#     # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
#     outlier.shape = NA
#   ) +
#   scale_fill_manual(values = c(
#     "super enhancer" = "#af8dc3",
#     "typical enhancer" = "#5ab4ac"
#   ))+
#   scale_x_discrete(labels = c(
#     "super enhancer" = "Super Enhancers",
#     "typical enhancer" = "Typical Enhancers"
#   ))+
#   coord_cartesian(ylim = c(0, y_upper)) +
#   scale_y_continuous(
#     breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
#     # , expand = expansion(mult = c(0, 0))
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")+
#   labs(x = "", y = y_axis_name, fill = "LA1 Type")+
#   theme(
#     panel.background = element_rect(fill = "white", colour = NA),
#     plot.background = element_rect(fill = "white", colour = NA),
#     legend.background = element_rect(fill = "white", colour = NA),
#     legend.box.background = element_rect(fill = "white", colour = NA),
#     panel.grid = element_blank(),
#     axis.line.x = element_line(color = "black", size = 0.6),
#     axis.line.y = element_line(color = "black", size = 0.6),
#     axis.ticks.x = element_blank(),
#     axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
#     axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
#     axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
#     plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
#   )
# ggsave(paste0(file_prefix, "_p2.svg"), plot = p3, width = 4, height = 4, dpi = 1200)
# 
# p4<-ggplot(rel_data, aes(x = LA1_type, y = .data[[metric_name]], fill = LA1_type)) +
#   geom_boxplot(
#     # width = 0.45,                            # Make boxes narrower
#     # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
#     outlier.shape = NA
#   ) +
#   scale_fill_manual(values = c(
#     "super enhancer" = "#af8dc3",
#     "typical enhancer" = "#5ab4ac"
#   ))+
#   scale_x_discrete(labels = c(
#     "super enhancer" = "Super Enhancers",
#     "typical enhancer" = "Typical Enhancers"
#   ))+
#   coord_cartesian(ylim = c(0, y_upper)) +
#   scale_y_continuous(
#     breaks = seq(floor(y_lower), ceiling(y_upper), by=by_inp)
#     # , expand = expansion(mult = c(0, 0))
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")+
#   labs(x = "Cluster Type", y = metric_name, fill = "LA1 Type")+
#   theme(
#     panel.background = element_rect(fill = "white", colour = NA),
#     plot.background = element_rect(fill = "white", colour = NA),
#     legend.background = element_rect(fill = "white", colour = NA),
#     legend.box.background = element_rect(fill = "white", colour = NA),
#     panel.grid = element_blank(),
#     axis.line.x = element_line(color = "black", size = 0.6),
#     axis.line.y = element_line(color = "black", size = 0.6),
#     axis.ticks.x = element_blank(),
#     axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
#     axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
#     axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
#     plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
#   )
# print(p1)
# # print(p2)
# print(p3)
# # print(p4)
# 
# library(dplyr)
# 
# # Overall comparison between super enhancer and typical enhancer
# cat("\n=== Overall Comparison: Super Enhancer vs Typical Enhancer ===\n")
# overall_test <- wilcox.test(
#   rel_data %>% filter(LA1_type == "super enhancer") %>% pull(.data[[metric_name]]),
#   rel_data %>% filter(LA1_type == "typical enhancer") %>% pull(.data[[metric_name]])
# )
# cat("W statistic: ", overall_test$statistic, "\n")
# cat("p-value: ", overall_test$p.value, "\n")
# 
# cat("\n------------------------------------------------------------\n")
# 
# # Cluster-wise comparisons
# cat("\n=== Cluster-wise Comparisons ===\n")
# 
# # Comparison between P-SE and P-TE clusters
# cat("\n-- P-SE vs P-TE --\n")
# pse_pte_test <- wilcox.test(
#   rel_data %>% filter(cluster_type == "P-SE") %>% pull(.data[[metric_name]]),
#   rel_data %>% filter(cluster_type == "P-TE") %>% pull(.data[[metric_name]])
# )
# cat("W statistic: ", pse_pte_test$statistic, "\n")
# cat("p-value: ", pse_pte_test$p.value, "\n")
# 
# # Comparison within P-SE-TE cluster between super and typical enhancers
# cat("\n-- Within P-SE-TE: Super Enhancer vs Typical Enhancer --\n")
# pse_te_test <- wilcox.test(
#   rel_data %>% filter(cluster_type == "P-SE-TE", LA1_type == "super enhancer") %>% pull(.data[[metric_name]]),
#   rel_data %>% filter(cluster_type == "P-SE-TE", LA1_type == "typical enhancer") %>% pull(.data[[metric_name]])
# )
# cat("W statistic: ", pse_te_test$statistic, "\n")
# cat("p-value: ", pse_te_test$p.value, "\n")
# ##################


# plot_func(data_frame = wt0_norm_betweenness, metric_name = "betweenness",y_axis_name = "Normalised Betweenness", by_inp = 1)
# plot_func(data_frame = wt0_norm_closeness, metric_name = "closeness",y_axis_name = "Normalised Closeness", by_inp = 1)
# plot_func(data_frame = wt0_norm_degree, metric_name = "degree",y_axis_name = "Normalised Degree", by_inp = 1)
# plot_func(data_frame = wt0_norm_harmonic, metric_name = "harmonic_centrality",y_axis_name = "Normalised Harmonic Centrality", by_inp = 1)

plot_metric_trends <- function(metric_name, a_wt0, a_wt360, a_wt4320, by_inp, y_axis_name, file_prefix) {
  # Extract metric
  wt0 <- a_wt0[[metric_name]] %>% mutate(data = "0hr")
  wt360 <- a_wt360[[metric_name]] %>% mutate(data = "6hr")
  wt4320 <- a_wt4320[[metric_name]] %>% mutate(data = "72hr")
  
  # Combine
  full_df <- bind_rows(wt0, wt360, wt4320) %>%
    mutate(data_LA1 = paste(data, LA1_type, sep = "_"))
  
  whiskers <- full_df %>%
    group_by(data, LA1_type) %>%
    summarise(
      Q1 = quantile(.data[[metric_name]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[metric_name]], 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_whisker = Q1 - 1.5 * IQR,
      upper_whisker = Q3 + 1.5 * IQR,
      .groups = "drop"
    )
  
  global_min <- min(whiskers$lower_whisker, na.rm = TRUE)
  global_max <- max(whiskers$upper_whisker, na.rm = TRUE)
  range_padding <- 0.05 * (global_max - global_min)
  
  y_lower <- global_min - range_padding
  y_upper <- global_max + range_padding
  p1<-ggplot(full_df, aes(x = LA1_type, y = .data[[metric_name]], fill = data_LA1)) +
    geom_boxplot(
      # width = 0.45,                            # Make boxes narrower
      # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c(
      "0hr_super enhancer" = "#af8dc3",
      "0hr_typical enhancer" = "#5ab4ac",
      "6hr_super enhancer" = "#c7afd5",
      "6hr_typical enhancer" = "#8bcac5",
      "72hr_super enhancer" = "#dfd1e7",
      "72hr_typical enhancer" = "#bde1de"
    ))+
    scale_x_discrete(labels = c(
      "super enhancer" = "Super Enhancers",
      "typical enhancer" = "Typical Enhancers"
    ))+
    coord_cartesian(ylim = c(0, y_upper)) +
    scale_y_continuous(
      breaks = seq(0,y_upper , by=by_inp)
      # , expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal() +
    theme(legend.position = "none")+
    labs(x = "", y = y_axis_name, fill = "LA1 Type")+
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.6),
      axis.line.y = element_line(color = "black", size = 0.6),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
      axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
      axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  library(dplyr)
  print(p1)
  ggsave(paste0(file_prefix,"wt0_combined_plot.svg"), p1, dpi=1200, width = 4, height = 4)
  # Kruskal-Wallis Test: Comparing across all groups defined by 'data_LA1'
  cat("\n=== Kruskal-Wallis Test Across All Groups (data_LA1) ===\n")
  
  kruskal_test <- kruskal.test(
    formula = as.formula(paste(metric_name, "~ data_LA1")),
    data = full_df
  )
  
  cat("Chi-squared statistic: ", kruskal_test$statistic, "\n")
  cat("Degrees of freedom: ", kruskal_test$parameter, "\n")
  cat("p-value: ", kruskal_test$p.value, "\n")
  
  cat("\n------------------------------------------------------------\n")
  
  # Kruskal-Wallis Test within each LA1_type separately (super enhancer vs typical enhancer) over time
  cat("\n=== Kruskal-Wallis Test Within Each LA1 Type ===\n")
  
  # For Super Enhancers over time
  cat("\n-- Super Enhancers across Timepoints --\n")
  kruskal_super <- kruskal.test(
    formula = as.formula(paste(metric_name, "~ data")),
    data = full_df %>% filter(LA1_type == "super enhancer")
  )
  cat("Chi-squared statistic: ", kruskal_super$statistic, "\n")
  cat("Degrees of freedom: ", kruskal_super$parameter, "\n")
  cat("p-value: ", kruskal_super$p.value, "\n")
  
  # For Typical Enhancers over time
  cat("\n-- Typical Enhancers across Timepoints --\n")
  kruskal_typical <- kruskal.test(
    formula = as.formula(paste(metric_name, "~ data")),
    data = full_df %>% filter(LA1_type == "typical enhancer")
  )
  cat("Chi-squared statistic: ", kruskal_typical$statistic, "\n")
  cat("Degrees of freedom: ", kruskal_typical$parameter, "\n")
  cat("p-value: ", kruskal_typical$p.value, "\n")
  
  
  cat("-----------------------------------------------------------------------------------------","\n")
  # # Kruskal-Wallis Tests
  # cat("\nKruskal-Wallis test results for SUPER enhancers:\n")
  # print(kruskal.test(as.formula(paste0(metric_name, " ~ data")),
  #                    data = full_df %>% filter(LA1_type == "super enhancer")))
  # 
  # cat("\nKruskal-Wallis test results for TYPICAL enhancers:\n")
  # print(kruskal.test(as.formula(paste0(metric_name, " ~ data")),
  #                    data = full_df %>% filter(LA1_type == "typical enhancer")))
  
  return(invisible(full_df))
}

plot_metric_trends(metric_name = "closeness", a_wt0, a_wt360, a_wt4320, by_inp = 1, y_axis_name = "Closeness", file_prefix = "combined_closeness")

plot_metric_trends(metric_name = "degree", a_wt0, a_wt360, a_wt4320, by_inp = 1, y_axis_name = "Degree", file_prefix = "combined_degree")

plot_metric_trends(metric_name = "betweenness", a_wt0, a_wt360, a_wt4320, by_inp = 1, y_axis_name = "Betweenness", file_prefix = "combined_betweenness")

plot_metric_trends(metric_name = "strength", a_wt0, a_wt360, a_wt4320, by_inp = 5, y_axis_name = "Strength", file_prefix = "combined_strength")


metric_name<-"harmonic"
  # Extract metric
  wt0 <- a_wt0[[metric_name]] %>% mutate(data = "0hr")
  wt360 <- a_wt360[[metric_name]] %>% mutate(data = "6hr")
  wt4320 <- a_wt4320[[metric_name]] %>% mutate(data = "72hr")
  
  # Combine
  full_df <- bind_rows(wt0, wt360, wt4320) %>%
    mutate(data_LA1 = paste(data, LA1_type, sep = "_"))
  metric_name<-"harmonic_centrality"
  whiskers <- full_df %>%
    group_by(data, LA1_type) %>%
    summarise(
      Q1 = quantile(.data[[metric_name]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[metric_name]], 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_whisker = Q1 - 1.5 * IQR,
      upper_whisker = Q3 + 1.5 * IQR,
      .groups = "drop"
    )
  
  global_min <- min(whiskers$lower_whisker, na.rm = TRUE)
  global_max <- max(whiskers$upper_whisker, na.rm = TRUE)
  range_padding <- 0.05 * (global_max - global_min)
  
  y_lower <- global_min - range_padding
  y_upper <- global_max + range_padding
  
  by_inp<-20
  y_axis_name<-"Harmonic Centrality"
  p1<-ggplot(full_df, aes(x = LA1_type, y = .data[[metric_name]], fill = data_LA1)) +
    geom_boxplot(
      # width = 0.45,                            # Make boxes narrower
      # position = position_dodge(width = 0.5),  # Dodge boxes with some gap
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c(
      "0hr_super enhancer" = "#af8dc3",
      "0hr_typical enhancer" = "#5ab4ac",
      "6hr_super enhancer" = "#c7afd5",
      "6hr_typical enhancer" = "#8bcac5",
      "72hr_super enhancer" = "#dfd1e7",
      "72hr_typical enhancer" = "#bde1de"
    ))+
    scale_x_discrete(labels = c(
      "super enhancer" = "Super Enhancers",
      "typical enhancer" = "Typical Enhancers"
    ))+
    coord_cartesian(ylim = c(0, y_upper)) +
    scale_y_continuous(
      breaks = seq(0,y_upper , by=by_inp)
      # , expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal() +
    theme(legend.position = "none")+
    labs(x = "", y = y_axis_name, fill = "LA1 Type")+
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.6),
      axis.line.y = element_line(color = "black", size = 0.6),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
      axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
      axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  library(dplyr)
  print(p1)
  file_prefix<-"harmonic_combined"
  ggsave(paste0(file_prefix,"wt0_combined_plot.svg"), p1, dpi=1200, width = 4, height = 4)
  # Kruskal-Wallis Test: Comparing across all groups defined by 'data_LA1'
  cat("\n=== Kruskal-Wallis Test Across All Groups (data_LA1) ===\n")
  
  kruskal_test <- kruskal.test(
    formula = as.formula(paste(metric_name, "~ data_LA1")),
    data = full_df
  )
  
  cat("Chi-squared statistic: ", kruskal_test$statistic, "\n")
  cat("Degrees of freedom: ", kruskal_test$parameter, "\n")
  cat("p-value: ", kruskal_test$p.value, "\n")
  
  cat("\n------------------------------------------------------------\n")
  
  # Kruskal-Wallis Test within each LA1_type separately (super enhancer vs typical enhancer) over time
  cat("\n=== Kruskal-Wallis Test Within Each LA1 Type ===\n")
  
  # For Super Enhancers over time
  cat("\n-- Super Enhancers across Timepoints --\n")
  kruskal_super <- kruskal.test(
    formula = as.formula(paste(metric_name, "~ data")),
    data = full_df %>% filter(LA1_type == "super enhancer")
  )
  cat("Chi-squared statistic: ", kruskal_super$statistic, "\n")
  cat("Degrees of freedom: ", kruskal_super$parameter, "\n")
  cat("p-value: ", kruskal_super$p.value, "\n")
  
  # For Typical Enhancers over time
  cat("\n-- Typical Enhancers across Timepoints --\n")
  kruskal_typical <- kruskal.test(
    formula = as.formula(paste(metric_name, "~ data")),
    data = full_df %>% filter(LA1_type == "typical enhancer")
  )
  cat("Chi-squared statistic: ", kruskal_typical$statistic, "\n")
  cat("Degrees of freedom: ", kruskal_typical$parameter, "\n")
  cat("p-value: ", kruskal_typical$p.value, "\n")
  
  
  cat("-----------------------------------------------------------------------------------------","\n")
  # # Kruskal-Wallis Tests
  # cat("\nKruskal-Wallis test results for SUPER enhancers:\n")
  # print(kruskal.test(as.formula(paste0(metric_name, " ~ data")),
  #                    data = full_df %>% filter(LA1_type == "super enhancer")))
  # 
  # cat("\nKruskal-Wallis test results for TYPICAL enhancers:\n")
  # print(kruskal.test(as.formula(paste0(metric_name, " ~ data")),
  #                    data = full_df %>% filter(LA1_type == "typical enhancer")))
  
  # return(invisible(full_df))


# plot_metric_trends(metric_name = "harmonic_", a_wt0, a_wt360, a_wt4320, by_inp = 5, y_axis_name = "Strength")

# plotting th e jaccard indices
# Get list of super enhancers and typical enhancers
se_nodes <- V(g_wt0)[LA1_type == "super enhancer"]$name
te_nodes <- V(g_wt0)[LA1_type == "typical enhancer"]$name

# neighbors(g_wt0, "ENSR00000659605", mode = "all")$name
# Jaccard similarity function

jaccard_similarity <- function(set1, set2) {
  length(intersect(set1, set2)) / length(union(set1, set2))
}

# Function to compute jaccard for a node across networks
get_node_jaccard <- function(node, g1, g2) {
  n1 <- neighbors(g1, node, mode = "all")$name
  n2 <- neighbors(g2, node, mode = "all")$name
  jaccard_similarity(n1, n2)
}

# # Example: compute for all SE nodes
# jaccard_df <- lapply(se_nodes, function(node) {
#   data.frame(
#     node = se_nodes,
#     type = "super enhancer",
#     j_0_6 = get_node_jaccard(node, g_wt0, g_wt360),
#     j_6_72 = get_node_jaccard(node, g_wt360, g_wt4320),
#     j_0_72 = get_node_jaccard(node, g_wt0, g_wt4320)
#   )
# }) %>% bind_rows()

# jaccard_df <- lapply(se_nodes, function(node) {
#   data.frame(
#     node = node,                      # Correct: only the current node
#     type = "super enhancer",          # Label it as a super enhancer
#     j_0_6 = get_node_jaccard(node, g_wt0, g_wt360),     
#     j_6_72 = get_node_jaccard(node, g_wt360, g_wt4320),
#     j_0_72 = get_node_jaccard(node, g_wt0, g_wt4320)
#   )
# }) %>% bind_rows()
# se_nodes %>% head()
# Get vertex names from each graph

nodes_0 <- V(g_wt0)$name
nodes_360 <- V(g_wt360)$name
nodes_4320 <- V(g_wt4320)$name

# Keep only SE nodes present in all graphs
common_se_nodes <- se_nodes[se_nodes %in% nodes_0 & se_nodes %in% nodes_360 & se_nodes %in% nodes_4320]
jaccard_df_se <- lapply(common_se_nodes, function(node) {
  data.frame(
    node = node,
    type = "super enhancer",
    j_0_6 = get_node_jaccard(node, g_wt0, g_wt360),     
    j_6_72 = get_node_jaccard(node, g_wt360, g_wt4320),
    j_0_72 = get_node_jaccard(node, g_wt0, g_wt4320)
  )
}) %>% bind_rows()

common_te_nodes <- te_nodes[te_nodes %in% nodes_0 & te_nodes %in% nodes_360 & te_nodes %in% nodes_4320]
jaccard_df_te <- lapply(common_te_nodes, function(node) {
  data.frame(
    node = node,
    type = "typical enhancer",
    j_0_6 = get_node_jaccard(node, g_wt0, g_wt360),     
    j_6_72 = get_node_jaccard(node, g_wt360, g_wt4320),
    j_0_72 = get_node_jaccard(node, g_wt0, g_wt4320)
  )
}) %>% bind_rows()
jaccard_df<-rbind(jaccard_df_se, jaccard_df_te)
jaccard_long <- jaccard_df %>%
  pivot_longer(
    cols = starts_with("j_"),
    names_to = "comparison",
    values_to = "jaccard"
  ) %>%
  mutate(comparison = recode(comparison,
                             j_0_6 = "0hr vs 6hr",
                             j_6_72 = "6hr vs 72hr",
                             j_0_72 = "0hr vs 72hr"))

# Boxplot
ggplot(jaccard_long, aes(x = comparison, y = jaccard, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  labs(
    title = "Jaccard Similarity of Node Partners Over Time",
    x = "",
    y = "Jaccard Similarity Index",
    fill = "Enhancer Type"
  ) +
  scale_fill_manual(values = c(
    "super enhancer" = "#af8dc3",
    "typical enhancer" = "#5ab4ac"
  )) +
  theme_minimal(base_size = 14)+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.6),
    axis.line.y = element_line(color = "black", size = 0.6),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = "black", linetype = "dashed", size = 0.5),
    axis.text.x = element_text(face = "bold", size = 12),         # X-axis tick labels
    axis.title.y = element_text(face = "bold", size = 16),        # Y-axis label
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )+
  theme(legend.position = "None")

ggsave("jaccard_indices_plot.svg", dpi=1200, width = 6, height = 4)

jaccard_long %>% head()

wilcox.test(jaccard_long %>% filter(type=="super enhancer", comparison=="0hr vs 6hr") %>% pull(jaccard),
            jaccard_long %>% filter(type=="typical enhancer", comparison=="0hr vs 6hr") %>% pull(jaccard))

wilcox.test(jaccard_long %>% filter(type=="super enhancer", comparison=="0hr vs 72hr") %>% pull(jaccard),
            jaccard_long %>% filter(type=="typical enhancer", comparison=="0hr vs 72hr") %>% pull(jaccard))

wilcox.test(jaccard_long %>% filter(type=="super enhancer", comparison=="6hr vs 72hr") %>% pull(jaccard),
            jaccard_long %>% filter(type=="typical enhancer", comparison=="6hr vs 72hr") %>% pull(jaccard))

######################################
library(ggplot2)

# Filter for relevant enhancer types if needed (optional, in case other types are present)
jaccard_long %>% select(type) %>% unique()
filtered_data <- jaccard_long %>% 
  filter(comparison=="6hr vs 72hr")

# Density plot
ggplot(filtered_data, aes(x = jaccard, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Jaccard Index",
    x = "Jaccard Index",
    y = "Density",
    fill = "Enhancer Type"
  ) +
  theme_minimal(base_size = 14)

