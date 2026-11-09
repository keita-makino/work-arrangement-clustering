database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

work_matrix <- readRDS(
  file.path(
    "data",
    "work_matrix.rds"
  )
)

# work_matrix.rds is saved before the sample filters; keep the final sample only
joined <- database %>%
  select(
    row_index,
    cluster
  ) %>%
  mutate(
    row_index = as.numeric(row_index)
  ) %>%
  inner_join(
    work_matrix,
    by = "row_index"
  )

stopifnot(nrow(joined) == nrow(database))

cluster_labels <- c(
  "Primary",
  "Temporary",
  "Home",
  "Primary-Home",
  "Home-Primary",
  "All-Mixed"
)

places <- c("Primary", "Temporary", "Home")

day_labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

period_labels <- c(
  "12AM-6AM",
  "6AM-8AM",
  "8AM-10AM",
  "10AM-12PM",
  "12PM-2PM",
  "2PM-4PM",
  "4PM-6PM",
  "6PM-8PM",
  "8PM-10PM",
  "10PM-12AM"
)

# Whether each worker worked at each place
# on the ith day of the week, jth period of the day
# work_matrix_i_j is a bit code: 4 - Primary, 2 - Temporary, 1 - Home
long <- joined %>%
  pivot_longer(
    cols = starts_with("work_matrix_"),
    names_to = c("day", "period"),
    names_pattern = "work_matrix_(\\d{1,2})_(\\d{1,2})",
    names_transform = as.numeric,
    values_to = "value"
  ) %>%
  mutate(
    Primary = bitwAnd(value, 4) > 0,
    Temporary = bitwAnd(value, 2) > 0,
    Home = bitwAnd(value, 1) > 0
  ) %>%
  pivot_longer(
    cols = all_of(places),
    names_to = "place",
    values_to = "worked"
  )

# Share of workers in each cluster who worked at each place in each cell
# (denominator: all workers in the cluster)
cell_share <- long %>%
  group_by(cluster, day, period, place) %>%
  summarise(
    share = mean(worked),
    .groups = "drop"
  )

# Share of workers in each cluster who worked at each place
# in at least one period of the day
daily_share <- long %>%
  group_by(cluster, row_index, day, place) %>%
  summarise(
    worked = any(worked),
    .groups = "drop"
  ) %>%
  group_by(cluster, day, place) %>%
  summarise(
    daily_share = mean(worked),
    .groups = "drop"
  )

# Daily share table: one 3 (place) x 7 (day) block per cluster, stacked vertically
daily_share %>%
  mutate(
    cluster = factor(cluster, levels = seq_along(cluster_labels), labels = cluster_labels),
    place = factor(place, levels = places),
    day = factor(day, levels = seq_along(day_labels), labels = day_labels),
    daily_share = round(daily_share * 100, 1)
  ) %>%
  arrange(cluster, place, day) %>%
  pivot_wider(
    names_from = day,
    values_from = daily_share
  ) %>%
  write_csv(
    file.path(
      "dist",
      "descriptive",
      "daily_share_by_cluster.csv"
    )
  )

# x is unique per day so that each panel can carry its own daily share label
plot_data <- cell_share %>%
  left_join(
    daily_share,
    by = c("cluster", "day", "place")
  ) %>%
  mutate(
    place = factor(place, levels = places),
    x = paste(day, place, sep = "_"),
    x_label = paste0(
      place,
      "\n",
      scales::percent(daily_share, accuracy = 1)
    ),
    cell_label = scales::percent(share, accuracy = 1)
  ) %>%
  arrange(day, place) %>%
  mutate(
    x = factor(x, levels = unique(x))
  )

for (k in seq_along(cluster_labels)) {
  data_k <- plot_data %>%
    filter(cluster == k)

  x_labels <- data_k %>%
    distinct(x, x_label) %>%
    deframe()

  n_k <- sum(joined$cluster == k)

  p <- data_k %>%
    ggplot(
      aes(
        x = x,
        y = period,
        fill = share
      )
    ) +
    geom_tile() +
    geom_text(
      aes(
        label = cell_label,
        color = share > 0.6
      ),
      size = 2.5
    ) +
    facet_wrap(
      ~day,
      nrow = 1,
      scales = "free_x",
      labeller = labeller(day = function(x) day_labels[as.numeric(x)])
    ) +
    scale_x_discrete(
      labels = x_labels,
      expand = c(0, 0)
    ) +
    scale_y_reverse(
      breaks = 1:10,
      labels = period_labels,
      expand = c(0, 0)
    ) +
    scale_fill_viridis_c(
      limits = c(0, 1)
    ) +
    scale_color_manual(
      values = c("TRUE" = "black", "FALSE" = "white"),
      guide = "none"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0(cluster_labels[k], " (n = ", n_k, ")"),
      x = NULL,
      y = "Time of day"
    )

  ggsave(
    filename = file.path(
      "dist",
      "descriptive",
      paste0("heatmap_work_matrix_cluster_", k, ".png")
    ),
    plot = p,
    width = 14,
    height = 6
  )
}

# Appendix-size versions of the same heatmaps (9 x 2.7 inch, about 6pt text),
# so that two of them fit on one landscape page of the manuscript;
# the cluster name and n are given in the figure caption instead of the title
for (k in seq_along(cluster_labels)) {
  data_k <- plot_data %>%
    filter(cluster == k)

  x_labels <- data_k %>%
    distinct(x, x_label) %>%
    deframe()

  p <- data_k %>%
    ggplot(
      aes(
        x = x,
        y = period,
        fill = share
      )
    ) +
    geom_tile() +
    geom_text(
      aes(
        label = cell_label,
        color = share > 0.6
      ),
      size = 2
    ) +
    facet_wrap(
      ~day,
      nrow = 1,
      scales = "free_x",
      labeller = labeller(day = function(x) day_labels[as.numeric(x)])
    ) +
    scale_x_discrete(
      labels = x_labels,
      expand = c(0, 0)
    ) +
    scale_y_reverse(
      breaks = 1:10,
      labels = period_labels,
      expand = c(0, 0)
    ) +
    scale_fill_viridis_c(
      limits = c(0, 1)
    ) +
    scale_color_manual(
      values = c("TRUE" = "black", "FALSE" = "white"),
      guide = "none"
    ) +
    theme_bw(base_size = 7) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      axis.text.x = element_text(size = 5.5),
      axis.text.y = element_text(size = 6)
    ) +
    labs(
      x = NULL,
      y = "Time of day"
    )

  ggsave(
    filename = file.path(
      "dist",
      "descriptive",
      paste0("heatmap_work_matrix_cluster_", k, "_appendix.png")
    ),
    plot = p,
    width = 9,
    height = 2.7
  )
}
