clustering = function(data, n_clusters) {
  set.seed(42)

  km <- kmeans(
    data %>%
      select(
        visited_total_1,
        visited_total_2,
        visited_total_3
      ),
    n_clusters,
    nstart = 50,
    iter.max = 1000
  )

  forward_mapper = km$centers %>%
    as_tibble() %>%
    mutate(
      cluster_index = row_number(),
      max_val = pmax(visited_total_1, visited_total_2, visited_total_3),
      dominant = case_when(
        max_val == visited_total_1 ~ 1,
        max_val == visited_total_2 ~ 2,
        max_val == visited_total_3 ~ 3
      ),
    ) %>%
    rowwise() %>%
    mutate(
      second_max_val = sort(
        c(visited_total_1, visited_total_2, visited_total_3),
        decreasing = TRUE
      )[2],
      third_max_val = sort(
        c(visited_total_1, visited_total_2, visited_total_3),
        decreasing = TRUE
      )[3],
      ratio_second = second_max_val / max_val,
      ratio_third = third_max_val / max_val,
      class = case_when(
        ratio_second < 0.2 ~ 1,
        ratio_third < 0.2 ~ 2,
        T ~ 3
      )
    ) %>%
    arrange(class, dominant) %>%
    ungroup() %>%
    mutate(
      row_index = row_number(),
    ) %>%
    select(
      cluster_index,
      row_index
    ) %>%
    arrange(cluster_index)

  data <- data %>%
    mutate(cluster = km$cluster) %>%
    mutate(
      cluster = forward_mapper[cluster, 2]$row_index
    )

  print(
    paste(
      "K-means clustering with",
      n_clusters,
      "clusters has been performed"
    )
  )

  print(
    "clustering centers:",
  )
  print(
    km$centers %>%
      bind_cols(
        forward_mapper %>%
          select(row_index)
      ) %>%
      arrange(row_index) %>%
      select(-row_index)
  )

  print(
    "Number of rows in each cluster:",
  )
  print(
    data %>%
      select(cluster) %>%
      table()
  )

  return(data)
}
