compute_commute_vmt <- function(data) {
  data <- data %>%
    mutate(
      freq = case_when(
        commute_pv_freq == 0 ~ 0,
        commute_pv_freq == 1 ~ 0.1,
        commute_pv_freq == 2 ~ 0.33,
        commute_pv_freq == 3 ~ 0.5,
        commute_pv_freq == 4 ~ 1.5,
        commute_pv_freq == 5 ~ 3.5,
        commute_pv_freq == 6 ~ 5.5
      ),
      calculated_vmt_commute = case_when(
        is.na(work_home_dist) ~ NA_real_,
        TRUE ~ as.numeric(work_home_dist) * freq * 2
      ),
      vmt_commute = case_when(
        is.na(suggested_vmt_commute) ~ 0,
        reported_vmt_commute > 0 ~ as.numeric(reported_vmt_commute),
        TRUE ~ calculated_vmt_commute
      ),
      vmt_source = case_when(
        is.na(suggested_vmt_commute) ~ "zero",
        reported_vmt_commute > 0 ~ "reported",
        !is.na(calculated_vmt_commute) ~ "calculated",
        TRUE ~ NA_character_
      )
    )

  vmt_cluster_stats <- data %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      n_vmt = sum(!is.na(vmt_commute)),
      missing_share = mean(is.na(vmt_commute)),
      vmt_commute_mean = if (all(is.na(vmt_commute))) {
        NA_real_
      } else {
        mean(vmt_commute, na.rm = TRUE)
      },
      vmt_commute_sd = sd(vmt_commute, na.rm = TRUE),
      vmt_commute_median = median(vmt_commute, na.rm = TRUE),
      vmt_commute_p75 = quantile(vmt_commute, 0.75, na.rm = TRUE, names = FALSE),
      vmt_commute_p90 = quantile(vmt_commute, 0.90, na.rm = TRUE, names = FALSE),
      vmt_commute_p95 = quantile(vmt_commute, 0.95, na.rm = TRUE, names = FALSE),
      vmt_commute_p99 = quantile(vmt_commute, 0.99, na.rm = TRUE, names = FALSE),
      vmt_commute_max = if (all(is.na(vmt_commute))) {
        NA_real_
      } else {
        max(vmt_commute, na.rm = TRUE)
      },
      top_1pct_share = {
        values <- vmt_commute[!is.na(vmt_commute)]
        total <- sum(values)
        if (length(values) == 0 || total == 0) {
          NA_real_
        } else {
          cutoff <- max(1, ceiling(length(values) * 0.01))
          sum(sort(values, decreasing = TRUE)[seq_len(cutoff)]) / total
        }
      },
      n_reported = sum(vmt_source == "reported", na.rm = TRUE),
      n_calculated = sum(vmt_source == "calculated", na.rm = TRUE),
      reported_vmt_mean = if (any(vmt_source == "reported", na.rm = TRUE)) {
        mean(vmt_commute[vmt_source == "reported"], na.rm = TRUE)
      } else {
        NA_real_
      },
      reported_vmt_max = if (any(vmt_source == "reported", na.rm = TRUE)) {
        max(vmt_commute[vmt_source == "reported"], na.rm = TRUE)
      } else {
        NA_real_
      },
      calculated_vmt_mean = if (any(vmt_source == "calculated", na.rm = TRUE)) {
        mean(vmt_commute[vmt_source == "calculated"], na.rm = TRUE)
      } else {
        NA_real_
      },
      calculated_vmt_max = if (any(vmt_source == "calculated", na.rm = TRUE)) {
        max(vmt_commute[vmt_source == "calculated"], na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  assign(
    "vmt_cluster_stats",
    vmt_cluster_stats,
    envir = .GlobalEnv
  )

  print("Commute VMT statistics by cluster:")
  print(vmt_cluster_stats)

  data <- data %>%
    select(
      -work_home_dist,
      -commute_pv_freq,
      -suggested_vmt_commute,
      -reported_vmt_commute,
      -freq,
      -calculated_vmt_commute,
      -vmt_source
    )

  return(data)
}
