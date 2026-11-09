# Save the recoded work arrangement matrices (work_matrix_i_j) with row_index
# to a separate file, since they are dropped in compute_work_hours().
# They can be joined to database.rds by row_index (e.g., descriptive/heatmap.R).

save_work_matrix = function(data) {
  stopifnot(!anyDuplicated(data$row_index))

  data %>%
    select(
      row_index,
      matches("^work_matrix_\\d{1,2}_\\d{1,2}$")
    ) %>%
    mutate(
      row_index = as.numeric(row_index)
    ) %>%
    saveRDS(
      file = file.path(
        "data",
        "work_matrix.rds"
      )
    )

  print(
    "Saved the recoded work arrangement matrices to work_matrix.rds"
  )

  return(data)
}
