# Recode work arrangement matrices from work_matrix_i_j_k to work_matrix_i_j
# The value of work_matrix_i_j corresponds to the work location selected for the ith day of the week, jth period of the day
# Working at multiple locations leads to a value of work_matrix_i_j = 7, 6, 5, or 3 according to the combination of locations
# 7 - Primary + Temporary + Home
# 6 - Primary + Temporary
# 5 - Primary + Home
# 4 - Primary
# 3 - Temporary + Home
# 2 - Temporary
# 1 - Home
# 0 - Not working

recode_work_arrangement_matrices = function(data) {
  for (i in 1:7) {
    for (j in 1:10) {
      index <- data %>%
        select(
          matches(
            paste("work_matrix", i, j, "\\d", sep = "_")
          )
        ) %>%
        mutate_all(~ pmax(., 0, na.rm = T)) %>%
        sweep(2, c(4, 2, 1), "*") %>%
        rowSums(na.rm = TRUE)
      data <- data %>%
        mutate(
          !!paste("work_matrix", i, j, sep = "_") :=
            index
        )
    }
  }

  print("Work arrangement matrices recoded")

  return(data)
}
