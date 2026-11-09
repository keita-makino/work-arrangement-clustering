# Recode work arrangement matrices from work_matrix_i_j_k to work_matrix_i_j
# The value of work_matrix_i_j corresponds to the work location selected for the ith day of the week, jth period of the day
# Day of the week: 1 - Sunday, ..., 7 - Saturday
# Period of the day: 1 - 12AM-6AM, then 2-hour periods (2 - 6AM-8AM, ..., 10 - 10PM-12AM)
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
