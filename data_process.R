source("./data_process/data_import.R")
source("./data_process/select_columns.R")
source("./data_process/annex_home_state.R")
source("./data_process/basic_filter.R")
source("./data_process/recode_work_arrangement_matrices.R")
source("./data_process/save_work_matrix.R")
source("./data_process/compute_work_hours.R")
source("./data_process/compute_work_days.R")
source("./data_process/filter_by_work_hours_and_days.R")
source("./data_process/adjust_work_hours.R")
source("./data_process/recoding.R")
source("./data_process/add_built_environment.R")
source("./data_process/clustering.R")
source("./data_process/compute_commute_vmt.R")
source("./data_process/factor_analysis.R")
source("./data_process/create_database.R")
source("./data_process/imputation.R")

database <- data_import() %>%
  annex_home_state() %>%
  basic_filter(
    filter_list = list(
      supervisor_permission = F,
      job_nature_capability = F
    )
  ) %>%
  select_columns() %>%
  recode_work_arrangement_matrices() %>%
  save_work_matrix() %>%
  compute_work_hours() %>%
  compute_work_days() %>%
  filter_by_work_hours_and_days() %>%
  adjust_work_hours() %>%
  recoding() %>%
  add_built_environment() %>%
  clustering(6) %>%
  compute_commute_vmt() %>%
  factor_analysis(8) %>%
  create_database() %>%
  imputation()

saveRDS(
  database,
  file = file.path(
    "data",
    "database.rds"
  )
)
