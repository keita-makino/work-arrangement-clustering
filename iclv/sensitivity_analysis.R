model_name <- "6_8_1729_medium"

database <- readRDS(
  file.path(
    "data",
    "database.rds"
  )
)

model <- apollo_loadModel(
  file.path("dist", "iclv", model_name)
)
