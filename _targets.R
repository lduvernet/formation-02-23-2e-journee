# _targets.R file
library(targets)
source("script/functions.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "gt", "MASS", "forcats"))
list(
  tar_target(file_data, "individu_reg.parquet", format = "file"),
  tar_target(data, read_from_parquet(file_data)),
  tar_target(file_token , "secrets.yaml", format = "file"),
  tar_target(token, read_yaml_secret(file_token, "api_token"))
  # tar_target(model, fit_model(data)),
  # tar_target(plot, plot_model(model, data))
)