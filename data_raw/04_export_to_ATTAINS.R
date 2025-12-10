library(odeqtmdl)
library(readxl)

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c("text", "text"))

action_ids <- "OR_TMDL_20230915"
out_dir <- file.path(paths$attains_project_path[1], "ATTAINS_uploads")

export_status <- tmdl_export_attains(out_dir = out_dir, action_ids = action_ids)
