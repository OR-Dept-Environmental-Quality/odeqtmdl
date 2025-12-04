# This script generates the following data tables

# LU_ben_use
# LU_ben_use_id
# LU_pollutant

# It should be opened within the Project Rstudio view so the working directory is set correctly.

library(readxl)
library(dplyr)
library(tidyr)

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c("text", "text"))

#- LU_ben_use ----------------------------------------------------------------

LU_ben_use <- readxl::read_excel(path = file.path(paths$package_path[1], "data_raw", "LU_ben_use.xlsx"),
                               sheet = "Sheet1", col_names = TRUE,
                               col_types = c("numeric", "text")) %>%
  select(ben_use_id, ben_use) %>%
  distinct() %>%
  arrange(ben_use_id) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(LU_ben_use, file = file.path(paths$package_path[1], "data", "LU_ben_use.rda"))

#- LU_ben_use_code ----------------------------------------------------------------

LU_ben_use_code <- readxl::read_excel(path = file.path(paths$package_path[1], "data_raw", "LU_ben_use_code.xlsx"),
                                    sheet = "sheet1", col_names = TRUE,
                                    col_types = c("numeric", "numeric", "text")) %>%
  select(ben_use_code,	ben_use_id,	ben_use) %>%
  distinct() %>%
  arrange(ben_use_code,	ben_use_id) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(LU_ben_use_code, file = file.path(paths$package_path[1], "data", "LU_ben_use_code.rda"))

#- LU_pollutant ----------------------------------------------------------------

LU_pollutant <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], "Resources", "LU_Pollu_ID.xlsx"),
                                   sheet = "Final", col_names = TRUE,
                                   col_types = c('numeric', 'text', 'text',
                                                 'text', 'numeric', 'text',
                                                 'text', "logical")) %>%
  select(-TMDL_program) %>%
  arrange(Pollutant_DEQ) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(LU_pollutant, file = file.path(paths$package_path[1], "data", "LU_pollutant.rda"))

#- LU_action_type ----------------------------------------------------------------

IR_year_ref <- 2022
IR_xlsx <- "2022_Final_IR.xlsx"

LU_action_type <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], IR_xlsx),
                                   sheet = "Sheet1", col_names = TRUE,
                                   col_types = c("text", "text", "text", "numeric", "numeric",
                                                 "text", "text", "text", "text", "text",
                                                 "text", "text", "text", "text","numeric",
                                                 "text", "numeric", "text")) %>%
  filter(Parameter_category %in% c("4A","4B","4C","5")) %>%
  select(AU_ID, Pollu_ID) %>%
  mutate(action_type = "TMDL",
         IR_year = IR_year_ref) %>%
  distinct() %>%
  right_join(odeqtmdl::tmdl_au, by = c("AU_ID", "Pollu_ID")) %>%
  filter(TMDL_scope == "TMDL") %>%
  mutate(action_type = replace_na(action_type, "Protection Approach"),
         IR_year = replace_na(IR_year, IR_year_ref)) %>%
  select(action_id, AU_ID, Pollu_ID, TMDL_parameter, action_type, IR_year, TMDL_scope) %>%
  distinct() %>%
  arrange(action_id, AU_ID, TMDL_parameter, action_type) %>%
  as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(LU_action_type, file = file.path(paths$package_path[1], "data", "LU_action_type.rda"))
