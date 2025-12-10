#' Export TMDL database info to an excel spreadsheet.
#'
#' Exports all or a subset of the Oregon TMDL database including
#' action table (\code{\link{tmdl_actions}}),
#' assessment unit table (\code{\link{tmdl_au}}),
#' beneficial use table (\code{\link{tmdl_ben_use}}),
#' geo_id table (\code{\link{tmdl_geo_id}}),
#' TMDL target table (\code{\link{tmdl_targets}}),
#' and NPDES permittee WLA table (\code{\link{tmdl_wla}}) as an excel workbook.
#' The workbook format is based on DEQ TMDL database template version 3.0.
#'
#' @param action_ids vector of TMDL action IDs to export. Required.
#' @param xlsx_filename The path and file name of the output excel file.
#' @export

tmdl_export_xlsx <- function(action_ids  = NULL, xlsx_filename = NULL) {

  if (is.null(action_ids)) {
    stop("action_ids is NULL")
  }

 action_id_check <- any(!action_ids %in% tmdl_actions$action_id)

 if(action_id_check) {

   action_id_missing <- action_ids[!action_ids %in% tmdl_actions$action_id]

   stop(paste0("The following action_ids are not the TMDL database: ",
               paste0(action_id_missing, collapse = ", ")))
   }

  if (is.null(xlsx_filename)) {
    stop("xlsx_filename is NULL")
  }

  #- tmdl_actions --------------------------------------------------------------

  tmdl_actions_ex <- tmdl_actions |>
    dplyr::filter(action_id %in% action_ids) |>
    dplyr::select(action_id,
                  TMDL_name,
                  TMDL_issue_year,
                  issue_agency,
                  TMDL_comment,
                  in_attains,
                  attains_status,
                  TMDL_issue_date,
                  EPA_action_date,
                  citation_abbreviated,
                  citation_full,
                  URL)

 #- tmdl_parameters -------------------------------------------------------------------

 tmdl_parameters_ex <- tmdl_parameters |>
   dplyr::filter(action_id %in% action_ids) |>
   dplyr::left_join(tmdl_actions_ex[, c("action_id", "TMDL_name", "TMDL_issue_year")],
                    by = "action_id") |>
   dplyr::select(action_id,
                 TMDL_name,
                 TMDL_issue_year,
                 TMDL_parameter,
                 TMDL_pollutant)

  #- tmdl_au -------------------------------------------------------------------

  tmdl_au_ex <- tmdl_au |>
    dplyr::filter(action_id %in% action_ids) |>
    dplyr::left_join(tmdl_actions_ex[, c("action_id", "TMDL_name", "TMDL_issue_year")],
                     by = "action_id") |>
    dplyr::select(action_id,
                  TMDL_name,
                  TMDL_issue_year,
                  TMDL_parameter,
                  TMDL_pollutant,
                  AU_ID,
                  TMDL_scope,
                  Period,
                  Source,
                  TMDL_status,
                  revision_action_id)

  #- tmdl_ben_use --------------------------------------------------------------

  tmdl_ben_use_ex <- tmdl_ben_use |>
    dplyr::filter(action_id %in% action_ids) |>
    dplyr::left_join(tmdl_actions_ex[, c("action_id", "TMDL_name", "TMDL_issue_year")],
                     by = "action_id") |>
    dplyr::select(action_id,
                  TMDL_name,
                  TMDL_issue_year,
                  TMDL_parameter,
                  ben_use_id,
                  ben_use)

  #- tmdl_geo_ids --------------------------------------------------------------

  tmdl_geo_ids_ex <- tmdl_geo_ids |>
   dplyr::filter(action_id %in% action_ids) |>
    dplyr::left_join(tmdl_actions_ex[, c("action_id", "TMDL_name", "TMDL_issue_year")],
                     by = "action_id") |>
    dplyr::select(geo_id,
                  geo_description,
                  geo_id_mapped,
                  action_id,
                  TMDL_name,
                  TMDL_issue_year)

  #- tmdl_targets --------------------------------------------------------------

  tmdl_targets_ex <- tmdl_targets |>
    dplyr::filter(action_id %in% action_ids) |>
    dplyr::left_join(tmdl_actions_ex[, c("action_id", "TMDL_name", "TMDL_issue_year")],
                     by = "action_id") |>
    dplyr::mutate(TMDL_parameter = NA_character_) |>
    dplyr::select(geo_id,
                  action_id,
                  TMDL_name,
                  TMDL_issue_year,
                  TMDL_pollutant,
                  TMDL_parameter,
                  field_parameter,
                  target_type,
                  target_value,
                  target_units,
                  Unit_UID,
                  target_time_base,
                  time_base_UID,
                  target_stat_base,
                  stat_base_UID,
                  season_start,
                  season_end,
                  target_conditionals,
                  TMDL_element,
                  target_reference,
                  target_comments)

  #- tmdl_wla ------------------------------------------------------------------

  tmdl_wla_ex <- tmdl_wla |>
    dplyr::filter(action_id %in% action_ids) |>
    dplyr::left_join(tmdl_actions_ex[, c("action_id", "TMDL_name", "TMDL_issue_year")],
                     by = "action_id") |>
    dplyr::select(action_id,
                  TMDL_name,
                  TMDL_issue_year,
                  AU_ID,
                  TMDL_pollutant,
                  EPANum,
                  WQFileNum,
                  facility_name)

  #- Query Info ----------------------------------------------------------------

  query_table <- data.frame(Date = format(Sys.time(), "%F %T"),
                            'odeqtmdl package version' = packageDescription(pkg = "odeqtmdl")[["Version"]],
                            'Export action IDs' = paste0(action_ids, collapse = ", "),
                            'Note' = "This spreadsheet was exported using odeqtmdl::tmdl_export_xlsx",
                            check.names = FALSE,
                            stringsAsFactors = FALSE)


  options(openxlsx.dateFormat = "mm/dd/yyyy")
  openxlsx::write.xlsx(x = list("metadata" = query_table,
                                "tmdl_actions" = tmdl_actions_ex,
                                "tmdl_au" = tmdl_au_ex,
                                "tmdl_parameters" = tmdl_parameters_ex,
                                "tmdl_ben_use" = tmdl_ben_use_ex,
                                "tmdl_geo_ids" = tmdl_geo_ids_ex,
                                "tmdl_targets" = tmdl_targets_ex,
                                "tmdl_wla" = tmdl_wla_ex),
                       file = xlsx_filename,
                       asTable = TRUE,
                       colWidths = "auto",
                       firstActiveRow = c(2,3,3,3,3,3,3,3),
                       rowNames = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                       borders = "rows",
                       startCol = c(1), startRow = list(1,2,2,2,2,2,2,2), withFilter = FALSE,
                       headerStyle = openxlsx::createStyle(fgFill = "#000000",
                                                           halign = "LEFT",
                                                           textDecoration = "Bold",
                                                           wrapText = FALSE,
                                                           numFmt = "NUMBER",
                                                           border = "Bottom",
                                                           fontColour = "white",
                                                           fontName = "Arial",
                                                           fontSize = 10))


}



