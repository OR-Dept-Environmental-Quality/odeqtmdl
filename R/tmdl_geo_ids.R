#' Oregon TMDL geo IDs
#'
#' Inventory of unique TMDL geo IDs for Oregon TMDLs. A geo ID is a
#' unique ID that is used to identify where a specific TMDL target applies. Geo IDs
#' are mapped to the National Hydrography Dataset version NHDH_OR_931v220.
#' See \code{\link{tmdl_targets}} for a summary of TMDL targets.
#'
#' Note the TMDL information is still being assembled and is undergoing review.
#' Some information may not be accurate. See each relevant TMDL document for the
#' official record and more information. TMDLs developed by tribal governments are not included at this time. The inventory
#' is still being developed and some information may not be accurate.
#' A full listing of all non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item geo_id:	Unique ID assigned to the NHD reaches codes where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
#'   \item geo_description:	General name and description of area where the TMDL target applies.
#'   \item geo_id_mapped:	Boolean to indicate if the geo_id has been mapped in a GIS.
#' }
#'
#' @docType data
#' @usage data(tmdl_geo_ids)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' db1 <- data(tmdl_geo_ids)
#' db2 <-odeqtmdl::tmdl_geo_ids
#'

"tmdl_geo_ids"
