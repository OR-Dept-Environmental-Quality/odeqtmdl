#' Oregon TMDL targets
#'
#' Inventory of unique TMDL targets from non-tribal Oregon TMDLs. Note the inventory is incomplete. See each relevant TMDL document
#' for more information. A full listing of all non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item geo_id:	Unique ID assigned to the NHD reaches where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item target_type:	Type of target, e.g. concentration, load, clarity, percent, temperature.
#'   \item target_value:	Value of TMDL Target.
#'   \item target_units:	Parameter units of the TMDL Target.
#'   \item target_stat_base:	The method used to calculate derive results of the TMDL target.
#'   \item season_start:	The beginning of the period when the target applies. Date in format %m-%b (e.g 31-Aug)
#'   \item season_end:	The end of the period when the target applies. Date in format %m-%b (e.g 31-Aug)
#'   \item target_conditionals_references:	Specific conditions or references describing how or when to apply the target.
#'   \item TMDL_element:	The TMDL element under which the target is identified.e.g. Loading Capacity, Load Allocation, Endpoint
#'   \item notes:	Notes.
#' }
#'
#' @docType data
#' @usage data(tmdl_targets)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' db1 <- data(tmdl_targets)
#' db2 <-odeqtmdl::tmdl_targets
#'

"tmdl_targets"






