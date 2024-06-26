#' Oregon TMDL database of watershed assessment unit and GNIS name
#'
#' Inventory of Oregon TMDL information for each named stream in watershed assessment units.
#' The information is summarized for each AU_GNIS. AU_GNIS is a concatenation of
#' the Oregon watershed assessment unit ID (AU) and the NHD GNIS name.
#' Other assessment Unit types are not included in this table (e.g. Stream/River, Lakes/Reservoirs, Estuaries/Bays).
#' See \code{\link{tmdl_au}} for the TMDL summary for all assessment unit types, including watershed units.
#' See \code{\link{tmdl_reaches}} for TMDL information summarized for each NHD reach.
#'
#' Note the TMDL information is still being assembled and is undergoing review.
#' Some information may not be accurate. See each relevant TMDL document for the
#' official record and more information. TMDLs developed by tribal governments are not included at this time. The inventory
#' is still being developed and some information may not be accurate.
#' A full listing of all non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#' NHD values are derived from NHDH_OR_931v220, which is the current version used for DEQ business data. Database fields include:
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_wq_limited_parameter:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_scope: Provides information about how the TMDL applies.
#'      \itemize{
#'      \item TMDL:	The TMDL was developed to address a 303(d) listing or future listing in this assessment unit.
#'      \item Allocation only: A TMDL allocation applies in this assessment unit
#'                            but the TMDL does not address a 303(d) listing or future listing.
#'                            Typically this situation is applicable for tributaries or canals
#'                            that are upstream of the reach where the "TMDL" applies.
#'                            The pollutant reduction in the upstream reach is needed to achieve the
#'                            TMDL loading capacity of the downstream reach.
#'      \item Advisory allocation: A TMDL allocation may apply
#'                            based on assessment of source loads and if pollutant
#'                            reduction is needed to achieve a TMDL allocation or
#'                            loading capacity downstream. See TMDL document for details and requirements.
#'                            The TMDL does not address a 303(d) listing or future listing in this reach.
#'                                }
#'   \item Period: Identifies the fish use period that the TMDL addresses. Only used for TMDLs that address temperature or dissolved oxygen.
#'      \itemize{
#'      \item year_round: TMDL addresses only non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item spawning: TMDL addresses only spawning uses for the temperature or dissolved oxygen water quality standards.
#'      \item Both: TMDL addresses both spawning and non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item Mixed: TMDL addresses different fish use periods in different sections of the assessment unit.
#'      }
#'   \item Source:
#'      \itemize{
#'      \item Point source: Identifies assessment units where pollutant loading is from point sources only.
#'      \item Nonpoint source: Identifies assessment units where pollutant loading is from nonpoint sources only.
#'      \item Both: Identifies assessment units where pollutant loading is from point sources and nonpoint sources.
#'      }
#'   \item Pollu_ID: DEQ water quality parameter ID. ID is for the parameter in 'TMDL_wq_limited_parameter'.
#'   \item HUC6: Basin six digit USGS hydrological unit code
#'   \item HUC6_Name: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC_6 and HU_6_NAME fields.
#'   \item HUC8: Subbasin six digit USGS hydrological unit code.
#'   \item HUC8_Name: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC8 and HUC8_Name fields.
#'   \item AU_ID:	Assessment Unit ID.
#'   \item AU_Name: Name of the assessment unit
#'   \item AU_GNIS_Name: Assessment unit and GNIS name concatenation.
#'   \item AU_GNIS: Same as GNIS name but with a few additional names not in NHD.
#'   \item TMDL_length_km: Length of the assessment unit in kilometers where TMDL_scope = 'TMDL'. Length is calculated using the linear flowline feature in Albers Equal Area projection.
#'   \item Allocation_only_km: Length of the GNIS assessment unit in kilometers where TMDL_scope = 'Allocation only'.
#'   \item Advisory_allocation_km: Length of the GNIS assessment unit in kilometers where TMDL_scope = 'Advisory allocation'.
#'   \item AU_length_km: Length of the entire assessment unit in kilometers. Length is calculated using the linear flowline feature in Albers Equal Area projection.
#'   \item AU_GNIS_length_km: Length of the GNIS assessment unit in kilometers. Length is calculated using the linear flowline feature in Albers Equal Area projection.
#'   \item TMDL_AU_GNIS_Percent: Percent of the GNIS assessment unit where a TMDL has been developed to address a 303(d) listing or future listing.
#'   \item Allocation_AU_GNIS_Percent: Percent of the GNIS assessment unit where a TMDL allocation applies.
#' }
#'
#' @docType data
#' @usage data(tmdl_au_gnis)
#' @keywords Oregon TMDL assessment unit database
#' @keywords datasets
#' @examples
#' db1 <- data(tmdl_au_gnis)
#' db2 <-odeqtmdl::tmd_au_gnis
#'

"tmdl_au_gnis"







