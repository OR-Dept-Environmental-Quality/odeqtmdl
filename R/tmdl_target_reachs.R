#' Oregon TMDL targets by NHD reach
#'
#' Table of Oregon TMDL targets mapped to the high resolution
#' National Hydrography Dataset (NHD) version NHDH_OR_931v220. Note the TMDL
#' information is still being assembled and is undergoing review. Not all
#' TMDL targets are included. See each relevant TMDL document for the official
#' record and more information.
#'
#' Table fields include:
#'
#' \itemize{
#'   \item geo_id:	Unique ID assigned to the NHD reaches codes where a TMDL target applies.
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item GLOBALID: Unique ID for every NHD reach record. Used for joins with TMDL GIS features.
#'   \item Permanent_Identifier: NHD Permanent Identifier.
#'   \item ReachCode: NHD reach code.
#'   \item GNIS_Name: Proper name, specific term, or expression by which a particular geographic entity is known
#'   \item GNIS_ID: Unique identifier assigned by GNIS, length 10.
#'   \item AU_ID:	Assessment Unit ID.
#'   \item AU_Name: Name of the assessment unit.
#'   \item AU_Description: Assessment unit descriptions.
#'   \item AU_GNIS_Name: Assessment unit and GNIS name concatenation.
#'   \item AU_GNIS: Same as GNIS name but with a few additional names not in NHD.
#'   \item HUC6: Basin six digit USGS hydrological unit code.
#'   \item HUC6_Name: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC6 and HUC6_Name fields.
#'   \item HUC8: Subbasin eight digit USGS hydrological unit code.
#'   \item HUC8_Name: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC8 and HUC8_Name fields.
#'   \item HUC10: Watershed ten digit USGS hydrological unit code.
#'   \item HUC10_Name: USGS Watershed name.
#'   \item HUC10_full: Concatenation of the HUC10 and HUC10_Name fields.
#'   \item LengthKM: Length of linear feature based on Albers Equal Area.
#' }
#'
#' @export
#' @docType data
#' @keywords datasets
#' @return dataframe

"tmdl_target_reaches"
