#' Beneficial uses being addressed by the TMDL
#'
#' Look up the beneficial uses by pollutant parameter ID and TMDL.
#'
#' Data fields include:
#' \itemize{
#'   \item action_id:	USEPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_parameter: Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item Pollu_ID: DEQ water quality parameter ID. ID is for the parameter in 'TMDL_parameter'.
#'   \item ben_use_id: The ID value for the beneficial use.
#'   \item ben_use: The beneficial use name being impacted by the water quality parameter addressed by the TMDL.
#' }
#'
#' @docType data
#' @usage data(tmdl_ben_use)
#' @keywords Oregon beneficial uses
#' @keywords datasets
#' @examples
#' tmdl_ben_use1 <- data(tmdl_ben_use)
#' tmdl_ben_use2 <- odeqtmdl::tmdl_ben_use
#'

"tmdl_ben_use"
