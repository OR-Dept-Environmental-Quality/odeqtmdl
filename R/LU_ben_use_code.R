#' Beneficial use codes and corresponding beneficial use IDs
#'
#' Look up table of the applicable beneficial use IDs using the beneficial use code.
#' Each assessment unit has a beneficial use code assigned. This table cane be used
#' to determine which beneficial uses have been designated on which assessment units.
#'
#' Data fields include:
#' \itemize{
#'   \item ben_use_code: Code assigned to the unique group of designated beneficial uses.
#'   \item ben_use_id: The ID value for the beneficial use.
#'   \item ben_use: The beneficial use name.
#' }
#'
#' @docType data
#' @usage data(LU_ben_use_code)
#' @keywords Oregon beneficial uses
#' @keywords datasets
#' @examples
#' LU_ben_use_code1 <- data(LU_ben_use_code)
#' LU_ben_use_code2 <- odeqtmdl::LU_ben_use_code
#'

"LU_ben_use_code"
