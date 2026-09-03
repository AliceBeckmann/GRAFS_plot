#' @keywords internal
"_PACKAGE"

#' GRAFS: Automated GRAFS Diagram Plotter
#'
#' Automates the creation of GRAFS (Generalized Representation of Agro-Food
#' Systems) diagrams. Reads flow data from CSV files, populates a draw.io
#' XML template with proportional arrow widths and labels, and exports the
#' result as PNG images.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{create_GRAFS}}}{Generate GRAFS diagrams from CSV data}
#'   \item{\code{\link{validate_inputs}}}{Check input file consistency}
#'   \item{\code{\link{find_drawio}}}{Locate the draw.io executable}
#'   \item{\code{\link{check_setup}}}{Verify all dependencies are installed}
#' }
#'
#' @section Getting started:
#' Run \code{check_setup()} to verify your installation, then see
#' \code{vignette("getting-started", package = "GRAFS")} for a guided
#' walkthrough.
#'
#' @references
#' Billen, G., et al. (2014). \emph{Regional Studies}.
#'
#' Lassaletta, L., et al. (2015). \emph{Global Biogeochemical Cycles}.
#'
#' Le Noe, J., et al. (2017). \emph{Science of the Total Environment}.
#'
#' Rodriguez, A., et al. (2023). \emph{Science of the Total Environment}
#' 892: 164467.
#'
#' @name GRAFS-package
#' @aliases GRAFS
NULL
