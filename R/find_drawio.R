#' Find draw.io Executable
#'
#' Searches common installation paths for the draw.io desktop application
#' across Linux, macOS, and Windows.
#'
#' @return Path to the draw.io executable as a character string, or
#'   \code{NULL} if not found.
#'
#' @examples
#' \dontrun{
#' drawio <- find_drawio()
#' if (is.null(drawio)) {
#'   message("Install draw.io from https://github.com/jgraph/drawio-desktop")
#' }
#' }
#'
#' @export
find_drawio <- function() {
  candidates <- c(
    "/usr/bin/drawio",
    "/usr/local/bin/drawio",
    "/snap/bin/drawio",
    "/Applications/draw.io.app/Contents/MacOS/draw.io",
    "C:/Program Files/draw.io/draw.io.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/draw.io/draw.io.exe")
  )

  for (path in candidates) {
    if (nzchar(path) && file.exists(path)) {
      return(path)
    }
  }

  found <- Sys.which("drawio")
  if (nzchar(found)) {
    return(as.character(found))
  }

  NULL
}

#' Check GRAFS Package Setup
#'
#' Validates that all requirements for using the GRAFS package are met.
#' Checks for the draw.io executable, required R packages, and included
#' data files. Prints a diagnostic report with actionable guidance for
#' any missing components.
#'
#' @param drawio_path Optional path to the draw.io executable. If
#'   \code{NULL} (default), \code{\link{find_drawio}} is used to search
#'   for it automatically.
#'
#' @return Invisible \code{TRUE} if all checks pass, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' check_setup()
#' }
#'
#' @export
check_setup <- function(drawio_path = NULL) {
  ok <- TRUE

  # -- draw.io --
  message("Checking draw.io installation...")
  if (is.null(drawio_path)) {
    drawio_path <- find_drawio()
  }
  if (!is.null(drawio_path) && file.exists(drawio_path)) {
    message("  OK: draw.io found at ", drawio_path)
  } else {
    ok <- FALSE
    message("  MISSING: draw.io not found")
    message("  Install from: https://github.com/jgraph/drawio-desktop/releases")
    os <- .Platform$OS.type
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Linux") {
      message("  On Linux: download the .deb/.rpm from the releases page,")
      message("    or: sudo snap install drawio")
    } else if (sysname == "Darwin") {
      message("  On macOS: download the .dmg from the releases page,")
      message("    or: brew install --cask drawio")
      message("  Then run: chmod +x /Applications/draw.io.app/Contents/MacOS/draw.io")
    } else if (os == "windows") {
      message("  On Windows: download the installer from the releases page")
    }
  }

  # -- R packages --
  message("Checking R package dependencies...")
  pkgs <- c("xml2", "readr", "processx")
  for (pkg in pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      message("  OK: ", pkg)
    } else {
      ok <- FALSE
      message("  MISSING: ", pkg)
    }
  }
  if (!all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    message(
      "  Install missing packages: install.packages(",
      paste0("\"", missing, "\"", collapse = ", "), ")"
    )
  }

  # -- Bundled data --
  message("Checking bundled data files...")
  files <- list(
    "Example data" = system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS"),
    "Arrow IDs" = system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS"),
    "Template" = system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  )
  for (name in names(files)) {
    if (nzchar(files[[name]])) {
      message("  OK: ", name)
    } else {
      ok <- FALSE
      message("  MISSING: ", name, " (package may not be installed correctly)")
    }
  }

  # -- Summary --
  if (ok) {
    message("\nAll checks passed. Ready to use create_GRAFS().")
  } else {
    message("\nSome checks failed. Please fix the issues above.")
  }

  invisible(ok)
}
