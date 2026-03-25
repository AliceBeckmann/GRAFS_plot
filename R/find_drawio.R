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
