# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(stringr)
library(tidyr)
library(readtext)
library(XML)
library(processx)
library(dplyr)
# library(GRAFS)

run_basic_example <- function() {
  # Get the inputs from the demo data from the GRAFS package
  XML_BASE <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  arrow_ids <- system.file(
    "extdata",
    "GRAFS_arrows_ids.csv",
    package = "GRAFS"
  ) |>
    readr::read_csv()

  # Mac path
  # chmod +x /Applications/draw.io.app/Contents/MacOS/draw.io
  DRAW_IO_EXE <- "/usr/bin/drawio"
  # Default Windows path
  if (!file.exists(DRAW_IO_EXE)) {
    DRAW_IO_EXE <- "C:/Program Files/draw.io/draw.io.exe"
  }

  # If not found, then try to search for that in Windows
  if (!file.exists(DRAW_IO_EXE)) {
    DRAW_IO_EXE <- search_drawio()
  }
  if (length(DRAW_IO_EXE) == 0) {
    stop("Please configure first the draw.io path")
  }

  # Figures will be generated in the current path
  PATH_OUTPUTS <- "./GRAFS_test-outputs/"

  system.file(
    "extdata",
    "GRAFS_spain_data.csv",
    # "GRAFS_alfredo.csv",
    package = "GRAFS"
  ) |>
    readr::read_csv() |>
    dplyr::filter(!label %in% c("{YEAR}", "{PROVINCE_NAME}", "{WIDTH_MAX}")) |>
    create_GRAFS(
      PATH_OUTPUTS,
      XML_BASE,
      arrow_ids,
      DRAW_IO_EXE,
      max_width_arrows = 15,
      val_max_width = 10000,
      regions = c("Albacete", "Zamora"),
      years = 1930:1931,
      years_change = 1920:1921
    )
}
