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

  val_max_width <- 1000 #choose max value to change arrow sizes
  system.file(
    "extdata",
    "GRAFS_spain_data.csv",
    package = "GRAFS"
  ) |>
    readr::read_csv() |>
    dplyr::mutate(
      label = ifelse(label == "{WIDTH_MAX}", val_max_width, label)
    ) |>
    dplyr::filter(!label %in% c("{YEAR}", "{PROVINCE_NAME}")) |>
    create_GRAFS(
      PATH_OUTPUTS,
      XML_BASE,
      arrow_ids,
      DRAW_IO_EXE,
      max_width_arrows = 20,
      val_max_width = val_max_width,
      regions = c("Spain"),
      years = 1960:1970,
      years_change = 1920:1930
    )
}
