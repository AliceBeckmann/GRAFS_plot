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
  CSV_INPUTS <- system.file(
    "extdata",
    "GRAFS_spain_data.csv",
    # "GRAFS_alfredo.csv",
    package = "GRAFS"
  )
  XML_BASE <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  TABLA_ID_XML <- system.file(
    "extdata",
    "GRAFS_arrows_ids.csv",
    package = "GRAFS"
  )

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

  create_GRAFS(
    CSV_INPUTS,
    PATH_OUTPUTS,
    XML_BASE,
    TABLA_ID_XML,
    DRAW_IO_EXE,
    MAX_WIDTH_ARROWS = 15,
    VAL_MAX_WIDTH = 10000,
    REGIONS = "Albacete",
    PERIODS = list(
      list(years = 1930:1931)
      # list(years = 1930:1931, prev_years = 1920:1921)
    )
  )
}
