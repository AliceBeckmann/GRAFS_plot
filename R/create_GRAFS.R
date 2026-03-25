# Internal helpers ------------------------------------------------------------

parse_style <- function(style) {
  parts <- strsplit(style, ";")[[1]]
  parts <- parts[nzchar(parts)]
  kv <- lapply(parts, function(x) strsplit(x, "=")[[1]])
  values <- vapply(kv, function(x) if (length(x) > 1) x[2] else "", character(1))
  names(values) <- vapply(kv, function(x) x[1], character(1))
  values
}

build_style <- function(style_list) {
  paste(paste(names(style_list), style_list, sep = "="), collapse = ";")
}

remove_id <- function(doc, id) {
  xpath <- sprintf(".//mxCell[contains(@value, '%s')]", id)
  cells <- xml2::xml_find_all(doc, xpath)
  if (length(cells) == 0) {
    warning("No mxCell found with id: ", id)
  }
  xml2::xml_remove(cells)
  doc
}

remove_change_bubbles <- function(doc) {
  bubbles <- xml2::xml_find_all(doc, ".//mxCell[contains(@style, 'ellipse')]")
  if (length(bubbles) == 0) {
    warning("No ellipse bubbles found in template")
  }
  xml2::xml_remove(bubbles)
  doc
}

change_bubble_size <- function(doc, label, new_size, new_text_size) {
  cells <- xml2::xml_find_all(doc, ".//mxCell[@value]")
  found <- FALSE
  for (cell in cells) {
    cell_value <- xml2::xml_attr(cell, "value")
    if (!is.na(cell_value) && cell_value == label) {
      style_list <- parse_style(xml2::xml_attr(cell, "style"))
      style_list["fontSize"] <- as.character(new_text_size)
      style_list["width"] <- as.character(new_size)
      style_list["height"] <- as.character(new_size)
      xml2::xml_set_attr(cell, "style", build_style(style_list))
      found <- TRUE
    }
  }
  if (!found) {
    warning("No mxCell found with value: ", label)
  }
  doc
}

change_style <- function(doc, label, identifier, value,
                         t_id_arrow, max_width_arrows, val_max_width) {
  xpath <- sprintf(".//mxCell[contains(@value, '%s')]", label)
  cell <- xml2::xml_find_all(doc, xpath)
  if (length(cell) == 0) {
    warning("Cell with label '", label, "' not found")
    return(doc)
  }

  id <- t_id_arrow$id[t_id_arrow$label == label]
  if (length(id) == 0) {
    warning("Arrow for label '", label, "' not found in ID table")
    return(doc)
  }

  xpath <- sprintf(".//mxCell[@id='%s']", id)
  cell <- xml2::xml_find_all(doc, xpath)
  if (length(cell) == 0) {
    warning("Cell for arrow '", label, "' (id: ", id, ") not found in template")
    return(doc)
  }

  style_list <- parse_style(xml2::xml_attr(cell, "style"))

  if (identifier == "strokeWidth") {
    if (!is.numeric(value)) {
      value <- 1
    } else {
      value <- min(value, val_max_width) * max_width_arrows / val_max_width
    }
    style_list[identifier] <- as.character(round(max(1, value)))
  } else {
    style_list[identifier] <- as.character(value)
  }

  xml2::xml_set_attr(cell, "style", build_style(style_list))

  if (value == 0 && identifier == "strokeWidth") {
    doc <- remove_id(doc, label)
  }

  doc
}

replace_label_in_value <- function(doc, label, value) {
  xpath <- sprintf(".//mxCell[contains(@value, '%s')]", label)
  cells <- xml2::xml_find_all(doc, xpath)

  if (length(cells) == 0) {
    warning("No mxCell found with label: ", label)
    return(doc)
  }

  for (cell in cells) {
    cell_value <- xml2::xml_attr(cell, "value")
    if (!is.na(cell_value) && grepl(label, cell_value, fixed = TRUE)) {
      new_value <- sub(label, value, cell_value, fixed = TRUE)
      xml2::xml_set_attr(cell, "value", new_value)
    }
  }

  doc
}

create_png <- function(drawio_exe, xml_path, png_path,
                       timeout = 120) {
  args <- c("-x", "-o", png_path, xml_path)

  # macOS .app bundles may need --no-sandbox in some environments
  if (Sys.info()[["sysname"]] == "Darwin") {
    args <- c("--no-sandbox", args)
  }

  p <- processx::process$new(drawio_exe, args)
  p$wait(timeout * 1000)

  if (p$is_alive()) {
    p$kill()
    warning("draw.io export timed out after ", timeout,
            "s for: ", xml_path)
    return(invisible(png_path))
  }

  if (p$get_exit_status() != 0) {
    stderr_out <- p$read_all_error_lines()
    warning("draw.io export failed for: ", xml_path,
            if (length(stderr_out) > 0)
              paste0("\n  ", paste(stderr_out, collapse = "\n  ")))
  }

  message("Created: ", png_path)
  invisible(png_path)
}

year_info <- function(years) {
  if (length(years) == 0) return("")
  if (length(years) == 1) return(as.character(years))

  label <- as.character(years[1])
  for (i in 2:length(years)) {
    if (years[i] != (years[i - 1] + 1)) {
      label <- paste0(label, "-", years[i - 1], "_", years[i])
    } else if (i == length(years)) {
      label <- paste0(label, "-", years[i])
    }
  }
  for (y in unique(years)) {
    label <- gsub(paste0(y, "-", y), as.character(y), label, fixed = TRUE)
  }
  label
}

prepare_data <- function(csv_path) {
  d <- readr::read_csv(csv_path, show_col_types = FALSE)
  d <- unique(d)
  d[!grepl("WIDTH_MAX", d$label), ]
}

prepare_directories <- function(path) {
  dir.create(file.path(path, "xml"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "png"), recursive = TRUE, showWarnings = FALSE)
}

select_region_data <- function(d, region, years) {
  d[d$province == region & d$year %in% years, ]
}

process_label <- function(doc, x, xch, label, years, years_change,
                          decimals, plot_change, increase_color, decrease_color,
                          t_id_arrow, max_width_arrows, val_max_width) {
  arrowcolors <- unique(x$arrowColor[!is.na(x$arrowColor)])
  arrowcolor <- if (length(arrowcolors) > 0) arrowcolors[1] else NA
  value_change <- NA

  is_text_label <- label %in% c("{PROVINCE_NAME}", "{YEAR}")

  if (label == "{PROVINCE_NAME}") {
    mean_val <- x$data[1]
  } else if (label == "{YEAR}") {
    mean_val <- paste0("mean_", min(years), "-", max(years))
  } else {
    x$data <- as.numeric(x$data)
    mean_val <- round(mean(x$data), decimals)
    if (mean_val == 0) {
      mean_val <- round(mean(x$data), decimals + 1)
    }
    if (plot_change && !is.null(xch) && nrow(xch) > 0) {
      xch$data <- as.numeric(xch$data)
      if (mean(xch$data) != 0) {
        value_change <- mean(x$data) * 100 / mean(xch$data) - 100
      }
    }
  }

  if (!is_text_label) {
    doc <- change_style(
      doc, label, "strokeWidth", mean_val,
      t_id_arrow, max_width_arrows, val_max_width
    )

    if (!is.na(arrowcolor)) {
      doc <- change_style(
        doc, label, "fillColor", arrowcolor,
        t_id_arrow, max_width_arrows, val_max_width
      )
    }

    if (plot_change && !is.na(value_change)) {
      doc <- change_style(
        doc, label, "fillColor",
        ifelse(value_change > 0, increase_color, decrease_color),
        t_id_arrow, max_width_arrows, val_max_width
      )
      doc <- change_bubble_size(doc, label, abs(value_change), 12)
    }
  }

  doc <- replace_label_in_value(doc, label, mean_val)
  doc
}

process_labels_loop <- function(doc, dact, dactch, years, years_change,
                                decimals, plot_change, increase_color,
                                decrease_color, t_id_arrow,
                                max_width_arrows, val_max_width) {
  for (lact in unique(dact$label)) {
    x <- dact[dact$label == lact, ]
    xch <- if (plot_change) dactch[dactch$label == lact, ] else NULL

    if (nrow(x) != length(years)) {
      warning(
        "Expected ", length(years), " rows for label '", lact,
        "' but found ", nrow(x)
      )
    }
    if (plot_change && !is.null(xch) && nrow(xch) != length(years_change)) {
      warning(
        "Expected ", length(years_change), " rows for change label '", lact,
        "' but found ", nrow(xch)
      )
    }

    doc <- process_label(
      doc, x, xch, lact, years, years_change,
      decimals, plot_change, increase_color, decrease_color,
      t_id_arrow, max_width_arrows, val_max_width
    )
  }
  doc
}

process_period_region <- function(period_idx, period, regions, d,
                                  xml_base, path_outputs, drawio_exe,
                                  decimals, val_max_width,
                                  increase_color, decrease_color,
                                  t_id_arrow, overwrite,
                                  max_width_arrows) {
  years <- period$years
  years_change <- period$prev_years
  plot_change <- !is.null(years_change)

  for (region in regions) {
    dact <- select_region_data(d, region, years)
    dactch <- select_region_data(d, region, years_change)

    filename <- sprintf(
      "GRAFS_%s_P%d_MEAN_%s", region, period_idx, year_info(years)
    )
    xml_out <- file.path(path_outputs, "xml", paste0(filename, ".xml"))
    png_out <- file.path(path_outputs, "png", paste0(filename, ".png"))

    if (file.exists(xml_out) && !overwrite) {
      next
    }

    doc <- xml2::read_xml(xml_base) |>
      process_labels_loop(
        dact, dactch, years, years_change,
        decimals, plot_change, increase_color, decrease_color,
        t_id_arrow, max_width_arrows, val_max_width
      )

    if (!plot_change) {
      doc <- remove_change_bubbles(doc)
    }

    writeLines(as.character(doc), xml_out)
    create_png(drawio_exe, xml_out, png_out)
  }
}

# Exported functions ----------------------------------------------------------

#' Create GRAFS Diagrams
#'
#' Generate GRAFS (General Representation of Agro-Food Systems) diagrams
#' by populating a draw.io XML template with data and exporting to PNG.
#'
#' The function reads flow data from a CSV file, applies it to a draw.io
#' XML template (modifying arrow widths, colors, and labels proportionally),
#' and exports the result as PNG images using the draw.io CLI.
#'
#' @param csv_inputs Path to the CSV file containing flow data. Must have
#'   columns: \code{province}, \code{year}, \code{label}, \code{data},
#'   \code{align}, and optionally \code{arrowColor}. The package includes
#'   example data accessible via
#'   \code{system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")}.
#' @param path_outputs Directory for output files. Created automatically if
#'   it does not exist. Subdirectories \code{xml/} and \code{png/} are created
#'   inside.
#' @param xml_base Path to the draw.io XML template. A default template is
#'   included:
#'   \code{system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")}.
#' @param arrows_csv Path to the CSV mapping flow labels to draw.io element
#'   IDs. The default mapping is included:
#'   \code{system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")}.
#' @param drawio_exe Path to the draw.io executable. Use
#'   \code{\link{find_drawio}} to locate it automatically.
#' @param regions Character vector of region names to process. Must match values
#'   in the \code{province} column of the input CSV.
#' @param periods List of period specifications. Each element is a list with
#'   a \code{years} vector and an optional \code{prev_years} vector for
#'   computing percentage changes. Example:
#'   \code{list(list(years = 2011:2015, prev_years = 1990:1994))}.
#' @param decimals Number of decimal places for displayed values
#'   (default 0). Use 1 for smaller regions with small flow values.
#' @param max_width_arrows Maximum arrow width in the diagram (default 25).
#' @param val_max_width Data value corresponding to \code{max_width_arrows}
#'   (default 1000). Arrow widths scale proportionally.
#' @param increase_color Hex color for positive change bubbles
#'   (default \code{"#97cde5"}).
#' @param decrease_color Hex color for negative change bubbles
#'   (default \code{"#a9d77f"}).
#' @param overwrite Overwrite existing output files (default \code{TRUE}).
#'
#' @return Invisible \code{NULL}. Called for side effects: creates XML and PNG
#'   files in \code{path_outputs}.
#'
#' @references
#' Billen, G., et al. (2014). \emph{Regional Studies}.
#'
#' Lassaletta, L., et al. (2015). \emph{Global Biogeochemical Cycles}.
#'
#' Le Noe, J., et al. (2017). \emph{Science of the Total Environment}.
#'
#' @examples
#' \dontrun{
#' csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
#' xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
#' ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
#' drawio <- find_drawio()
#'
#' create_GRAFS(
#'   csv_inputs = csv,
#'   path_outputs = tempdir(),
#'   xml_base = xml,
#'   arrows_csv = ids,
#'   drawio_exe = drawio,
#'   regions = "Albacete",
#'   periods = list(list(years = 1930:1931))
#' )
#'
#' # Compare two periods with change bubbles
#' create_GRAFS(
#'   csv_inputs = csv,
#'   path_outputs = tempdir(),
#'   xml_base = xml,
#'   arrows_csv = ids,
#'   drawio_exe = drawio,
#'   regions = "Albacete",
#'   periods = list(list(years = 2011:2015, prev_years = 1990:1994))
#' )
#' }
#'
#' @export
create_GRAFS <- function(csv_inputs,
                         path_outputs,
                         xml_base,
                         arrows_csv,
                         drawio_exe,
                         regions,
                         periods,
                         decimals = 0,
                         max_width_arrows = 25,
                         val_max_width = 1000,
                         increase_color = "#97cde5",
                         decrease_color = "#a9d77f",
                         overwrite = TRUE) {
  if (!is.character(regions) || length(regions) == 0) {
    stop("'regions' must be a non-empty character vector")
  }
  if (!is.list(periods) || length(periods) == 0) {
    stop("'periods' must be a non-empty list")
  }
  for (idx in seq_along(periods)) {
    if (is.null(periods[[idx]]$years)) {
      stop("Period ", idx, " is missing required 'years' element")
    }
  }
  if (!file.exists(csv_inputs)) {
    stop("Input CSV file not found: ", csv_inputs)
  }
  if (!file.exists(xml_base)) {
    stop("XML template not found: ", xml_base)
  }
  if (!file.exists(arrows_csv)) {
    stop("Arrow IDs CSV not found: ", arrows_csv)
  }
  if (!file.exists(drawio_exe)) {
    stop(
      "draw.io executable not found at: ", drawio_exe,
      "\nInstall draw.io from https://github.com/jgraph/drawio-desktop",
      "\nor use find_drawio() to locate an existing installation."
    )
  }

  t_id_arrow <- readr::read_csv(arrows_csv, show_col_types = FALSE)
  prepare_directories(path_outputs)
  d <- prepare_data(csv_inputs)

  for (i in seq_along(periods)) {
    process_period_region(
      period_idx = i,
      period = periods[[i]],
      regions = regions,
      d = d,
      xml_base = xml_base,
      path_outputs = path_outputs,
      drawio_exe = drawio_exe,
      decimals = decimals,
      val_max_width = val_max_width,
      increase_color = increase_color,
      decrease_color = decrease_color,
      t_id_arrow = t_id_arrow,
      overwrite = overwrite,
      max_width_arrows = max_width_arrows
    )
  }

  invisible(NULL)
}
