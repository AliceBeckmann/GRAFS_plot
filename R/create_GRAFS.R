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
  parts <- vapply(seq_along(style_list), function(i) {
    if (nzchar(style_list[i])) {
      paste0(names(style_list)[i], "=", style_list[i])
    } else {
      names(style_list)[i]
    }
  }, character(1))
  paste(parts, collapse = ";")
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
  bubbles <- xml2::xml_find_all(
    doc, ".//mxCell[contains(@style, 'ellipse') and @value != '']"
  )
  if (length(bubbles) == 0) {
    warning("No ellipse bubbles found in template")
  }
  xml2::xml_remove(bubbles)
  doc
}

change_bubble_size <- function(doc, label, new_size, new_text_size,
                               fill_color = NULL) {
  cells <- xml2::xml_find_all(doc, ".//mxCell[@value]")
  found <- FALSE
  for (cell in cells) {
    cell_value <- xml2::xml_attr(cell, "value")
    if (!is.na(cell_value) && grepl(label, cell_value, fixed = TRUE)) {
      style_list <- parse_style(xml2::xml_attr(cell, "style"))
      style_list["fontSize"] <- as.character(new_text_size)
      style_list["width"] <- as.character(new_size)
      style_list["height"] <- as.character(new_size)
      if (!is.null(fill_color)) style_list["fillColor"] <- fill_color
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
    if (!is.numeric(value) || is.na(value)) {
      value <- 1
    } else {
      value <- min(value, val_max_width) * max_width_arrows / val_max_width
    }
    if (value == 0) {
      doc <- remove_id(doc, label)
      xml2::xml_remove(cell)
      return(doc)
    }
    style_list[identifier] <- as.character(round(max(1, value)))
    style_list["endWidth"] <- "0"
  } else {
    style_list[identifier] <- as.character(value)
  }

  xml2::xml_set_attr(cell, "style", build_style(style_list))

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
      new_value <- gsub(label, value, cell_value, fixed = TRUE)
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

  p <- processx::process$new(drawio_exe, args, stderr = "|")
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
    return(invisible(png_path))
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

#' Add derived total-cropland-N rows where missing
#'
#' \code{\{CRPLNDTOTN\}} (total cropland N) is not always a raw value in the
#' input data -- some datasets provide it precomputed, others expect it
#' derived as the sum of the five underlying crop-type components. Adds a
#' \code{\{CRPLNDTOTN\}} row for each province/year that has all five
#' components as numeric values but no \code{\{CRPLNDTOTN\}} row of its own;
#' leaves data that already has it untouched. A province/year missing a
#' component, or with a non-numeric one, gets no row at all rather than a
#' partial total.
#'
#' @return The input data frame \code{d}, with derived
#'   \code{\{CRPLNDTOTN\}} rows appended where applicable.
#'
#' @keywords internal
add_crplndtotn <- function(d) {
  components <- c("{PERrN}", "{PERiN}", "{NPErN}", "{NPEiN}", "{GREHN}")
  comp_rows <- d[d$label %in% components, ]
  comp_rows$data <- suppressWarnings(as.numeric(comp_rows$data))
  comp_rows <- comp_rows[!is.na(comp_rows$data), ]
  if (nrow(comp_rows) == 0) {
    return(d)
  }

  has_total <- paste(d$province, d$year)[d$label == "{CRPLNDTOTN}"]
  groups <- split(comp_rows, paste(comp_rows$province, comp_rows$year))
  groups <- groups[!names(groups) %in% has_total]
  totals <- lapply(groups, crplndtotn_row, components = components)
  totals <- totals[!vapply(totals, is.null, logical(1))]
  if (length(totals) == 0) {
    return(d)
  }

  rbind(d, do.call(rbind, totals))
}

crplndtotn_row <- function(rows, components) {
  rows <- rows[!duplicated(rows$label), ]
  if (!all(components %in% rows$label)) {
    return(NULL)
  }

  # Built from a component row so every column of the input data -- including
  # any the package does not know about -- is carried over unchanged.
  row <- rows[1, ]
  row$label <- "{CRPLNDTOTN}"
  row$data <- sum(rows$data)
  row
}

prepare_data <- function(csv_path) {
  d <- readr::read_csv(csv_path, show_col_types = FALSE)
  d <- unique(d)
  d <- add_crplndtotn(d)
  d[!grepl("WIDTH_MAX", d$label), ]
}

read_width_max <- function(csv_path) {
  d <- readr::read_csv(csv_path, show_col_types = FALSE)
  d[grepl("WIDTH_MAX", d$label), ]
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
                          t_id_arrow, max_width_arrows, val_max_width,
                          min_bubble_size = 10) {
  arrowcolors <- unique(x$arrowColor[!is.na(x$arrowColor)])
  arrowcolor <- if (length(arrowcolors) > 0) arrowcolors[1] else NA
  value_change <- NA

  is_text_label <- label %in% c("{PROVINCE_NAME}", "{YEAR}")

  if (label == "{PROVINCE_NAME}") {
    mean_val <- x$data[1]
  } else if (label == "{YEAR}") {
    mean_val <- year_info(years)
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

    bubble_label_raw <- t_id_arrow$labelchange[t_id_arrow$label == label]
    has_bubble <- length(bubble_label_raw) > 0 && !is.na(bubble_label_raw[1])

    if (plot_change && !is.na(value_change)) {
      if (has_bubble) {
        bubble_label <- paste0("{", bubble_label_raw[1], "}")
        bubble_color <- ifelse(value_change > 0, increase_color, decrease_color)
        bubble_size <- min(max(min_bubble_size, sqrt(abs(value_change)) * 30), 45)
        doc <- change_bubble_size(doc, bubble_label, bubble_size, 12,
                                  fill_color = bubble_color)
        doc <- replace_label_in_value(doc, bubble_label,
                                      paste0(round(value_change), "%"))
      }
    } else if (plot_change && is.na(value_change) && has_bubble) {
      bubble_label <- paste0("{", bubble_label_raw[1], "}")
      doc <- suppressWarnings(remove_id(doc, bubble_label))
    }
  }

  doc <- replace_label_in_value(doc, label, mean_val)
  doc
}

process_labels_loop <- function(doc, dact, dactch, years, years_change,
                                decimals, plot_change, increase_color,
                                decrease_color, t_id_arrow,
                                max_width_arrows, val_max_width,
                                min_bubble_size = 10) {
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
      t_id_arrow, max_width_arrows, val_max_width, min_bubble_size
    )
  }
  doc
}

process_period_region <- function(period_idx, period, regions, d,
                                  xml_base, path_outputs, drawio_exe,
                                  decimals, val_max_width,
                                  increase_color, decrease_color,
                                  t_id_arrow, overwrite,
                                  max_width_arrows, d_width_max,
                                  min_bubble_size, unit) {
  years <- period$years
  years_change <- period$prev_years
  plot_change <- !is.null(years_change)
  outputs <- character()

  for (region in regions) {
    dact <- select_region_data(d, region, years)

    if (nrow(dact) == 0) {
      warning("No data found for region '", region,
              "' in years ", min(years), "-", max(years), ". Skipping.")
      next
    }

    dactch <- select_region_data(d, region, years_change)

    wm_rows <- d_width_max[d_width_max$province == region &
                             d_width_max$year %in% years, ]
    region_val_max_width <- if (nrow(wm_rows) > 0) {
      round(mean(as.numeric(wm_rows$data)))
    } else {
      val_max_width
    }

    filename <- sprintf(
      "GRAFS_%s_P%d_MEAN_%s", region, period_idx, year_info(years)
    )
    xml_out <- file.path(path_outputs, "xml", paste0(filename, ".xml"))
    png_out <- file.path(path_outputs, "png", paste0(filename, ".png"))

    if (file.exists(xml_out) && !overwrite) {
      outputs <- c(outputs, png_out)
      next
    }

    doc <- xml2::read_xml(xml_base) |>
      process_labels_loop(
        dact, dactch, years, years_change,
        decimals, plot_change, increase_color, decrease_color,
        t_id_arrow, max_width_arrows, region_val_max_width, min_bubble_size
      )

    if (!plot_change) {
      doc <- remove_change_bubbles(doc)
    }

    year_change_text <- if (plot_change) paste0("(", year_info(years_change), ")") else ""
    doc <- replace_label_in_value(doc, "{YEARCHANGE}", year_change_text)
    doc <- replace_label_in_value(doc, "{WIDTH_MAX}", as.character(region_val_max_width))
    doc <- replace_label_in_value(doc, "{UNIT}", unit)

    writeLines(as.character(doc), xml_out, useBytes = TRUE)
    create_png(drawio_exe, xml_out, png_out)
    outputs <- c(outputs, png_out)
  }
  outputs
}

# Exported functions ----------------------------------------------------------

#' Create GRAFS Diagrams
#'
#' Generate GRAFS (Generalized Representation of Agro-Food Systems) diagrams
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
#'   \code{system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")},
#'   drawn from the national-scale nitrogen budget (1990-2015) published in
#'   Rodriguez et al. (2023) -- see \code{References}.
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
#' @param min_bubble_size Minimum diameter of a change bubble, in diagram
#'   units (default 10). Change bubbles scale with the square root of the
#'   percentage change, floored at this size so small changes stay legible
#'   and capped at 45 so large ones do not overflow the diagram.
#' @param unit Unit label displayed in the Reference box and next to total N
#'   values (default \code{"MgN"}). Set to \code{"GgN"} for gigagram data, or
#'   any string matching the units of your input CSV.
#' @param overwrite Overwrite existing output files (default \code{TRUE}).
#'
#' @return Invisible character vector of output PNG file paths.
#'
#' @references
#' Billen, G., et al. (2014). \emph{Regional Studies}.
#'
#' Lassaletta, L., et al. (2015). \emph{Global Biogeochemical Cycles}.
#'
#' Le Noe, J., et al. (2017). \emph{Science of the Total Environment}.
#'
#' Rodriguez, A., Sanz-Cobena, A., Ruiz-Ramos, M., Aguilera, E., Quemada, M.,
#' Billen, G., Garnier, J., Lassaletta, L. (2023). Nesting nitrogen budgets
#' through spatial and system scales in the Spanish agro-food system over 26
#' years. \emph{Science of The Total Environment}, 892, 164467.
#' \url{https://doi.org/10.1016/j.scitotenv.2023.164467}
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
#'   regions = "spain",
#'   periods = list(list(years = 1990:1991))
#' )
#'
#' # Compare two periods with change bubbles
#' create_GRAFS(
#'   csv_inputs = csv,
#'   path_outputs = tempdir(),
#'   xml_base = xml,
#'   arrows_csv = ids,
#'   drawio_exe = drawio,
#'   regions = "spain",
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
                         min_bubble_size = 10,
                         unit = "MgN",
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
  d_width_max <- read_width_max(csv_inputs)

  outputs <- character()
  for (i in seq_along(periods)) {
    paths <- process_period_region(
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
      max_width_arrows = max_width_arrows,
      d_width_max = d_width_max,
      min_bubble_size = min_bubble_size,
      unit = unit
    )
    outputs <- c(outputs, paths)
  }

  invisible(outputs)
}
