html_escape <- function(text) {
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text
}

remove_id <- function(doc, id) {
  id <- html_escape(id)
  cells <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[contains(@value, '{id}')]")
  )
  if (length(cells) == 0) {
    warning(paste("No mxCell found with id:", id))
  }
  doc
}

remove_change_bubbles <- function(doc) {
  bubbles <- xml2::xml_find_all(doc, ".//mxCell[contains(@style, 'ellipse')]")
  if (length(bubbles) == 0) {
    warning("No ellipse bubbles found")
  }
  xml2::xml_remove(bubbles)
  doc
}

change_bubble_size <- function(doc, label, NEW_SIZE, NEW_TEXT_SIZE) {
  cells <- xml2::xml_find_all(doc, ".//mxCell[@value]")
  found <- FALSE
  for (cell in cells) {
    cell_value <- xml2::xml_attr(cell, "value")
    if (!is.na(cell_value) && cell_value == label) {
      style <- xml2::xml_attr(cell, "style")
      style_parts <- strsplit(style, ";")[[1]]
      style_kv <- lapply(style_parts, function(x) strsplit(x, "=")[[1]])
      style_list <- setNames(
        sapply(style_kv, function(x) if (length(x) > 1) x[2] else ""),
        sapply(style_kv, function(x) x[1])
      )
      style_list["fontSize"] <- as.character(NEW_TEXT_SIZE)
      style_list["width"] <- as.character(NEW_SIZE)
      style_list["height"] <- as.character(NEW_SIZE)
      new_style <- paste(
        paste(names(style_list), style_list, sep = "="),
        collapse = ";"
      )
      xml2::xml_set_attr(cell, "style", new_style)
      found <- TRUE
    }
  }
  if (!found) {
    warning(paste("No mxCell found with value:", label))
  }
  doc
}

change_style <- function(doc, id, identifier, value, T_ID_ARROW) {
  id <- html_escape(id)
  cell <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[contains(@value, '{id}')]")
  )
  if (length(cell) == 0) {
    warning(paste("Cell with id", id, "not found"))
    return(doc)
  }
  style <- xml2::xml_attr(cell, "style")
  style_parts <- strsplit(style, ";")[[1]]
  style_kv <- lapply(style_parts, function(x) strsplit(x, "=")[[1]])
  style_list <- setNames(
    sapply(style_kv, function(x) if (length(x) > 1) x[2] else ""),
    sapply(style_kv, function(x) x[1])
  )
  if (identifier == "strokeWidth") {
    # TODO: make cleaner
    if (!is.numeric(value)) {
      value <- 1
    }
    style_list[identifier] <- as.character(round(max(1, value)))
  } else {
    style_list[identifier] <- as.character(value)
  }
  new_style <- paste(
    paste(names(style_list), style_list, sep = "="),
    collapse = ";"
  )
  xml2::xml_set_attr(cell, "style", new_style)
  if (value == 0 && identifier == "strokeWidth") {
    doc <- remove_id(doc, id)
    # TODO: revise
    # aux <- T_ID_ARROW$associated_label_id[T_ID_ARROW$id == id]
    # if (!is.na(aux)) {
    #   doc <- remove_id(doc, aux)
    # }
  }
  doc
}

replace_label_in_value <- function(doc, label, value) {
  label <- html_escape(label)
  cells <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[contains(@value, '{label}')]")
  )
  if (length(cells) == 0) {
    warning(paste("No mxCell found with label:", label))
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

crea_png <- function(exe_draw_io, xml_in, png_out) {
  p <- process$new(exe_draw_io, c("-x", paste0("-o", png_out), xml_in))
  p$wait()
  print(paste0(png_out, " created!"))
}

year_info <- function(YEARS) {
  label <- YEARS[1]
  for (i in 2:length(YEARS)) {
    if (YEARS[i] != (YEARS[i - 1] + 1)) {
      label <- paste0(label, "-", YEARS[i - 1], "_", YEARS[i])
    } else {
      if (i == length(YEARS)) {
        label <- paste0(label, "-", YEARS[i])
      }
    }
  }
  for (i in 1:length(unique(YEARS))) {
    label <- str_replace_all(
      label,
      paste0(YEARS[i], "-", YEARS[i]),
      paste0(YEARS[i], "")
    )
  }
  return(label)
}

prepare_data <- function(XLSX_INPUTS) {
  d <- openxlsx::read.xlsx(XLSX_INPUTS)
  d <- unique(d)

  # TODO: handle these cleanly, so annoying
  d$label <- gsub("&lt;", "<", d$label)
  d$label <- gsub("&gt;", ">", d$label)

  subset(d, !grepl("WIDTH_MAX", label))
}

prepare_directories <- function(PATH_OUTPUTS) {
  dir.create(PATH_OUTPUTS, showWarnings = FALSE)
  dir.create(paste0(PATH_OUTPUTS, "xml/"), showWarnings = FALSE)
  dir.create(paste0(PATH_OUTPUTS, "png/"), showWarnings = FALSE)
}

augment_crplndtotn <- function(dact, dactch = NULL, PLOT_CHANGE = FALSE) {
  x <- which(dact$label == "<CRPLNDTOTN>")
  if (length(x) == 0) {
    xx <- subset(
      dact,
      is.element(
        label,
        c("<PERrN>", "<PERiN>", "<NPErN>", "<NPEiN>", "<GREHN>")
      )
    )
    xx$data <- as.numeric(xx$data)
    xxx <- xx %>%
      group_by(province, year, align, arrowColor) %>%
      summarise(label = "<CRPLNDTOTN>", data = sum(data))
    xxx <- xxx[, c("province", "year", "label", "data", "align", "arrowColor")]
    dact <- rbind(dact, xxx)
    if (PLOT_CHANGE && !is.null(dactch)) {
      xxxch <- xxx
      xxxch$data <- NA
      dactch <- rbind(dactch, xxx)
    }
  }
  list(dact = dact, dactch = dactch)
}

apply_unit_label_changes <- function(dact, dactch, UNITch, LABELch) {
  UNITch_flat <- unlist(UNITch)
  if (!any(is.na(UNITch_flat))) {
    for (ii in seq_along(UNITch)) {
      dact$data[dact$label == UNITch[[ii]]$old] <- as.numeric(dact$data[
        dact$label == UNITch[[ii]]$old
      ]) /
        UNITch[[ii]]$div
      dact$label[dact$label == UNITch[[ii]]$old] <- UNITch[[ii]]$new
      dactch$label[dactch$label == UNITch[[ii]]$old] <- UNITch[[ii]]$new
    }
  }
  LABELch_flat <- unlist(LABELch)
  if (!any(is.na(LABELch_flat))) {
    for (ii in seq_along(LABELch)) {
      # This only changes XML styles, not data
      # The actual style change is done in XML doc preparation
      next
    }
  }
  list(dact = dact, dactch = dactch)
}

process_label <- function(
  doc,
  x,
  xch,
  lact,
  YEARS,
  YEARS_CHANGE,
  DECIMALES_XML,
  PLOT_CHANGE,
  INCREASE_COLOR,
  DECREASE_COLOR,
  T_ID_ARROW
) {
  arrowcolor <- unique(x$arrowColor)
  value_change <- NA
  if (lact == "<PROVINCE_NAME>") {
    mean_val <- x$data[1]
  } else if (lact == "<YEAR>") {
    mean_val <- paste0("mean_", min(YEARS), "-", max(YEARS))
  } else {
    x$data <- as.numeric(x$data)
    mean_val <- round(mean(x$data), DECIMALES_XML)
    if (mean_val == 0) {
      mean_val <- round(mean(x$data), DECIMALES_XML + 1)
    }
    if (PLOT_CHANGE && !is.null(xch)) {
      xch$data <- as.numeric(xch$data)
      value_change <- mean(x$data) * 100 / mean(xch$data) - 100
      if (mean(xch$data) == 0) value_change <- NA
    }
  }
  doc <- change_style(doc, lact, "strokeWidth", mean_val, T_ID_ARROW)
  if (!is.na(arrowcolor)) {
    doc <- change_style(doc, lact, "fillColor", arrowcolor, T_ID_ARROW)
  }
  if (PLOT_CHANGE && !is.na(value_change)) {
    doc <- change_style(
      doc,
      lact,
      "fillColor",
      ifelse(value_change > 0, INCREASE_COLOR, DECREASE_COLOR),
      T_ID_ARROW
    )
    doc <- change_bubble_size(doc, lact, abs(value_change), 12)
  }
  doc <- replace_label_in_value(doc, lact, mean_val)
  doc
}

parse_period <- function(tPER) {
  if (grepl("-", tPER)) {
    PLOT_CHANGE <- TRUE
    aux <- str_split(tPER, "-")[[1]]
    YEARS <- as.numeric(aux[1])
    YEARS_CHANGE <- as.numeric(aux[2])
  } else {
    PLOT_CHANGE <- FALSE
    YEARS <- stringr::str_split_1(tPER, ":") |> as.integer()
    YEARS_CHANGE <- NULL
  }
  list(PLOT_CHANGE = PLOT_CHANGE, YEARS = YEARS, YEARS_CHANGE = YEARS_CHANGE)
}

select_region_data <- function(d, PROV_ACT, YEARS, YEARS_CHANGE, PLOT_CHANGE) {
  dact <- subset(d, province == PROV_ACT & is.element(year, YEARS))
  dactch <- if (PLOT_CHANGE) {
    subset(d, province == PROV_ACT & is.element(year, YEARS_CHANGE))
  } else {
    NULL
  }
  list(dact = dact, dactch = dactch)
}

initialize_xml_doc <- function(
  XML_BASE,
  PLOT_CHANGE,
  INCREASE_COLOR,
  T_ID_ARROW,
  LABELch,
  VAL_MAX_WIDTH
) {
  doc <- xml2::read_xml(XML_BASE)
  if (PLOT_CHANGE) {
    doc <- change_style(
      doc,
      "<YEARCHANGE>",
      "fillColor",
      INCREASE_COLOR,
      T_ID_ARROW
    )
  }
  LABELch_flat <- unlist(LABELch)
  if (!any(is.na(LABELch_flat))) {
    for (ii in seq_along(LABELch)) {
      doc <- change_style(
        doc,
        LABELch[[ii]]$old,
        "fillColor",
        LABELch[[ii]]$new,
        T_ID_ARROW
      )
    }
  }
  doc <- change_style(
    doc,
    "<WIDTH_MAX>",
    "strokeWidth",
    VAL_MAX_WIDTH,
    T_ID_ARROW
  )
  doc
}

process_labels_loop <- function(
  doc,
  dact,
  dactch,
  YEARS,
  YEARS_CHANGE,
  DECIMALES_XML,
  PLOT_CHANGE,
  INCREASE_COLOR,
  DECREASE_COLOR,
  T_ID_ARROW
) {
  for (lact in unique(dact$label)) {
    x <- dact[dact$label == lact, ]
    xch <- if (PLOT_CHANGE) dactch[dactch$label == lact, ] else NULL
    if (length(x[, 1]) != length(YEARS)) {
      warning("something_is_missing_check")
    }
    if (
      PLOT_CHANGE && !is.null(xch) && length(xch[, 1]) != length(YEARS_CHANGE)
    ) {
      warning("something_is_missing_check")
    }
    doc <- process_label(
      doc,
      x,
      xch,
      lact,
      YEARS,
      YEARS_CHANGE,
      DECIMALES_XML,
      PLOT_CHANGE,
      INCREASE_COLOR,
      DECREASE_COLOR,
      T_ID_ARROW
    )
  }
  doc
}

process_period_region <- function(
  PERIOD,
  tPER,
  PERIODS,
  REGIONS,
  d,
  XML_BASE,
  PATH_OUTPUTS,
  DRAW_IO_EXE,
  DECIMALES_XML,
  VAL_MAX_WIDTH,
  INCREASE_COLOR,
  DECREASE_COLOR,
  T_ID_ARROW,
  UNITch,
  LABELch,
  OVERWRITE
) {
  period_info <- parse_period(tPER)
  PLOT_CHANGE <- period_info$PLOT_CHANGE
  YEARS <- period_info$YEARS
  YEARS_CHANGE <- period_info$YEARS_CHANGE

  for (PROV_ACT in REGIONS) {
    region_data <- select_region_data(
      d,
      PROV_ACT,
      YEARS,
      YEARS_CHANGE,
      PLOT_CHANGE
    )
    dact <- region_data$dact
    dactch <- region_data$dactch

    aug <- augment_crplndtotn(dact, dactch, PLOT_CHANGE)
    dact <- aug$dact
    dactch <- aug$dactch

    changes <- apply_unit_label_changes(dact, dactch, UNITch, LABELch)
    dact <- changes$dact
    dactch <- changes$dactch

    FACT_XML <- paste0(
      PATH_OUTPUTS,
      "xml/",
      "GRAFS_",
      PROV_ACT,
      "_P",
      PERIOD,
      "_MEAN_",
      year_info(YEARS),
      ".xml"
    )
    if (file.exists(FACT_XML) & !OVERWRITE) {
      next
    }

    doc <- initialize_xml_doc(
      XML_BASE,
      PLOT_CHANGE,
      INCREASE_COLOR,
      T_ID_ARROW,
      LABELch,
      VAL_MAX_WIDTH
    )
    doc <- process_labels_loop(
      doc,
      dact,
      dactch,
      YEARS,
      YEARS_CHANGE,
      DECIMALES_XML,
      PLOT_CHANGE,
      INCREASE_COLOR,
      DECREASE_COLOR,
      T_ID_ARROW
    )
    if (!PLOT_CHANGE) {
      doc <- remove_change_bubbles(doc)
    }
    writeLines(as.character(doc), FACT_XML)
    crea_png(DRAW_IO_EXE, FACT_XML, str_replace_all(FACT_XML, "xml", "png"))
    print(paste0("GRAFS creado!"))
  }
}

create_GRAFS <- function(
  XLSX_INPUTS,
  PATH_OUTPUTS,
  XML_BASE,
  TABLA_ID_XML,
  DRAW_IO_EXE,
  REGIONS,
  PERIODS,
  DECIMALES_XML = 0,
  MAX_WIDTH_ARROWS = 25,
  VAL_MAX_WIDTH = 1000,
  INCREASE_COLOR = "#97cde5",
  DECREASE_COLOR = "#a9d77f",
  OVERWRITE = TRUE,
  VERBOSE = FALSE,
  UNITch = NA,
  LABELch = NA,
  DATOch = NA
) {
  T_ID_ARROW <- openxlsx::read.xlsx(TABLA_ID_XML)
  prepare_directories(PATH_OUTPUTS)
  d <- prepare_data(XLSX_INPUTS)
  for (PERIOD in seq_along(PERIODS)) {
    tPER <- PERIODS[PERIOD]
    process_period_region(
      PERIOD,
      tPER,
      PERIODS,
      REGIONS,
      d,
      XML_BASE,
      PATH_OUTPUTS,
      DRAW_IO_EXE,
      DECIMALES_XML,
      VAL_MAX_WIDTH,
      INCREASE_COLOR,
      DECREASE_COLOR,
      T_ID_ARROW,
      UNITch,
      LABELch,
      OVERWRITE
    )
  }
}
