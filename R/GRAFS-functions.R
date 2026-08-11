remove_id <- function(doc, id) {
  cells <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[contains(@value, '{id}')]")
  )
  if (length(cells) == 0) {
    warning(paste("No mxCell found with id:", id))
  }
  xml2::xml_remove(cells)
  doc
}

maybe_remove_change_bubbles <- function(doc, years_change) {
  if (!is.null(years_change)) {
    return(doc)
  }

  bubbles <- xml2::xml_find_all(
    doc,
    ".//mxCell[contains(@style, 'ellipse') and @value != '']"
  )
  if (length(bubbles) == 0) {
    warning("No ellipse bubbles found")
  }
  xml2::xml_remove(bubbles)
  doc
}

change_bubble_size <- function(
  doc,
  label,
  avg_change,
  NEW_TEXT_SIZE,
  INCREASE_COLOR,
  DECREASE_COLOR
) {
  # TODO: fix for labels without bubble
  if (is.na(label)) {
    return(doc)
  }

  cell <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[contains(@value, '{label}')]")
  )

  if (length(cell) == 0) {
    warning(paste("Cell with label", label, "not found"))
    return(doc)
  }

  bubble_color <- if (!is.na(avg_change) && avg_change < 0) {
    DECREASE_COLOR
  } else {
    INCREASE_COLOR
  }

  style <- xml2::xml_attr(cell, "style")
  style <- stringr::str_replace(
    style,
    "fontSize=\\d+",
    stringr::str_glue("fontSize={NEW_TEXT_SIZE}")
  )
  style <- stringr::str_replace(
    style,
    "fillColor=#[0-9a-fA-F]{6}",
    stringr::str_glue("fillColor={bubble_color}")
  )
  xml2::xml_set_attr(cell, "style", style)

  NEW_SIZE <- min(sqrt(abs(avg_change)) * 30, 45)

  mx_geometry <- xml2::xml_find_first(cell, ".//mxGeometry")
  xml2::xml_set_attr(mx_geometry, "width", NEW_SIZE)
  xml2::xml_set_attr(mx_geometry, "height", NEW_SIZE)

  doc
}

change_style <- function(
  doc,
  label,
  identifier,
  value,
  id,
  MAX_WIDTH_ARROWS,
  VAL_MAX_WIDTH,
  MIN_WIDTH_ARROWS = 1
) {
  cell <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[contains(@value, '{label}')]")
  )
  if (length(cell) == 0) {
    warning(paste("Cell with label", label, "not found"))
    return(doc)
  }

  if (is.na(id)) {
    warning(paste("Arrow for label", label, "not found"))
    return(doc)
  }

  cell <- xml2::xml_find_all(
    doc,
    stringr::str_glue(".//mxCell[@id='{id}']")
  )
  if (length(cell) == 0) {
    warning(paste("Cell for arrow label", label, "not found"))
    return(doc)
  }

  style <- xml2::xml_attr(cell, "style")
  style_parts <- stringr::str_split_1(style, ";")
  style_kv <- lapply(style_parts, function(x) strsplit(x, "=")[[1]])

  style_list <- setNames(
    sapply(style_kv, function(x) if (length(x) > 1) x[2] else ""),
    sapply(style_kv, function(x) x[1])
  )

  if (identifier == "strokeWidth") {
    if (!is.numeric(value)) {
      value <- 1
    } else {
      value <- min(value, VAL_MAX_WIDTH) * MAX_WIDTH_ARROWS / VAL_MAX_WIDTH
    }

    style_list[identifier] <- as.character(
      round(
        max(MIN_WIDTH_ARROWS, min(MAX_WIDTH_ARROWS, value))
      )
    )
  } else {
    style_list[identifier] <- as.character(value)
  }

  new_style <- paste(
    paste(names(style_list), style_list, sep = "="),
    collapse = ";"
  )
  xml2::xml_set_attr(cell, "style", new_style)

  if (value == 0 && identifier == "strokeWidth") {
    doc <- remove_id(doc, label)
    # TODO: revise
    # aux <- T_ID_ARROW$associated_label_id[T_ID_ARROW$id == id]
    # if (!is.na(aux)) {
    #   doc <- remove_id(doc, aux)
    # }
  }

  doc
}

replace_label_in_value <- function(doc, label, value) {
  # TODO: fix for bubble changes
  if (is.na(label)) {
    return(doc)
  }
  # TODO: cleaner implementation, currently for no {YEARCHANGE}
  if (is.na(value)) {
    value <- ""
  }

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
  if (length(YEARS) == 0) {
    return(NA)
  }

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

prepare_directories <- function(PATH_OUTPUTS) {
  dir.create(PATH_OUTPUTS, showWarnings = FALSE)
  dir.create(paste0(PATH_OUTPUTS, "xml/"), showWarnings = FALSE)
  dir.create(paste0(PATH_OUTPUTS, "png/"), showWarnings = FALSE)
}

augment_crplndtotn <- function(dact, dactch = NULL, PLOT_CHANGE = FALSE) {
  x <- which(dact$label == "{CRPLNDTOTN}")
  if (length(x) == 0) {
    xx <- subset(
      dact,
      is.element(
        label,
        c("{PERrN}", "{PERiN}", "{NPErN}", "{NPEiN}", "{GREHN}")
      )
    )
    xx$data <- as.numeric(xx$data)
    xxx <- xx |>
      group_by(province, year, align, arrowColor) |>
      summarise(label = "{CRPLNDTOTN}", data = sum(data))
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

apply_styles_to_label <- function(
  template,
  row,
  max_width_arrows,
  val_max_width,
  min_width_arrows,
  increase_color,
  decrease_color
) {
  template |>
    change_style(
      row$label,
      "strokeWidth",
      row$avg_current,
      row$id,
      max_width_arrows,
      val_max_width,
      MIN_WIDTH_ARROWS = min_width_arrows
    ) |>
    replace_label_in_value(
      row$label,
      if (row$avg_current < 1) {
        format(round(row$avg_current, 2), nsmall = 2)
      } else {
        as.character(round(row$avg_current))
      }
    ) |>
    change_bubble_size(
      row$labelchange,
      row$avg_change,
      12,
      increase_color,
      decrease_color
    ) |>
    replace_label_in_value(
      row$labelchange,
      stringr::str_glue("{round(row$avg_change)} %")
    )
}

replace_region_label <- function(doc, region) {
  doc |>
    replace_label_in_value("{PROVINCE_NAME}", region)
}

replace_year_labels <- function(doc, years, years_change) {
  doc |>
    replace_label_in_value("{YEAR}", year_info(years)) |>
    replace_label_in_value("{YEARCHANGE}", year_info(years_change))
}

create_regional_grafs <- function(
  df,
  region,
  years,
  years_change,
  grafs_template,
  max_width_arrows,
  val_max_width,
  min_width_arrows,
  increase_color,
  decrease_color,
  output_path,
  draw_io_exe
) {
  print(region)
  doc <- xml2::read_xml(grafs_template)

  output_xml_path <- file.path(
    output_path,
    "xml",
    stringr::str_glue("GRAFS_{region}_MEAN_{year_info(years)}.xml")
  )

  df |>
    purrr::pmap(list) |>
    purrr::reduce(
      .init = doc,
      function(template, row) {
        apply_styles_to_label(
          template,
          row,
          max_width_arrows,
          val_max_width,
          min_width_arrows,
          increase_color,
          decrease_color
        )
      }
    ) |>
    maybe_remove_change_bubbles(years_change) |>
    replace_region_label(region) |>
    replace_year_labels(years, years_change) |>
    xml2::write_xml(output_xml_path)

  crea_png(
    draw_io_exe,
    output_xml_path,
    str_replace_all(output_xml_path, "xml", "png")
  )
  cli::cli_alert_info("GRAFS created!")
}

compute_means <- function(df, years, years_change) {
  df |>
    dplyr::filter(year %in% c(years, years_change)) |>
    dplyr::mutate(
      period = ifelse(year %in% years, "current", "previous"),
      period = factor(period, c("current", "previous")),
      data = as.numeric(data)
    ) |>
    dplyr::summarise(
      avg = mean(data),
      .by = c("province", "label", "period")
    ) |>
    tidyr::pivot_wider(
      names_from = "period",
      values_from = avg,
      names_prefix = "avg_",
      names_expand = TRUE
    ) |>
    dplyr::mutate(avg_change = avg_current / avg_previous * 100 - 100)
}

create_GRAFS <- function(
  df,
  output_path,
  grafs_template,
  arrow_ids,
  draw_io_exe,
  regions,
  years,
  years_change = NULL,
  DECIMALES_XML = 0,
  max_width_arrows = 25,
  val_max_width = 100,
  min_width_arrows = 3,
  INCREASE_COLOR = "#97cde5",
  DECREASE_COLOR = "#a9d77f",
  OVERWRITE = TRUE,
  VERBOSE = FALSE,
  UNITch = NA,
  LABELch = NA,
  DATOch = NA
) {
  prepare_directories(output_path)

  df |>
    dplyr::filter(province %in% regions) |>
    compute_means(years, years_change) |>
    dplyr::left_join(arrow_ids) |>
    dplyr::group_by(province) |>
    dplyr::group_walk(
      .f = ~ create_regional_grafs(
        .x,
        .y$province,
        years,
        years_change,
        grafs_template,
        max_width_arrows,
        val_max_width,
        min_width_arrows,
        INCREASE_COLOR,
        DECREASE_COLOR,
        output_path,
        draw_io_exe
      )
    )
}
