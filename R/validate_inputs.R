#' Validate GRAFS Input Files
#'
#' Checks consistency between the input CSV data, the arrow ID mapping,
#' and the draw.io XML template. Reports which data labels have arrow
#' mappings, which template placeholders have data, and any mismatches.
#'
#' This function does not require draw.io and can be used to debug input
#' files before generating diagrams.
#'
#' @param csv_inputs Path to the CSV file containing flow data.
#' @param arrows_csv Path to the CSV mapping flow labels to draw.io element IDs.
#' @param xml_base Path to the draw.io XML template.
#' @param regions Optional character vector of regions to check. If provided,
#'   checks that each region has data in the CSV.
#'
#' @return Invisible list with elements:
#'   \describe{
#'     \item{data_labels}{All unique labels in the CSV data}
#'     \item{arrow_labels}{All labels in the arrow ID mapping}
#'     \item{template_labels}{All label placeholders found in the template}
#'     \item{unmapped_data}{Data labels with no arrow mapping}
#'     \item{unmapped_arrows}{Arrow labels with no data}
#'     \item{missing_in_template}{Arrow labels not found in the template}
#'     \item{ok}{TRUE if no critical issues found}
#'   }
#'
#' @examples
#' \dontrun{
#' csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
#' xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
#' ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
#'
#' result <- validate_inputs(csv, ids, xml, regions = "spain")
#' }
#'
#' @export
validate_inputs <- function(csv_inputs, arrows_csv, xml_base, regions = NULL) {
  ok <- TRUE

  if (!file.exists(csv_inputs)) stop("Input CSV not found: ", csv_inputs)
  if (!file.exists(arrows_csv)) stop("Arrow IDs CSV not found: ", arrows_csv)
  if (!file.exists(xml_base)) stop("XML template not found: ", xml_base)

  # Read inputs
  d <- readr::read_csv(csv_inputs, show_col_types = FALSE)
  d <- add_crplndtotn(d)
  arrows <- readr::read_csv(arrows_csv, show_col_types = FALSE)
  doc <- xml2::read_xml(xml_base)

  data_labels <- unique(d$label)
  arrow_labels <- arrows$label[arrows$label != "{WIDTH_MAX}"]

  # Extract template labels (anything in curly braces in mxCell values)
  cells <- xml2::xml_find_all(doc, ".//mxCell[@value]")
  values <- xml2::xml_attr(cells, "value")
  template_labels <- unique(unlist(regmatches(
    values, gregexpr("\\{[^}]+\\}", values)
  )))

  # Cross-reference
  unmapped_data <- setdiff(data_labels, arrow_labels)
  unmapped_arrows <- setdiff(arrow_labels, data_labels)
  missing_in_template <- setdiff(
    arrow_labels,
    template_labels
  )

  # Report

  message("=== GRAFS Input Validation ===\n")

  message("Data CSV: ", length(data_labels), " unique labels, ",
          length(unique(d$province)), " regions, ",
          length(unique(d$year)), " years")
  message("Arrow mapping: ", length(arrow_labels), " mapped flows")
  message("Template: ", length(template_labels), " label placeholders\n")

  # Arrow-mapped labels that have data: these are the ones that will render
  mapped_and_has_data <- intersect(arrow_labels, data_labels)
  message("Matched (arrow + data): ", length(mapped_and_has_data), " labels")

  if (length(unmapped_data) > 0) {
    # Split into labels that appear in template (informational) vs not
    in_template <- intersect(unmapped_data, template_labels)
    not_in_template <- setdiff(unmapped_data, template_labels)

    if (length(in_template) > 0) {
      message("\nData labels in template but no arrow mapping (text-only, OK):")
      message("  ", paste(sort(in_template), collapse = ", "))
    }
    if (length(not_in_template) > 0) {
      message("\nData labels not in template or arrow mapping (ignored):")
      message("  ", paste(sort(not_in_template), collapse = ", "))
    }
  }

  if (length(unmapped_arrows) > 0) {
    ok <- FALSE
    message("\nWARNING - Arrow mappings with NO data (arrows will use default width):")
    message("  ", paste(sort(unmapped_arrows), collapse = ", "))
  }

  if (length(missing_in_template) > 0) {
    ok <- FALSE
    message("\nWARNING - Arrow mappings not found in template:")
    message("  ", paste(sort(missing_in_template), collapse = ", "))
  }

  # Region checks
  if (!is.null(regions)) {
    available <- unique(d$province)
    missing_regions <- setdiff(regions, available)
    if (length(missing_regions) > 0) {
      ok <- FALSE
      message("\nWARNING - Requested regions not found in data:")
      message("  ", paste(missing_regions, collapse = ", "))
      shown <- sort(available)[seq_len(min(10, length(available)))]
      message("  Available: ", paste(shown, collapse = ", "),
              if (length(available) > 10) paste0(" ... (", length(available), " total)"))
    } else {
      message("\nRegion check: all ", length(regions), " requested regions found in data")
    }
  }

  # Data quality
  numeric_labels <- setdiff(data_labels, c("{PROVINCE_NAME}", "{YEAR}"))
  numeric_data <- d[d$label %in% numeric_labels, ]
  na_count <- sum(is.na(suppressWarnings(as.numeric(numeric_data$data))))
  if (na_count > 0) {
    message("\nWARNING - ", na_count, " non-numeric values found in data column")
  }

  if (ok) {
    message("\nNo critical issues found.")
  } else {
    message("\nSome issues found. Review the warnings above.")
  }

  invisible(list(
    data_labels = data_labels,
    arrow_labels = arrow_labels,
    template_labels = template_labels,
    unmapped_data = unmapped_data,
    unmapped_arrows = unmapped_arrows,
    missing_in_template = missing_in_template,
    ok = ok
  ))
}
