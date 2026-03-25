test_that("select_region_data filters by province and years", {
  d <- data.frame(
    province = c("A", "A", "B", "B"),
    year = c(2000, 2001, 2000, 2001),
    label = c("{X}", "{X}", "{X}", "{X}"),
    data = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "A", 2000:2001)
  expect_equal(nrow(result), 2)
  expect_true(all(result$province == "A"))

  result <- GRAFS:::select_region_data(d, "B", 2000)
  expect_equal(nrow(result), 1)
  expect_equal(result$data, 3)
})

test_that("select_region_data returns empty for unknown region", {
  d <- data.frame(
    province = c("A"),
    year = c(2000),
    label = c("{X}"),
    data = c(1),
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "MISSING", 2000)
  expect_equal(nrow(result), 0)
})

test_that("select_region_data handles NULL years", {
  d <- data.frame(
    province = c("A", "A"),
    year = c(2000, 2001),
    label = c("{X}", "{X}"),
    data = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "A", NULL)
  expect_equal(nrow(result), 0)
})

test_that("select_region_data handles multiple year ranges", {
  d <- data.frame(
    province = c("A", "A", "A", "A"),
    year = c(2000, 2001, 2005, 2006),
    label = rep("{X}", 4),
    data = 1:4,
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "A", c(2000, 2005))
  expect_equal(nrow(result), 2)
  expect_equal(result$year, c(2000, 2005))
})

test_that("prepare_directories creates xml and png subdirs", {
  tmp <- file.path(tempdir(), paste0("grafs_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  GRAFS:::prepare_directories(tmp)
  expect_true(dir.exists(file.path(tmp, "xml")))
  expect_true(dir.exists(file.path(tmp, "png")))
})

test_that("prepare_directories is idempotent", {
  tmp <- file.path(tempdir(), paste0("grafs_test_idem_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  GRAFS:::prepare_directories(tmp)
  # Second call should not error
  expect_no_error(GRAFS:::prepare_directories(tmp))
  expect_true(dir.exists(file.path(tmp, "xml")))
})

test_that("prepare_data reads CSV and removes WIDTH_MAX rows", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FLOW},100,L,NA",
    "A,2000,{FLOW},100,L,NA",
    "A,2000,{WIDTH_MAX},999,L,NA"
  ), tmp_csv)

  result <- GRAFS:::prepare_data(tmp_csv)
  expect_equal(nrow(result), 1)
  expect_false(any(grepl("WIDTH_MAX", result$label)))
})

test_that("prepare_data deduplicates rows", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FLOW_A},100,L,NA",
    "A,2000,{FLOW_A},100,L,NA",
    "A,2000,{FLOW_B},200,R,NA"
  ), tmp_csv)

  result <- GRAFS:::prepare_data(tmp_csv)
  expect_equal(nrow(result), 2)
})

test_that("prepare_data reads gzipped CSV", {
  tmp_gz <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(tmp_gz), add = TRUE)

  con <- gzfile(tmp_gz, "w")
  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FLOW_A},100,L,NA"
  ), con)
  close(con)

  result <- GRAFS:::prepare_data(tmp_gz)
  expect_equal(nrow(result), 1)
  expect_equal(result$label[1], "{FLOW_A}")
})

# --- process_labels_loop ---

test_that("process_labels_loop processes multiple labels", {
  doc <- xml2::read_xml('
    <mxGraphModel>
      <root>
        <mxCell id="0"/>
        <mxCell id="1" parent="0"/>
        <mxCell id="a1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
        <mxCell id="a2" value="{FLOW_B}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
        <mxCell id="prov" value="{PROVINCE_NAME}" style="fontSize=14" parent="1"/>
      </root>
    </mxGraphModel>')

  dact <- data.frame(
    label = c("{FLOW_A}", "{FLOW_B}", "{PROVINCE_NAME}"),
    data = c("500", "250", "TestRegion"),
    arrowColor = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  t_id <- data.frame(
    label = c("{FLOW_A}", "{FLOW_B}"),
    id = c("a1", "a2"),
    stringsAsFactors = FALSE
  )

  doc <- suppressWarnings(GRAFS:::process_labels_loop(
    doc, dact, NULL, 2000, NULL,
    0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
  ))

  # Check that labels got replaced
  a1 <- xml2::xml_find_first(doc, ".//mxCell[@id='a1']")
  expect_equal(xml2::xml_attr(a1, "value"), "500")

  a2 <- xml2::xml_find_first(doc, ".//mxCell[@id='a2']")
  expect_equal(xml2::xml_attr(a2, "value"), "250")

  prov <- xml2::xml_find_first(doc, ".//mxCell[@id='prov']")
  expect_equal(xml2::xml_attr(prov, "value"), "TestRegion")
})

test_that("process_labels_loop warns on mismatched row counts", {
  doc <- xml2::read_xml('
    <mxGraphModel>
      <root>
        <mxCell id="0"/>
        <mxCell id="1" parent="0"/>
        <mxCell id="a1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      </root>
    </mxGraphModel>')

  # 1 row but expecting 2 years
  dact <- data.frame(
    label = "{FLOW_A}", data = "500", arrowColor = NA,
    stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "a1", stringsAsFactors = FALSE)

  expect_warning(
    GRAFS:::process_labels_loop(
      doc, dact, NULL, 2000:2001, NULL,
      0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
    ),
    "Expected 2 rows"
  )
})

test_that("process_labels_loop warns on mismatched change row counts", {
  doc <- xml2::read_xml('
    <mxGraphModel>
      <root>
        <mxCell id="0"/>
        <mxCell id="1" parent="0"/>
        <mxCell id="a1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      </root>
    </mxGraphModel>')

  # Current period has 1 row, change period expects 2
  dact <- data.frame(
    label = "{FLOW_A}", data = "500", arrowColor = NA,
    stringsAsFactors = FALSE
  )
  dactch <- data.frame(
    label = "{FLOW_A}", data = "400", arrowColor = NA,
    stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "a1", stringsAsFactors = FALSE)

  expect_warning(
    GRAFS:::process_labels_loop(
      doc, dact, dactch, 2000, 1990:1991,
      0, TRUE, "#97cde5", "#a9d77f", t_id, 25, 1000
    ),
    "Expected 2 rows for change label"
  )
})

# --- create_png ---

test_that("create_png warns on failed export", {
  # Create a script that exits with non-zero status
  fake_exe <- tempfile(fileext = if (.Platform$OS.type == "windows") ".bat" else "")
  on.exit(unlink(fake_exe), add = TRUE)

  if (.Platform$OS.type == "windows") {
    writeLines("@echo off\nexit /b 1", fake_exe)
  } else {
    writeLines("#!/bin/sh\nexit 1", fake_exe)
    Sys.chmod(fake_exe, "755")
  }

  tmp_xml <- tempfile(fileext = ".xml")
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(c(tmp_xml, tmp_png)), add = TRUE)
  writeLines("<x/>", tmp_xml)

  expect_warning(
    GRAFS:::create_png(fake_exe, tmp_xml, tmp_png),
    "draw.io export failed"
  )
})

test_that("create_png returns png path invisibly", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  result <- GRAFS:::create_png(drawio, xml, tmp_png)
  expect_equal(result, tmp_png)
})

test_that("create_png handles timeout gracefully", {
  # Create a script that sleeps longer than the timeout
  fake_exe <- tempfile(fileext = if (.Platform$OS.type == "windows") ".bat" else "")
  on.exit(unlink(fake_exe), add = TRUE)

  if (.Platform$OS.type == "windows") {
    writeLines("@echo off\nping -n 30 127.0.0.1 > nul", fake_exe)
  } else {
    writeLines("#!/bin/sh\nsleep 30", fake_exe)
    Sys.chmod(fake_exe, "755")
  }

  tmp_xml <- tempfile(fileext = ".xml")
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(c(tmp_xml, tmp_png)), add = TRUE)
  writeLines("<x/>", tmp_xml)

  # Use a very short timeout (2 seconds)
  expect_warning(
    GRAFS:::create_png(fake_exe, tmp_xml, tmp_png, timeout = 2),
    "timed out"
  )
})
