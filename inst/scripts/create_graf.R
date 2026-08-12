create_GRAFS(
  csv_inputs = "inst/extdata/GRAFS_spain_data.csv.gz", # your data file
  path_outputs = "./output", # where to save results
  xml_base = "inst/templates/grafs_auto_v18.xml", # draw.io template
  arrows_csv = "inst/extdata/GRAFS_arrows_ids.csv", # flow → element ID mappings
  drawio_exe = find_drawio(), # auto-detects draw.io
  regions = "spain", # column name for regions
  # years to show. Add prev_years to compare with an earlier period (shows change in %).
  # To turn this off, either delete the whole "prev_years = ..." part, or set prev_years = 0.
  periods = list(list(years = 2011:2015, prev_years = 2000:2004)),
  val_max_width = 1500,
  max_width_arrows = 20,
  unit = "MgN"
)
