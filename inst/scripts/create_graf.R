create_GRAFS(
  # bundled example files -- system.file() finds them whether the package is
  # installed or just loaded from source via devtools::load_all()
  csv_inputs = system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS"), # your data file
  path_outputs = "./output", # where to save results
  xml_base = system.file("templates", "grafs_auto_v18.xml", package = "GRAFS"), # draw.io template
  arrows_csv = system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS"), # flow → element ID mappings
  drawio_exe = find_drawio(), # auto-detects draw.io
  regions = "spain", # column name for regions
  # years to show. Add prev_years to compare with an earlier period (shows change in %).
  # To turn this off, either delete the whole "prev_years = ..." part, or set prev_years = 0.
  periods = list(list(years = 2011:2015, prev_years = 2000:2004)),
  val_max_width = 1500,
  max_width_arrows = 20,
  unit = "MgN"
)
