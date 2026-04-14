create_GRAFS(
  csv_inputs = "inst/extdata/GRAFS_spain_data.csv.gz", # your data file
  path_outputs = "./output", # where to save results
  xml_base = "inst/templates/grafs_auto_v18.xml", # draw.io template
  arrows_csv = "inst/extdata/GRAFS_arrows_ids.csv", # flow → element ID mappings
  drawio_exe = find_drawio(), # auto-detects draw.io
  regions = "Albacete", # column name for regions
  periods = list(list(years = 2011:2015, prev_years = 2007:2010)), # time period(s) to visualize
  val_max_width = 60000,
  max_width_arrows = 10
)
