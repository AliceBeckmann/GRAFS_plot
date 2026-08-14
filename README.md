# GRAFS

R package for automated creation of GRAFS (General Representation of Agro-Food Systems) diagrams.

Works with any dataset structured in the expected format -- the bundled example covers Spain at the national level, but your own data could represent a different country, or a provincial/regional level instead. The bundled draw.io template can be revised too -- for example, removing arrows for flows you don't want to show -- as long as the arrow ID mapping stays in sync (see "Arrow ID mapping" below).

Reads flow data from CSV files, populates a [draw.io](https://github.com/jgraph/drawio-desktop) XML template with proportional arrow widths and labels, and exports the result as PNG images.

Based on the methodology described by Billen et al. (2014), Lassaletta et al. (2015), and Le Noe et al. (2017).

## Requirements

- **R** >= 4.1
- **draw.io desktop** - install from [github.com/jgraph/drawio-desktop/releases](https://github.com/jgraph/drawio-desktop/releases)

### Installing draw.io

| OS      | Method |
|---------|--------|
| Linux   | Download `.deb`/`.rpm` from releases, or `sudo snap install drawio` |
| macOS   | Download `.dmg` from releases, or `brew install --cask drawio`. Then: `chmod +x /Applications/draw.io.app/Contents/MacOS/draw.io` |
| Windows | Download the installer from releases |

## Installation

```r
# Install dependencies
install.packages(c("xml2", "readr", "processx"))

# Install from local source
install.packages("path/to/GRAFS_0.2.0.tar.gz", repos = NULL, type = "source")

# Or using devtools from a git repository
# devtools::install_github("user/GRAFS")
```

## Quick start

```r
library(GRAFS)

# Verify your setup
check_setup()

# Locate draw.io
drawio <- find_drawio()

# Use bundled example data
csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

# Generate a GRAFS diagram
create_GRAFS(
  csv_inputs = csv,
  path_outputs = "./output",
  xml_base = xml,
  arrows_csv = ids,
  drawio_exe = drawio,
  regions = "spain",
  periods = list(list(years = 1990:1991))
)
```

Output files are saved to `./output/xml/` and `./output/png/`.

## Comparing periods

To show percentage changes between two time periods:

```r
create_GRAFS(
  csv_inputs = csv,
  path_outputs = "./output",
  xml_base = xml,
  arrows_csv = ids,
  drawio_exe = drawio,
  regions = "spain",
  periods = list(
    list(years = 2011:2015, prev_years = 1990:1994)
  )
)
```

Change bubbles are colored by direction: blue for increases, green for decreases (configurable via `increase_color` and `decrease_color`).

To turn off the change bubbles, delete the whole `prev_years = ...` part (a
period with no `prev_years` gets a plain diagram).

## Input data format

The input CSV must have these columns:

| Column      | Description |
|-------------|-------------|
| `province`  | Region/geographic unit name |
| `year`      | Integer year |
| `label`     | Flow label matching the draw.io template (e.g., `{CROPS_TO_POP}`) |
| `data`      | Numeric value (dot as decimal separator) |
| `align`     | Text alignment: `"L"` or `"R"` |
| `arrowColor`| Optional hex color override (e.g., `#FF0000`), or `NA` |

The package includes example data for Spain at the national level, 1990-2015
(`GRAFS_spain_data.csv.gz`), from Rodriguez et al. (2023,
<https://doi.org/10.1016/j.scitotenv.2023.164467>).

## Arrow ID mapping

The `arrows_csv` file maps flow labels to draw.io element IDs. The bundled
`GRAFS_arrows_ids.csv` maps the 25 flows used in the bundled template.

You can revise the draw.io template itself -- for example, removing arrows
for flows you don't want to show, or don't have data for. If you remove an
arrow from the template, remove its row from `arrows_csv` too; if you add a
new arrow, add a row mapping its label to its new draw.io element ID.

## Validating inputs

Before generating diagrams, check that your data, template, and arrow mappings
are consistent:

```r
validate_inputs(csv, ids, xml, regions = "spain")
```

This reports matched labels, unmapped data, missing regions, and data quality
issues.

## Key parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max_width_arrows` | 25 | Arrow width for the maximum value |
| `val_max_width` | 1000 | Data value that maps to `max_width_arrows` |
| `decimals` | 0 | Decimal places shown (use 1 for small regions) |
| `overwrite` | TRUE | Whether to overwrite existing output files |

## License

CC BY-NC 4.0

## Authors

Alfredo Rodriguez (alfredo.rodriguez@uclm.es), Catalin Covaci, Alice Beckmann (alice.beckmann@cchs.csic.es, maintainer), Luis Lassaletta
