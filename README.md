# GRAFS

R package for automated creation of GRAFS (General Representation of Agro-Food Systems) diagrams.

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
  regions = "Albacete",
  periods = list(list(years = 1930:1931))
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
  regions = "Albacete",
  periods = list(
    list(years = 2011:2015, prev_years = 1990:1994)
  )
)
```

Change bubbles are colored by direction: blue for increases, green for decreases (configurable via `increase_color` and `decrease_color`).

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

The package includes example data for Spanish provinces (`GRAFS_spain_data.csv.gz`).

## Validating inputs

Before generating diagrams, check that your data, template, and arrow mappings
are consistent:

```r
validate_inputs(csv, ids, xml, regions = "Albacete")
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

## Author

Alfredo Rodriguez (alfredo.rodriguez@uclm.es)
