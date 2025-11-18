\name{create_GRAFS}
\alias{create_GRAFS}
\title{GRAFS plotter function}
\usage{
create_GRAFS(
  XLSX_INPUTS,
  PATH_OUTPUTS,
  XML_BASE,
  TABLA_ID_XML,
  DRAW_IO_EXE,
  REGIONS,
  PERIODS,
  DECIMALES_XML=0,
  MAX_WIDTH_ARROWS=25,
  VAL_MAX_WIDTH=1000,
  INCREASE_COLOR="#97cde5",
  DECREASE_COLOR="#a9d77f",
  OVERWRITE=TRUE,
  VERBOSE=FALSE,
  UNITch=NA,
  LABELch=NA,
  DATOch=NA
)
}
\description{
This function is the main function of the package, as the rest are internal auxiliary functions. By configuring the input files and parameters,
the GRAFS can be generated.In the description of each argument, you will find information on how to access and configure the required base files.
The function requires the draw.io software to be installed (available at \code{https://github.com/jgraph/drawio-desktop}) on the computer,
and the input data must be properly formatted in Excel.
The software has been thoroughly tested on:
Windows 11 with draw.io version 15.4.0 and version 27.0.9
macOS Sequoia 15.5 with draw.io version 26.2.15
It is also prepared to be used in a Linux environment.
To install the package, first install the dependencies using
\code{install.packages(c('openxlsx', 'tidyr', 'readtext', 'XML', 'processx', 'dplyr'))}
and then install the package using
\code{install.packages("<PATH_TO_PACKAGE>/GRAFS_0.1.tar.gz")}
THIS IS A BETA VERSION OF THE PACKAGE THAT HAS NOT YET BEEN PUBLISHED. PLEASE DO NOT REDISTRIBUTE IT FOR NOW.
This software is provided "as is", without any warranties or guarantees.
The author assumes no responsibility for any errors, data loss, or unintended consequences resulting from its use.
Users are solely responsible for verifying the accuracy and suitability of the software for their specific needs.
}

\arguments{
\item{XLSX_INPUTS}{ Indicates the path to the XLSX file containing the information used to create the GRAFS.
The package includes data for Spain for the period 1990–2015, from Rodríguez, A., et al. (2023). Nesting nitrogen budgets through spatial and system scales in the Spanish agro-food system over 26 years. Science of the Total Environment 892: 164467.
To access it, you can use \code{system.file("extdata", "GRAFS_spain_data.xlsx", package = "GRAFS")}
Within the spreadsheet, the first column indicates the geographic unit. Each geographic unit will generate a GRAFS. This is followed by the year, and then a label code. This code refers to each of the labels in the draw.io template.
The package will appropriately replace the label with the corresponding value (aggregating years as specified below with the other parameters). To access the template, you can use
\code{system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")}
In the template, you can see which flow each arrow corresponds to in order to provide the appropriate value.
The next column is the value, using a dot as the decimal separator, ending with a label indicating whether the replaced text is aligned to the left or right.
The last column, arrowColor, is optional but allows you to adjust the color of one or more arrows as desired by including the hexadecimal code. For example, if you include the code #FF0000, the arrow with that value will be displayed in bright red. }
\item{PATH_OUTPUTS}{
    Indicates the directory where the GRAFS will be created. If it does not exist, the directory will be created.
}
\item{XML_BASE}{
    Indicates the path to the draw.io template used as a base. Although it can be modified by the user, a basic one is provided here:
    \code{XML_BASE <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")}
}
\item{TABLA_ID_XML}{
    This table contains a mapping between different arrows (representing flows), identified by their label, and the internal identifier assigned by the draw.io program. It should not be modified. It should only be changed if arrows are added or altered to introduce new ones, providing the associated internal identifier.

    Similarly, the label \code{labelchange} is associated with each arrow and a bubble, which, if this approach is used, will show the percentage change of the associated flow compared to the specified base period.

    Lastly, if some flows may be zero, the identifier of the label should be provided to avoid displaying it in the graphic. This is done using \code{associated_label_id}.
}
\item{DRAW_IO_EXE}{
    This is the path to the draw.io program, which is an external requirement of the package. As mentioned earlier, it has been tested on Windows and Mac environments. Typically, in Windows the paths look like \code{"C:/Program Files/draw.io/draw.io.exe"} while on Mac it is usually something like \code{"/Applications/draw.io.app/Contents/MacOS/draw.io"}. Additionally, on Mac, execution permission must be granted, which can be done with \code{chmod +x /Applications/draw.io.app/Contents/MacOS/draw.io}.
}
\item{REGIONS}{
    This is the name of a spatial unit (corresponding to province in the base xlsx file) that will be represented. It can also be a vector of several spatial units to process them simultaneously.
}
\item{PERIODS}{
    This is a vector with the period or periods to process. You can specify a range of years separated by a colon, or, if you want to show the percentage change, a second group of years separated by a hyphen. For example, the string \code{2011:2015-1990:1994} would mean using the average of the years 2011 to 2015 and comparing it with the average of the period 1990 to 1994.
}
\item{DECIMALES_XML}{
    Indicates the number of decimal places for the values. By default, it is set to 0 (no decimals), but for smaller regions, the use of one decimal is recommended.
}
\item{MAX_WIDTH_ARROWS}{
    This is the width of the arrows associated with the maximum value specified in \code{VAL_MAX_WIDTH}. For example,
    if \code{VAL_MAX_WIDTH} is 1000 and \code{MAX_WIDTH_ARROWS} is 25, it means that a value of 1000 will be assigned an arrow
    width of 25, and the rest proportionally.
}
\item{VAL_MAX_WIDTH}{
    See the explanation of the previous parameter.
}
\item{INCREASE_COLOR}{
    If a comparison period is specified, this will be the color shown in the bubbles when the increase is positive.
}
\item{DECREASE_COLOR}{
    Similar to the previous one, but for decreases.
}
\item{OVERWRITE}{
    If set to \code{TRUE}, then the GRAFS will overwrite any existing ones.
}
\item{VERBOSE}{
    Depending on whether it is enabled or not, it shows more information on the screen about the process.
}
\item{UNITch}{
    In some cases, the units to be displayed may not be appropriate, so a label from
    the \code{XLSX_INPUTS} file can be used to create another one with more suitable units.
    A list of lists should be created, where each element would look like \code{list(old="<PERrha>", new="<PERrKha>;", div=(10^3))}.
    This would indicate that the \code{PERrKha} label is created from the \code{PERrha} data by dividing it by a thousand. This should be combined
    with the following parameters to work properly. In reality, the draw.io template could be modified, but this way different scales
    can be used with the same script and template.
}
\item{LABELch}{
    Changes the text in the labels. For example, the code \code{list(old="Mha", new="Kha")} could be used
    in combination with the above to make the template now expect the \code{PERrKha} data instead of \code{PERrMha}.
}
\item{DATOch}{
    This parameter allows changing the provided data for fine adjustments. It should be used with care. Currently,
    it has only been used to change region names. For example: \code{list(old="p.deasturias", new="Asturias")}
}
}
\examples{

library(stringr)
library(tidyr)
library(readtext)
library(XML)
library(processx)
library(dplyr)
library(openxlsx)
library(GRAFS)

# Get the inputs from the demo data from the GRAFS package
XLSX_INPUTS <- system.file("extdata", "GRAFS_spain_data.xlsx", package = "GRAFS")
XML_BASE <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
TABLA_ID_XML <- system.file("extdata", "GRAFS_arrows_ids.xlsx", package = "GRAFS")

# Mac path
# chmod +x /Applications/draw.io.app/Contents/MacOS/draw.io
DRAW_IO_EXE <- paste0("/Applications/draw.io.app/Contents/MacOS/draw.io")
# Default Windows path
if(!file.exists(DRAW_IO_EXE)){
  DRAW_IO_EXE <- "C:/Program Files/draw.io/draw.io.exe"
}

# If not found, then try to search for that in Windows
if(!file.exists(DRAW_IO_EXE)){
  DRAW_IO_EXE <- search_drawio()
}
if(length(DRAW_IO_EXE)==0){
  stop("Please configure first the draw.io path")
}

# Figures will be generated in the current path
PATH_OUTPUTS <- "./GRAFS_test-outputs/"
create_GRAFS(XLSX_INPUTS,PATH_OUTPUTS,
             XML_BASE,TABLA_ID_XML,
             DRAW_IO_EXE,
             REGIONS = "spain",
             PERIODS = c("2011:2015-1990:1994")
             )

}
