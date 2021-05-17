########################################################################
# relevant libraries
#
# Create MH::03/02/2021
########################################################################

#
# rm(list = ls())

###############################
# libraries
###############################
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.rstudio.com/")
  }
  sapply(pkg, require, character.only = TRUE)
}
rel_packages <- c(
  "tidyverse",
  "readxl",
  "bookdown",
  "tufte",
  "leaflet",
  "leaflet.extras",
  "sf",
  "cowplot",
  "gganimate",
  "ggrepel",
  "ggimage",
  "ggpubr",
  "grDevices",
  "treemapify",
  "showtext",
  "extrafont",
  "knitr",
  "kableExtra",
  "gifski",
  "magick",
  "htmltools",
  "htmlwidgets",
  "devtools",
  "svglite",
  "pander",
  "fontawesome",
  "haven",
  "webshot2",
  "echarts4r"
)

ipak(rel_packages)

if (!require(waffle)) install.packages("waffle", repos = "https://cinc.rud.is")
#devtools::install_github("r-spatial/lwgeom")
#devtools::install_github("grimbough/msmbstyle")

###############################
###############################
