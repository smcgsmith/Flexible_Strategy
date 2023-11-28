# List of required packages
required_packages <- c(
  "stats", "maps", "mapdata", "tidyr", "fields", "foreach", "dplyr", 
  "RColorBrewer", "rgdal", "reshape2", "reshape", "data.table", 
  "knitr", "ggplot2", "ggmap", "kableExtra", "magrittr", "tidyverse",
  "ggfortify", "officer", "rvg", "cowplot", "unikn", "epiR", "broom",
  "gplots", "stargazer", "sjPlot", "GGally"
)

# Function to check and install packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Check and install missing packages
install_missing_packages(required_packages)

# Load the packages
lapply(required_packages, library, character.only = TRUE)

#Colors
cbPalette <- usecol(pal = c(rev(pal_seeblau), "steelblue", pal_pinky), n = 12, alpha = 1)   # define color palette from 3 colors
# Sensitivity plot bar colors
blue<-c('#2171b5')
# Sensitivity plot labels (PRCC)
colors.short = c("black","black",
                 cbPalette[12],cbPalette[12],cbPalette[12],
                 cbPalette[12],cbPalette[12],cbPalette[12],
                 "black","black","black","black")

# Sensitivity plot labels (models)
colors.long = c("black","black","black",cbPalette[12],
           cbPalette[12],cbPalette[12],cbPalette[12],
           cbPalette[12],cbPalette[12],cbPalette[12], 
           cbPalette[12],cbPalette[12],cbPalette[12],
           cbPalette[12],cbPalette[12],
           "black","black","black")