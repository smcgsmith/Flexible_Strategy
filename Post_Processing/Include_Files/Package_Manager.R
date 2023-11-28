# List of required packages
required_packages <- c(
  "stats", "maps", "mapdata", "tidyr", "fields", "foreach", 
  "dplyr", "RColorBrewer", "rgdal", "reshape2", "reshape", 
  "data.table", "knitr", "ggplot2", "ggmap", "kableExtra", 
  "magrittr", "tidyverse", "data.table", "ggfortify", "officer",
  "rvg", "ggfortify", "cowplot", "unikn", "data.table",
  "epiR", "ggplot2", "tidyverse", "RColorBrewer", "broom",
  "foreach", "doParallel", "gplots", "stargazer", "sjPlot",
  "GGally", "tidyr", "ggpubr", "broom"
)

# Install missing packages
install.packages(setdiff(required_packages, installed.packages()[,"Package"]), dependencies = TRUE)

# Load the required libraries
purrr::map(required_packages, library, character.only = TRUE)

# #For importing and processing full runs
# library(stats); library(maps);  library(mapdata); library(tidyr); library(fields); library(foreach);
# library(dplyr); library(RColorBrewer); library(rgdal); library(reshape2); library(reshape); library(data.table); library(knitr);
# library(ggplot2); library(ggmap); library(kableExtra); library(magrittr);
# library(tidyverse);library(data.table); library(ggfortify);library(officer);
# library(rvg); require(ggfortify); library(cowplot);library(unikn);library(data.table)
# 
# #For importing and processing sensitivity runs
# library(epiR);library(ggplot2);library(tidyverse);library(RColorBrewer);library(broom);library(foreach);library(doParallel)
# library(gplots);library(stargazer);library(sjPlot);library(GGally);library(tidyr);library(ggpubr);library(broom)

#set default parameters
#Setting defaults
# export.datafiles= 3
# results.report = FALSE
# summaryTable = TRUE
# duration = TRUE
# Dur_min = 13
# Dur_cutoff = 150
# premInf = TRUE
# PremInf_min = 10
# PremInf_cutoff = 5000
# premReport = TRUE
# ReportedPrems_min = 5
# ReportedPrems_cutoff = 100
# epidemicExtent = TRUE
# EpidExt_min= 1
# EpidExt_cutoff=200
# movementBan = TRUE
# MB_min = 0
# MB_cutoff = 1
# premisesCulled = TRUE
# PremCull_min = 0
# PremCull_cutoff = 10
# premisesVax = TRUE
# PremVax_min = 0
# PremVax_cutoff = 2
# diagnosticTests = TRUE
# DiagTest_min = 25
# DiagTest_cutoff = 1000
# animalsInfected = TRUE
# Anim_min = 10
# Anim_cutoff = 1000
# countyRisk = TRUE
# CountyRisk_min = 0
# CountyRisk_cutoff = 0.001
# localSpread = TRUE
# plot_color = "color_blue"
# map_color = "color_red"
# ls_match = TRUE
# controlValue = TRUE
# control_min = 4000
# control_cutoff = 100000
# dataExist = TRUE
