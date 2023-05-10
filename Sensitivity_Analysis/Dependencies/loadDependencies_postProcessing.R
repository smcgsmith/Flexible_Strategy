#For importing and processing full runs
library(stats); library(maps);  library(mapdata); library(tidyr); library(fields); library(foreach);
library(dplyr); library(RColorBrewer); library(rgdal); library(reshape2); library(reshape); library(data.table); library(knitr);
library(ggplot2); library(ggmap); library(kableExtra); library(magrittr);
library(tidyverse);library(data.table); library(ggfortify);library(officer);
library(rvg); require(ggfortify); library(cowplot);library(unikn);library(data.table)

#For importing and processing sensitivity runs
library(epiR);library(ggplot2);library(tidyverse);library(RColorBrewer);library(broom)
library(gplots);library(stargazer);library(sjPlot);library(GGally);library(tidyr)

#Colors
#old post-processing color pallette - could rename new one to keep cbPalette original colors
#cbPalette <- c("#D55E00", "#CC79A7", "#56B4E9", "#009E73", "#0072B2", "#000000", "#F0E442", "#E69F00", "#E495A5") # Violin plots
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

# Sensitivity plot labels (models)
# colors.long.full = c("black","black","black",cbPalette[12],
#                 cbPalette[12],cbPalette[12],cbPalette[12],
#                 cbPalette[12],cbPalette[12],cbPalette[12], 
#                 cbPalette[12],cbPalette[12],cbPalette[12],
#                 cbPalette[12],cbPalette[12], cbPalette[12],
#                 cbPalette[12],cbPalette[12], cbPalette[12],
#                 "black","black","black")
