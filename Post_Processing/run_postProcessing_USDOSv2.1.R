setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("postProcessing_USDOSv2.1.R")

processUSDOS(export.datafiles = 3,
             summaryTable = F,
             duration = T,
             premInf = T,
             premReport = T,
             epidemicExtent = T,
             premisesCulled = F,
             premisesVax = F,
             diagnosticTests = F,
             animalsInfected = F,
             countyRisk = F,
             localSpread = F,
             plot_color = "color_red",
             map_color = "color_red",
             ls_match = TRUE,
             controlValue = F,
             animalsControlled = F,
             plots = T,
             maps = FALSE,
             verbose = 1,
             dataExist = F)

save(file = "postProcessing_USDOSv2.1.RData")
