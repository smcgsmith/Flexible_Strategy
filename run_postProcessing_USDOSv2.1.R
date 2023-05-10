setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("postProcessing_USDOSv2.1.R")

processUSDOS(export.datafiles = 3,
             summaryTable = F,
             duration = T,
             premInf = T,
             premReport = T,
             epidemicExtent = T,
             movementBan = F,
             premisesCulled = F,
             premisesVax = F,
             diagnosticTests = F,
             animalsInfected = F,
             countyRisk = F,
             localSpread = F,
             color_palette = "color_red",
             ls_match = TRUE,
             controlValue = TRUE,
             animalsControlled = TRUE,
             maps = TRUE)

save(file = "postProcessing_USDOSv2.1.RData")
