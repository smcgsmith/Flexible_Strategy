setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("postProcessing_USDOSv2.1.R")

processUSDOS(export.datafiles = 3,
             summaryTable = F,
             duration = F,
             premInf = F,
             premReport = F,
             epidemicExtent = F,
             diagnosticTests = F,
             animalsInfected = F,
             localSpread = F,
             premisesCulled = F,
             premisesVax = F,
             controlValue = T,
             animalsControlled = F,
             plots = F,
             maps = F,
             dataExist = T,
             custom_labels = F)

save(file = "postProcessing_USDOSv2.1.RData")
