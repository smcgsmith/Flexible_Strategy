setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("postProcessing_USDOSv2.1.R")

processUSDOS(export.datafiles = 3,
             duration = T,
             premInf = T,
             epidemicExtent = T,
             localSpread = T,
             controlValue = T,
             plots = T,
             maps = T,
             dataExist = T)

save(file = "postProcessing_USDOSv2.1.RData")
