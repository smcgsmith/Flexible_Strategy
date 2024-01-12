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
             custom_labels = F,
             usdos_output_file_path = "/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/flexibleStrategy/Post_Processing/Files_To_Process/"
             )

save(file = "postProcessing_USDOSv2.1.RData")
