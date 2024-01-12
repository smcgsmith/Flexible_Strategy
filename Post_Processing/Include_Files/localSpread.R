.localSpread = function (pathfiles = NULL, path0 = NULL, map_output = NULL, 
                         detail.fnames = NULL, export.datafiles = 3, data_output = NULL, 
                         run.types = NULL, runs_per_ctrl_type = NULL) {
  print("Entering local spread function")
  # Extract the infection route from the detail file
  setwd(pathfiles)
  TypeSpread.long <- list()

  # Set the number of cores for parallel processing
  num_cores <- detectCores() -2  # Adjust the number of cores as needed
  
  # Register the parallel backend
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  iterations = length(detail.fnames)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # List to store results
  TypeSpread.long <- foreach(file = 1:length(detail.fnames), .packages = c("data.table","dplyr"), .combine = rbind, .options.snow = opts) %dopar% {

    det.file <- fread(detail.fnames[file], select = c("SourceCounty", "InfRoute", "ControlPrevented"))
    
    detail.res <- det.file[ControlPrevented == "none", ][, .N, by = .(SourceCounty, InfRoute)] %>% 
      setnames(c("SourceCounty", "Local", "Ship")) %>% 
      replace(is.na(.), 0) %>% 
      mutate(type = unlist(strsplit(detail.fnames[file], "_FLAPS"))[1])
    
    return(detail.res)
  }
  
  # Stop the parallel backend
  stopCluster(cl)
  
  # Clean up
  gc()
  
  # Convert the result to a data.frame
  TypeSpread.long <- as.data.frame(TypeSpread.long)
  
  setwd(data_output)
  if (export.datafiles == 1 | export.datafiles == 3) {write.csv(TypeSpread.long, "TypeSpread.long.csv")}

  ## Maps of the proportion of spread events that are local spread. 
  # Create a vector for scale
  TypeSpread.summary <- TypeSpread.long %>%
    group_by(type,SourceCounty) %>% 
    summarise(local = sum(Local), ship = sum(Ship)) %>%
    mutate(propLocal = round((local/(local + ship))*100,0)) %>%
    ungroup()
  
  local_scale <- round(c(0, 25, 50, 60, 70, 80, 85, 90, 95, 97.5, 99, 100),2)
  
  # loop over run types
  print("Generating local spread maps.")
  setwd(map_output)
  for (i in 1:length(unique(TypeSpread.summary$type))){
    name_df=TypeSpread.summary %>% filter(type == unique(TypeSpread.summary$type)[i]) %>% select(SourceCounty, propLocal)
    if(all(colnames(name_df) == c("SourceCounty", "propLocal"))){
      jpeg(paste0(map_output, paste0("PropLocal_",unique(TypeSpread.summary$type)[i],".jpeg"), sep=""), width = 1800, height = 900, units = 'px', res = 100)
      map_by_fips(name_df, county.border.col = NA, state.border.col = "gray30",
                  missing.include = TRUE, color.break.type = "values", legend.digits = 1,
                  color.break.values = local_scale, 
                  color.sequence = if(ls_match == TRUE) {palette} else {color_bluepurple}, 
                  legend.spacing = 5.5, legend.shrink = 0.6, legend.width = 1)
      dev.off()
    } else {
      stop(".localSpread(): dataframe is not formatted correctly. Mapping aborted. ")
    }
  }
}
