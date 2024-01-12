.map = function (metric = "unknown", long_data = NULL, wide_data = NULL, min_value = NULL,
                 verbose = 0, map_output = NULL, palette = brewer.pal(8, "OrRd"), 
                 run.types = NULL, runs_per_ctrl_type = NULL) 
{
    
    setwd(map_output)
    if (verbose > 0) {print("Creating maps...")}
    
    Median_RepType=tapply(long_data$Value,long_data$type,FUN=median)
    Upper_RepType<-tapply(long_data$Value,long_data$type,FUN=quantile,probs=0.975, na.rm = TRUE)
    
    ## Create individual data frames for each run type. Add medians and upper 2.5% quartiles, then split into 
    # two-column dataframes with fips for mapping
    
    Medians=NULL
    Uppers=NULL
    
    median_uppers_long <- long_data %>%
      group_by(fips, type) %>%
      summarise(median = as.numeric(median(Value)), upper = as.numeric(quantile(Value, probs = 0.975, na.rm = TRUE)))
    
    for (i in 1:length(run.types)){

      newname <- run.types[i]
      df.new <- median_uppers_long %>%
        filter(str_detect(type, newname)) %>%
        ungroup()

      # Create the vectors for map scales
      Medians=c(Medians,df.new$median)
      Uppers=c(Uppers,df.new$upper)
      
      # save datasets for mapping, one for each level with medians and uppers with name of control type
      assign(paste0("Med_",newname), df.new[,grepl( "fips|median" , names( df.new ) )])
      assign(paste0("Upper_",newname), df.new[,grepl( "fips|upper" , names( df.new ) )])
      
      rm(df, df1,df.new,newname)
    }
    
    ## Median maps
    
    # Create a vector for scale for median maps
    Median_values <- round(c(0, min_value, mean(Medians, na.rm = TRUE), 
                                 mean(Medians, na.rm = TRUE) + sd(Medians, na.rm = TRUE), 
                                 mean(Medians, na.rm = TRUE) + 3*sd(Medians, na.rm = TRUE), 
                                 mean(Medians, na.rm = TRUE) + 6*sd(Medians, na.rm = TRUE), 
                                 max(Medians, na.rm = TRUE)), 0)
    Median_values<-unique(sort(Median_values));Median_values
    if (length(Median_values) ==2){Median_values <- c(0, max(Median_values)-1, max(Median_values) + 1)}
    
    for (i in 1:length(run.types)){
      name_df=get(paste0("Med_",run.types[i])) 
      if (!all(name_df$upper == 0)){
        name_df <- na.omit(name_df)
        if (!all(name_df$median == 0)){
          jpeg(paste0(paste0(metric,"_Median_Map_"),run.types[i],".jpeg"), width = 760, height = 520, units = 'px', res = 100)
          map_by_fips(name_df, county.border.col = NA, state.border.col = "gray30", 
                      missing.include = TRUE, color.break.type = "values", 
                      color.break.values = Median_values, color.sequence = palette, 
                      legend.spacing = 4.5, legend.shrink = 0.3, legend.width = 1)
          dev.off()
        }
      }
    }
    
    ## Upper 2.5% maps
    
    # Create a vector for scale for median maps
    Upper_values <- round(c(0, min_value, mean(Uppers, na.rm = TRUE), 
                                mean(Uppers, na.rm = TRUE) + sd(Uppers, na.rm = TRUE), 
                                mean(Uppers, na.rm = TRUE) + 3*sd(Uppers, na.rm = TRUE), 
                                mean(Uppers, na.rm = TRUE) + 6*sd(Uppers, na.rm = TRUE), 
                                max(Uppers, na.rm = TRUE)), 0)
    if(6*sd(Uppers, na.rm = TRUE)> max(Uppers, na.rm = TRUE)) Upper_values<-Upper_values[c(1:5,7)]
    Upper_values<-unique(sort(Upper_values));Upper_values
    
    
    for (i in 1:length(run.types)){
      name_df=get(paste0("Upper_",run.types[i])) 
      
      if (!all(name_df$upper == 0)){
        jpeg(paste0(paste0(metric,"_Upper_Map_"),run.types[i],".jpeg"), width = 1800, height = 900, units = 'px', res = 100)
        map_by_fips(name_df, county.border.col = NA, state.border.col = "gray30", 
                    missing.include = TRUE, color.break.type = "values", 
                    color.break.values = Upper_values, color.sequence = palette, 
                    legend.spacing = 4.5, legend.shrink = 0.3, legend.width = 1)
        dev.off()
      }
    }
  
  if (verbose > 0) {print("Mapping complete")}
}
