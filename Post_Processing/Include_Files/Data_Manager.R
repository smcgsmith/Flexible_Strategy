#' Import and process USDOS summary files in parallel.
#'
#' This function imports USDOS summary files, processes them in parallel, and merges the results with an existing data frame.
#'
#' @param flexibleControl Logical. If TRUE, adds a flexible control identifier to the "Type" column.
#' @param summary.files Vector of file names for USDOS summary files.
#' @param pathfiles Path to the directory containing summary files.
#' @param export.datafiles Export option for processed data files (1: CSV, 2: Excel, 3: both).
#' @param path_output Path to the directory for output files.
#'
#' @return A processed data frame containing merged USDOS summary file data.
#'
#' @details The function processes each summary file, extracts relevant information, and merges it with an existing data frame.
#' It utilizes parallel processing to speed up the import and processing of summary files.
#'
#' @export
.import_summaryFiles_parallel <- function(flexibleControl = FALSE, summary.files = NULL, 
                                          pathfiles = NULL, export.datafiles = 1, path_output = NULL) {
  
  print("Importing USDOS summary files.")
  
  # # Detect if there were any flexible control strategy runs.
  flexibleControl = any(str_detect(summary.files, "percentIncrease")) | any(str_detect(summary.files, "availability")) | any(str_detect(summary.files, "decrease"))
  
  if (flexibleControl) {
    print("Flexible control runs detected. Setting flexibleControl == TRUE")
  }
  
  run.types = NULL
  types_to_remove = NULL
  # Create a function for processing a single summary file
  process_summary_file <- function(file,pathfiles,flexibleControl) {
    summary.file.path <- paste0(pathfiles, file)
    summary.res <- fread(summary.file.path, header = TRUE)
    summary.res <- as.data.frame(summary.res)
    summary.res$type <- unlist(strsplit(file, "_FLAPS"))[1]
    summary.res$type <- unlist(strsplit(summary.res$type, "FMD_PTon_Infectious20days_"))[2]
    
    if (dim(summary.res)[1] == 3049) {
      if(colnames(summary.res)[1]=="Rep") {
        summary.res <- summary.res[,-grep("RunTime", colnames(summary.res))] 
        names(summary.res)[names(summary.res) == 'Seed_FIPS'] <- "fips"
        
        if (flexibleControl){ #this helps sort later
          if(grepl("percentIncrease|availability|decrease", summary.res$type[1])){
            summary.res$type = paste("flex",summary.res$type, sep = "_" ) ##add a flexible control identifier if
          } else if (grepl("newPremReportsOverX|newRegionReportsOverX", summary.res$type[1])) {
            summary.res$type = paste("static",summary.res$type, sep = "_" ) ##add a flexible control identifier if - this assume flex is only run with decrease, percentIncrease, or availability (may not be true in all cases)
          }
        }
        
        type = unique(summary.res$type)
        
        # Return the processed summary file
        return(list(summary.res,type))
      } else {
        cat("Check header: ", file, "\n")
        stop("Cannot import summary files without a header")
      } 
    } else {
      type = unique(summary.res$type)
      warning("Summary file not the correct dimensions", immediate. = TRUE)
      # stop("Summary file not the correct dimensions")
      return(list(file = NULL, type))
    }
  }
  
  
  num_cores <- detectCores() - 2
  # Register the parallel backend
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  iterations = length(summary.files)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # Use foreach to parallelize the processing of summary files
  summary_file_type_list <- foreach(i = 1:length(summary.files), .packages = c("data.table","stringr"), .options.snow = opts, .errorhandling = "stop") %dopar% {
    file_and_type <- process_summary_file(file = summary.files[i], pathfiles = pathfiles, flexibleControl = flexibleControl)
    # file <- file_and_type[[1]]
    # type <- file_and_type[[2]]
    
    return(file_and_type)
  }
  
  # Use lapply to extract data frames and character vectors
  summary_file_list <- lapply(summary_file_type_list, `[[`, 1)
  # Remove "fips" column from all data frames except the first one so that we merge with county.summary
  summary_file_list <- lapply(seq_along(summary_file_list), function(i) {
    if (i == 1) {
      # For the first data frame, keep all columns
      return(summary_file_list[[i]])
    } else {
      # For other data frames, remove the "type" column
      summary_file_list[[i]][, !(names(summary_file_list[[i]]) %in% "fips"), drop = FALSE]
    }
  })
  run.types <- unique(unlist(lapply(summary_file_type_list, `[[`, 2)))

  # Filter out NULL values
  summary_file_list <- summary_file_list[!sapply(summary_file_list, is.null)]
  
  #cat("Run types:",run.types, "\n")
  summary_file_df <- do.call(cbind, summary_file_list)
  
  county.summary <- merge(county.summary, summary_file_df, by = "fips", all = TRUE)
  
  empty_data_frame <- NULL
  # Use Filter to subset summary_file_type_list
  empty_data_frames <- Filter(function(x) is.null(x[[1]]), summary_file_type_list)
  types_to_remove <- unique(unlist(lapply(empty_data_frames, `[[`, 2)))
  
  # subset county.summary df if there were incomplete or "weird" summary files imported
  if (length(types_to_remove) > 0) {

    cat("Removing runs of type(s): ",types_to_remove,"\n because they had either unreadable headers or incorrect dimensions. Check summary files. \n")

    # Find columns with type. prefix
    type_columns <- grep("^type\\.", names(county.summary), value = TRUE)
    # Identify columns with values to remove
    columns_to_remove <- sapply(type_columns, function(col) any(county.summary[[col]] %in% types_to_remove))
    columns_to_remove <- which(columns_to_remove)
    # Extract the suffixes from the columns to be removed
    suffixes_to_remove <- gsub("^type\\.", "", type_columns[columns_to_remove])

    # Identify and remove columns with the same suffix
    columns_to_remove <- grep(paste(suffixes_to_remove, collapse = "\\b|"), names(county.summary))

    # Remove identified columns
    county.summary <- county.summary[, -columns_to_remove]
    # Calculate runs_per_ctrl_type before remove types
    runs_per_ctrl_type <- length(summary.files) / length(unique(run.types))
    run.types <- run.types[-which(run.types %in% types_to_remove)]
    
  } else {
    # Calculate runs_per_ctrl_type
    runs_per_ctrl_type <- length(summary.files) / length(unique(run.types))
  }
  
  # remove runs from run.type vector
  #cat("Runs per ctrl type:",runs_per_ctrl_type, "\n")
  # Assign variables to the global environment for future use. These won't conflict with anything
  assign("runs_per_ctrl_type", runs_per_ctrl_type, envir = .GlobalEnv)
  assign("run.types", unique(run.types), envir = .GlobalEnv)
  assign("types_to_remove", types_to_remove, .GlobalEnv)
  
  setwd(path_output)
  if (export.datafiles == 1 | export.datafiles == 3) {
    write.csv(county.summary, file = "CountySummary.csv")
  }
  
  print("Completed importing USDOS summary files.")
  #assign("county.summary", county.summary, envir = .GlobalEnv)
  return(county.summary)
  
  stopCluster(cl)
}

#' Import USDOS summary files
#'
#' This function imports USDOS summary files and returns a merged data frame. The function can handle flexible control scenarios, where multiple summary files are imported and merged based on the county FIPS codes. The function also calculates the number of runs per control type and exports the merged data frame to a CSV file in the specified output path, if required.
#'
#' @param flexibleControl logical indicating whether flexible control scenarios are used (default = FALSE).
#' @param summary.files a vector of summary file names to import.
#' @param pathfiles a character string indicating the directory path where the summary files are stored.
#' @param export.datafiles an integer specifying whether to export data files (default = 1). 0 means no export, 1 means export the merged data frame, and 3 means export the merged data frame and the runs per control type.
#' @param path_output a character string indicating the directory path where the merged data frame and runs per control type file are exported (default = NULL).
#'
#' @return A data frame containing the merged summary information.
#'
#' @details The imported summary files are assumed to have a specific file name format, containing the county FIPS code and the control type. The function removes unnecessary columns from the summary files and renames the FIPS column to "fips" for merging purposes. It also adds a flexible control identifier if the summary file belongs to a flexible control scenario. Finally, the function calculates the number of runs per control type and exports the merged data frame to a CSV file in the specified output path, if required.
#'
#' @export
#' 
.import_summaryFiles = function(flexibleControl = FALSE, summary.files = NULL, 
                                pathfiles = NULL, export.datafiles = 1, path_output = NULL) {
  
  print("Importing USDOS summary files.")
  
  run.types = NULL
  
  if(flexibleControl == TRUE){
    for(i in 1:length(summary.files)){
      summary.file <- summary.files[i]
      summary.file.path <- paste0(pathfiles, summary.file)
      summary.res <- fread(summary.file.path, header = TRUE)
      
      if(colnames(summary.res)[1]!="Rep") {
          cat("Check header: ", summary.file, "\n")
          stop("Cannot import summary files without a header")
      }
      
      # colnames(summary.res) <- c("Rep","Num_Inf","nAffCounties","Duration","Seed_Farms","Seed_FIPS","RunTimeSec","Num_Reports","shipBanImplemented","shipBanEffective","cullImplemented","cullEffective","vaxImplemented","vaxEffective","vaxImplementedDCSubset","vaxImplementedDCSubset","meanDCsPerRP")
      summary.res <- as.data.frame(summary.res)
      summary.res2 <- summary.res[,-grep( "RunTime" , colnames( summary.res ))] # KO: Also remove Rep,Seed_Farms if not needed?
      summary.res2$type <- unlist(strsplit(summary.file, "_FLAPS"))[1]
      
      #Lauren changed name of fips column in res2 because diff num rows
      names(summary.res2)[names(summary.res2) == 'Seed_FIPS'] <- "fips"
      
      summary.res2$type <- unlist(strsplit(summary.res2$type, "FMD_PTon_Infectious20days_"))[2]
      summary.res2$type <- unlist(strsplit(summary.res2$type, "_MvmtBan"))[1]

        if((FALSE %in% (unlist(str_extract_all(summary.res2$type[1], "[[:digit:]]+")) %in% c(0,1,10000,3000)))){
          summary.res2$type = paste("flex",summary.res2$type, sep = "_" ) ##add a flexible control identifier if
        } else if ("newPremReportsOverX" %in% unlist(strsplit(summary.res2$type[1], "_"))) {
          summary.res2$type = paste("static",summary.res2$type, sep = "_" ) ##add a flexible control identifier if - this assume flex is only run with decrease, percentIncrease, or availability (may not be true in all cases)
        } else{
          summary.res2$type = paste("static",summary.res2$type, sep = "_" ) ##add a flexible control identifier if
        }

      
      if(any(is.na(summary.res2$type))){
        print(head(summary.res2))
      }
      
      gc()
      run.types = c(run.types, unique(summary.res2$type))
      county.summary <- merge(county.summary, summary.res2, by ="fips", all=TRUE)
    }
    
  }else{
    
    for(i in 1:length(summary.files)){
      summary.file <- summary.files[i]
      summary.file.path <- paste(pathfiles, summary.file, sep = "")
      summary.res <- fread(summary.file.path, header = TRUE)
      summary.res <- as.data.frame(summary.res)
      summary.res2 <- summary.res[,-grep( "RunTime" , colnames( summary.res ))] # KO: Also remove Rep,Seed_Farms if not needed?
      summary.res2$type <- unlist(strsplit(summary.file, "_FLAPS"))[1]
      run.types=c(run.types,summary.res2$type[1])
      
      #Lauren changed name of fips column in res2 because diff num rows
      names(summary.res2)[names(summary.res2) == 'Seed_FIPS'] <- "fips"
      county.summary <- merge(county.summary, summary.res2, by ="fips", all=TRUE)
    }
    
  }
  
  ## To check for <100 runs per type and account for possibility that users run >100 iterations per county, calculate how many they ran. 
  runs_per_ctrl_type <- length(summary.files)/length(unique(run.types))
  run.types <- unique(run.types);run.types
  
  #assign them to the gloabl environment so that post-processing can use them without having to return them. Should
  # not create issues with other variables SMS
  assign("runs_per_ctrl_type", runs_per_ctrl_type, envir = .GlobalEnv)
  assign("run.types", run.types, envir = .GlobalEnv)
  
  setwd(path_output)
  if(export.datafiles == 1 | export.datafiles ==  3){write.csv(county.summary, file = paste0(paste0("CountySummary_",Sys.Date()),".csv"))}

  print("Completed importing USDOS summary files.")
  #assign("county.summary", county.summary, envir = .GlobalEnv)
  return(county.summary)
}

#' Import control summary files for cost calculations
#'
#' This function imports USDOS control output files for control cost calculations.
#'
#' @param summary.files a character vector of file names containing the summary files
#' @param pathfiles a character string indicating the path to the summary files
#' @param export.datafiles a numeric value indicating whether to export data files (1), not to export data files (0), or only to export graphs (2)
#' @param path_output a character string indicating the path to output the data files
#'
#' @return a data frame with information on animal counts and control status
#'
#' @export
#'
.import_controlSummaryFiles_parallel = function(control_file_names = NULL, summary_file_names = NULL, detail_file_names = NULL, dependencies = NULL,
                                                pathfiles = NULL, flaps_file_list, export.datafiles = 1, path_output = NULL, no_control_files = NULL) {
  
  print("Importing USDOS control output files for control cost calculations.")
  cat(length(control_file_names), "files to process.")

  # Define a function for processing a single control summary file
  process_control_summary_file <- function(file_idx, pathfiles, control_file_names, dependencies,
                                           summary_file_names, detail_file_names, flaps_file_list) {
    
    #Need to .extract_runDetailsFromType() function from Data_Manager.R
    source(paste0(dependencies,"Data_Manager.R"))
    
    setwd(pathfiles)
    
    #get file paths and then find the correct files
    control_run_type <- sub("(FLAPS12_Quarterly_USDOS_format_\\d{4}_\\d{2}).*", "\\1", control_file_names[file_idx])
    summary_run_types <- sub("(FLAPS12_Quarterly_USDOS_format_\\d{4}_\\d{2}).*", "\\1", summary_file_names)
    detail_run_types <- sub("(FLAPS12_Quarterly_USDOS_format_\\d{4}_\\d{2}).*", "\\1", detail_file_names)
    
    control_file_path <- paste0(pathfiles, control_file_names[file_idx])
    summary_file_path <- paste0(pathfiles, summary_file_names[which(summary_run_types == control_run_type)])
    detail_file_path <- paste0(pathfiles, detail_file_names[which(detail_run_types == control_run_type)])
    
    # Retrieve files. Use grep to find all cases where control was effective. Can't use sql type syntax with header = TRUE, etc
    control_file <- fread(cmd = paste('grep', 'effective', control_file_path))
    column_names <- c("Rep", "tStep", "farmID", "FIPS", "controlStatus")
    colnames(control_file) <- column_names
    
    summary_file <- fread(summary_file_path, header = TRUE, select = c("Rep", "Seed_Farms", "Seed_FIPS", "Duration", "cullEffective", "vaxEffective", "Num_Inf"))
    
    # tic <- Sys.time()
    # detail_file <- fread(detail_file_path, header = TRUE,
    #                      select = c("Rep", "ExposedID", "ExposedCounty", "SourceID", "SourceCounty", "ControlPrevented"))
    # toc <- Sys.time()
    # 
    # toc-tic
    #
    tic <- Sys.time()
    detail_file <- fread(cmd = paste('grep', 'none', detail_file_path))[,c(1,2,4,6,7,8)]
    colnames(detail_file) <- c("Rep", "ExposedID", "SourceID", "ControlPrevented", "ExposedCounty", "SourceCounty")
    toc <- Sys.time()

    toc-tic
    
    # Retrieve the correct flaps file from the flaps_file_list
    flaps_num <- substr(unlist(strsplit(summary_file_names[file_idx], "format_"))[2], 1, 4)
    flaps_file <- flaps_file_list[[as.numeric(flaps_num)]]
    
    # Merge summary and flaps files
    summary_file <- merge(summary_file, flaps_file[, c("Id", "anim")], by.x = "Seed_Farms", by.y = "Id", all.x = TRUE)
    rep_inf_totals <- merge(detail_file, flaps_file[, c("Id", "anim")], by.x = "ExposedID", by.y = "Id", all.x = TRUE) %>%
      dplyr::rename(Num_Anim_Inf = anim)
    
    # Filter through detail files
    #detail_file <- detail_file[detail_file$ControlPrevented == "none", ]
    # detail_file <- unique(as.data.table(detail_file), by = c("Rep", "ExposedID")) 
    # detail_file <- detail_file %>%
    #   mutate(Rep = as.integer(Rep),
    #          ExposedID = as.integer(ExposedID),
    #          ExposedCounty = as.integer(ExposedCounty),
    #          SourceID = as.integer(SourceID),
    #          SourceCounty = as.integer(SourceCounty))
    
    # Merge control file with flaps file to get number of animals on the controlled farm
    rep_control_totals <- merge(flaps_file[, c("Id", "County_fips", "anim")], 
                        control_file[, c("Rep", "farmID", "FIPS", "controlStatus")],
                        by.x = c("Id", "County_fips"), by.y = c("farmID", "FIPS"), all.y = TRUE) %>%
      dplyr::rename(ExposedID = Id, Num_Anim_Controlled = anim)
    
    filtered_rep_inf_totals <- rep_inf_totals %>%
      anti_join(rep_control_totals, by = c("Rep", "ExposedID"))
    
    rep_totals <- rep_control_totals %>%
      group_by(Rep, controlStatus, .drop = FALSE) %>%
      summarise(anim = sum(Num_Anim_Controlled)) %>%
      ungroup()
    
    rep_inf_totals <- filtered_rep_inf_totals %>%
      group_by(Rep, .drop = FALSE) %>%
      summarise(anim = sum(Num_Anim_Inf)) %>%
      ungroup()
    
    summary_file$Num_Inf_No_Control <- rep(0,dim(summary_file)[1])
    # If a Rep's cullEff or vaxEff >1, get the additional info ab that rep from the control summary file
    # Much faster to use a looop in this case for some reason.
    for(row in 1:max(summary_file$Rep)){
      if(summary_file$cullEffective[row]==0 & summary_file$vaxEffective[row]==0){
        summary_file$cullEffective[row]= 0
        summary_file$vaxEffective[row]= 0
        # print("true: none")
      } else if (summary_file$cullEffective[row]>0 & summary_file$vaxEffective[row]==0) {
        # print("true: cull only")
        summary_file$vaxEffective[row]= 0
        summary_file$cullEffective[row]=rep_totals$anim[which(rep_totals$Rep==summary_file$Rep[row]  & rep_totals$controlStatus == "effective.cull")]
      } else if (summary_file$vaxEffective[row]>0 & summary_file$cullEffective[row]==0) {
        # print("true: vax only")
        summary_file$cullEffective[row]= 0
        summary_file$vaxEffective[row]=rep_totals$anim[which(rep_totals$Rep==summary_file$Rep[row] & rep_totals$controlStatus == "effective.vax")]
      } else {
        summary_file$vaxEffective[row]=rep_totals$anim[which(rep_totals$Rep==summary_file$Rep[row] & rep_totals$controlStatus == "effective.vax")]
        summary_file$cullEffective[row]=rep_totals$anim[which(rep_totals$Rep==summary_file$Rep[row]  & rep_totals$controlStatus == "effective.cull")]
        # print("BOTH")
      }
      if (summary_file$Rep[row] %in% rep_inf_totals$Rep){
        summary_file$Num_Inf_No_Control[row]=rep_inf_totals$anim[which(rep_inf_totals$Rep==summary_file$Rep[row])] 
      }
    }
    
    run_anim <- summary_file %>%
      group_by(Rep, Seed_FIPS, Seed_Farms) %>%
      arrange(Seed_FIPS) %>%
      dplyr::rename("Num_Anim_Inf" = anim)
    
    rm(rep_totals, control_file, summary_file)
    gc()
    
    # run_anim$type <- control_file_names[file_idx]
    # run_anim <- .extract_runDetailsFromType(run_anim)
    
    new_columns <- .extract_runDetailsFromTypeString(control_file_names[file_idx])
    run_anim$type <- new_columns$type
    run_anim$control_type <- new_columns$control_type
    run_anim$delay <- new_columns$delay
    
    gc()
    
    if (file_idx == (length(control_file_names) / 2)) {
      cat("Halfway through control summary files (", file_idx, ")\n")
    }
    
    return(run_anim)
  }
  
  num_cores <- detectCores() - 2
  # Register the parallel backend
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  iterations = length(control_file_names)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # Use foreach to parallelize the processing of control summary files
  Anim_list <- foreach(i = 1:length(control_file_names), .packages = c("data.table","stringr","dplyr"), .options.snow = opts) %dopar% {
    process_control_summary_file(file_idx = i, pathfiles = pathfiles, 
                                 control_file_names = control_file_names,
                                 dependencies = dependencies,
                                 summary_file_names = summary_file_names,
                                 detail_file_names = detail_file_names,
                                 flaps_file_list = flaps_file_list)
  }
  
  # Stop the parallel backend
  stopCluster(cl)
  
  Anim_long <- rbindlist(Anim_list, idcol = "id")
  
  # Use lapply to read and process each no_control file
  no_control_list <- lapply(no_control_files, function(file) {
    no_control_df <- fread(paste0(pathfiles, file), header = TRUE, select = c("Rep", "Seed_Farms", "Seed_FIPS", "Duration","Num_Inf"))
    no_control_df$cullEffective <- 0
    no_control_df$vaxEffective <- 0
    # no_control_df$type <- file
    # no_control_df <- .extract_runDetailsFromType(no_control_df) #custom
    
    new_columns <- .extract_runDetailsFromTypeString(file)
    no_control_df$type <- new_columns$type
    no_control_df$control_type <- new_columns$control_type
    no_control_df$delay <- new_columns$delay
    
    return(no_control_df)
  })
  
  no_control_df <- rbindlist(no_control_list)
  
  Anim_long <- Anim_long %>% bind_rows(no_control_df)
  
  setwd(path_output)
  if (export.datafiles == 1 | export.datafiles == 3) {write.csv(Anim_long,paste0("Dur_Cull_Vax_NumInf_",Sys.Date(),".csv"),row.names=F)}
  
  return(Anim_long)
  print("Importing USDOS control output complete.")
}

#' Import control summary files for cost calculations
#'
#' This function imports USDOS control output files for control cost calculations.
#'
#' @param summary.files a character vector of file names containing the summary files
#' @param pathfiles a character string indicating the path to the summary files
#' @param export.datafiles a numeric value indicating whether to export data files (1), not to export data files (0), or only to export graphs (2)
#' @param path_output a character string indicating the path to output the data files
#'
#' @return a data frame with information on animal counts and control status
#'
#' @export
#'
.import_controlSummaryFiles = function(control.summary.files = NULL, summary.files = NULL, detail.fnames = NULL, path0 = NULL,
                                       pathfiles = NULL, flaps_file_list = NULL, export.datafiles = 1, path_output = NULL, no_control = NULL) {
  
  print("Importing USDOS control output files for control cost calculations.")
  cat(length(control.summary.files), "files to process.")
  
  Anim.list <- list()
  # Read through each summary/detail file 
  for (file in 1:length(control.summary.files)){
    
    # split_names <- unlist(str_split(control.summary.files[file], pattern = "_"))
    file_name <- paste0(pathfiles, control.summary.files[file])
    
    control.file <- fread(cmd = paste('grep', 'effective', file_name))
    column_names <- c("Rep","tStep","farmID","FIPS","controlStatus")
    
    if (dim(control.file)[2] != length(column_names)){break} # break the loop if a file isn't the right dimensions
    
    colnames(control.file) <- column_names
    
    summary.file <- fread(paste0(pathfiles,summary.files[file]), header = TRUE, select = c("Rep", "Seed_Farms", "Seed_FIPS", "Duration", "cullEffective", "vaxEffective", "Num_Inf"))
    
    #============================================
    # Select the right FLAPS file
    # setwd(path0)
    # FLAP <- substr(unlist(strsplit(summary.files[1],"format_"))[2],1,4)  
    # flaps = get(paste0("f",as.numeric(FLAP)), envir = .GlobalEnv)
    
    # Retrieve the correct flaps file from the flaps_file_list
    flaps_num <- substr(unlist(strsplit(summary.files[file], "format_"))[2], 1, 4)
    flaps <- flaps_file_list[[as.numeric(flaps_num)]]

    # merge FLAPS with summary file 
    summary.file=merge(summary.file,flaps[,c("Id", "anim")],by.x="Seed_Farms",by.y="Id",all.x=TRUE)
    
    # Read detail file
    setwd(pathfiles)
    det.file <- fread(detail.fnames[file], header = TRUE, select = c("Rep", "ExposedID", "ExposedCounty", "SourceID","SourceCounty", "ControlPrevented"))

    # Filter on not prevented and unique ExposureID by Rep
    det.file <- det.file[det.file$ControlPrevented == "none",]
    det.file <- unique(as.data.table(det.file),by=c("Rep","ExposedID")) 
    det.file <- det.file %>%
      mutate(Rep = as.integer(Rep),
             ExposedID = as.integer(ExposedID),
             ExposedCounty = as.integer(ExposedCounty),
             SourceID = as.integer(SourceID),
             SourceCounty = as.integer(SourceCounty))
    
    # Summarize the number of animals infected per Rep
    rep.totals <- merge(flaps[,c("Id", "County_fips", "anim")], control.file[,c("Rep","farmID", "FIPS", "controlStatus")],
                        by.x = c("Id", "County_fips"), by.y = c("farmID", "FIPS"), all = TRUE)
    
    # numReps <- 1:3049
    rep.totals <- rep.totals %>%
      group_by(Rep, controlStatus, .drop=FALSE) %>%
      summarise(anim = sum(anim)) %>%
      ungroup()
    
    # If a Rep's cullEff or vaxEff >1, get the additional info ab that rep from the control summary file
    # Loop is faster than using lapply() here
    for(row in 1:max(summary.file$Rep)){
      if(summary.file$cullEffective[row]==0 & summary.file$vaxEffective[row]==0){
        summary.file$cullEffective[row]= 0
        summary.file$vaxEffective[row]= 0
        # print("true: none")
      } else if (summary.file$cullEffective[row]>0 & summary.file$vaxEffective[row]==0) {
        # print("true: cull only")
        summary.file$vaxEffective[row]= 0
        summary.file$cullEffective[row]=rep.totals$anim[which(rep.totals$Rep==summary.file$Rep[row]  & rep.totals$controlStatus == "effective.cull")]
      } else if (summary.file$vaxEffective[row]>0 & summary.file$cullEffective[row]==0) {
        # print("true: vax only")
        summary.file$cullEffective[row]= 0
        summary.file$vaxEffective[row]=rep.totals$anim[which(rep.totals$Rep==summary.file$Rep[row] & rep.totals$controlStatus == "effective.vax")]
      } else {
        summary.file$vaxEffective[row]=rep.totals$anim[which(rep.totals$Rep==summary.file$Rep[row] & rep.totals$controlStatus == "effective.vax")]
        summary.file$cullEffective[row]=rep.totals$anim[which(rep.totals$Rep==summary.file$Rep[row]  & rep.totals$controlStatus == "effective.cull")]
        # print("BOTH")
      }
    }
    
    #run.anim=rep.totals
    run.anim=summary.file %>%
      group_by(Rep, Seed_FIPS, Seed_Farms) %>%
      arrange(Seed_FIPS)
    
    rm(rep.totals, control.file, summary.file)
    gc()
    
    run.anim$type <- control.summary.files[file]
    
    Anim.list[[file]] <- .extract_runDetailsFromType(run.anim)
    gc()
    if (file == (length(control.summary.files)/2)) {cat("Halfway through control summary files (",file,")")}
  } # End summary/detail file loop
  
  
  Anim.long <- bind_rows(Anim.list, .id = "id")
  
  for (file in 1:length(no_control)) {
    no_control_df <- fread(paste0(pathfiles,no_control[file]), header = TRUE, select = c("Rep", "Seed_Farms", "Seed_FIPS", "Duration"))
    no_control_df$cullEffective = 0
    no_control_df$vaxEffective = 0
    no_control_df$type <- no_control[file]
    no_control_df <- .extract_runDetailsFromType(no_control_df)
    
    Anim.long <- Anim.long %>% bind_rows(no_control_df)
  }
  
  setwd(path_output)
  if (export.datafiles == 1 | export.datafiles == 3) {write.csv(Anim.long,paste0("Dur_Cull_Vax_NumInf_",Sys.Date(),".csv"),row.names=F)}
  
  return(Anim.long)
  print("Importing USDOS control output complete.")
}

#' Imports FLAPS data files that are necessary for other importing functions
#'
#' This function imports FLAPS data files in USDOS format from the specified path and assigns them to the global environment as data frames.
#'
#' @param path0 The path to the directory containing the directory that holds flaps.
#' @return named flaps files (f1 to f10) in the global environment
#' @export
.import_FLAPS = function (path0 = NULL) {
  
flaps_file_list <- list()
flaps_file_list <- lapply(1:10, function(flap) {
    flname <- paste0("f", flap)
    flaps_file_path <- file.path(paste0(path0, paste0("/FLAPS/FLAPS12_Quarterly_USDOS_format_", sprintf("%04d", flap), ".txt")))
    flaps_file <- fread(flaps_file_path)
    flaps_file$anim=flaps_file$b_Q3+flaps_file$d_Q3
    flaps_file$County_fips[which(flaps_file$County_fips==46113)]<- 46102
    return(flaps_file)
  })
  return(flaps_file_list)
}

#' Import control output files for completion figures
#'
#' This function imports control output files and creates a summary of the proportion of completed, partial, and no control runs across all files. It also exports the summary as a CSV file to the specified path. 
#'
#' @param summary.files a vector of file names with control output data
#' @param pathfiles the path to the directory where the summary files are located
#' @param export.datafiles a flag to export the summary data as a CSV file. 1 to export only the completion summary, 2 to export only the waitlist summary, 3 to export both summaries.
#' @param path_output the path to the directory where the CSV file(s) should be saved
#'
#' @return a data frame with the completion summary data for all input files
#'
#' @export
.import_controlWaitlistSummaryFiles = function (summary.files = NULL, pathfiles = NULL, export.datafiles = 1, path_output = NULL) {
  
  print("Importing USDOS control output files for completion figures.")
  
  Anim.list <- list()

  # Read through each summary/detail file 
  for (file in 1:length(summary.files)){
    # read summary file
    if((str_detect(summary.files[file], "cull") & str_detect(summary.files[file], "vax")) |
       str_detect(summary.files[file], "cullVax")) {
      vax <- fread(cmd = paste('grep', 'effective.vax', paste0(pathfiles,summary.files[file])))
      cull <- fread(cmd = paste('grep', 'effective.cull', paste0(pathfiles,summary.files[file])))

      sum.file <- bind_rows(cull,vax)
      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    } else if (str_detect(summary.files[file], "vax")) {
      sum.file <- fread(cmd = paste('grep', 'effective.vax', paste0(pathfiles,summary.files[file])))

      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    } else if (str_detect(summary.files[file], "cull")) {
      sum.file <- fread(cmd = paste('grep', 'effective.cull', paste0(pathfiles,summary.files[file])))

      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    } else {
      vax <- tryCatch(fread(cmd = paste('grep', 'effective.vax', paste0(pathfiles,summary.files[file]))))
      cull <- tryCatch(fread(cmd = paste('grep', 'effective.cull', paste0(pathfiles,summary.files[file]))))

      sum.file <- bind_rows(cull,vax)
    }
    
    # Number of reps. Used to fill in where no control occured
    numReps <- 1:3049
    
    rep.totals <- sum.file %>%
      distinct(Rep, controlStatus, .keep_all = TRUE) %>%
      complete(Rep = numReps, fill = list(controlStatus = "none", farmID = 0, tStep = 0, FIPS = 0)) %>% #farmID = 0, tStep = 0, FIPS = 0, anim = 0
      group_by(Rep) %>%
      summarise(completed = all(c("effective.cull", "effective.vax") %in% controlStatus), 
                partial = all(("effective.cull" %in% controlStatus) & !("effective.vax" %in% controlStatus)),
                noControl = all("none" %in% controlStatus)) %>%
      # mutate(completed = case_when(completed == FALSE ~ 0, TRUE ~ 1),
      #        partial = case_when(partial == FALSE ~ 0, TRUE ~ 1),
      #        noControl = case_when(noControl == FALSE ~ 0, TRUE ~ 1)) %>%
      summarise(prop_both = mean(completed), prop_one = mean(partial), prop_none = mean(noControl))
    
    run.anim=rep.totals
    
    rm(rep.totals, cull, vax, sum.file)
    gc()
    
    run.anim$type <- summary.files[file]
    
    Anim.list[[file]] <- .extract_runDetailsFromType(run.anim)
    
    gc()
    print(file)
  } # End summary/detail file loop
  
  Anim.long <- bind_rows(Anim.list, .id = "id")
  
  setwd(path_output)
  if (export.datafiles == 1 | export.datafiles == 3) {write.csv(Anim.long,paste0(paste0("Control_Completion_",Sys.Date()),".csv"),row.names=F)}
  
  return(Anim.long)
  print("Importing USDOS control and waitlist output complete.")
}

#' Import control and waitlist output files
#'
#' This function imports USDOS control and waitlist output files for waitlist-related figures. It reads through each summary/detail file and merges the FLAPS file with the summary file to get the number of animals on the seed farm. Then, it calculates the last control status for each group and filters out the "effective.cull" and "effective.vax" statuses. Finally, it stores the output in a data frame and exports it to a file if specified.
#'
#' @param summary.files A vector of summary file names.
#' @param waitlist.files A vector of waitlist file names.
#' @param pathfiles The path to the directory containing the input files.
#' @param export.datafiles A flag indicating whether to export the output data files. Default is 1 (TRUE).
#' @param path_output The path to the directory where the output files should be stored.
#'
#' @return A list of data frames containing the last control status for each group, and the total number of animals on the seed farm. Each data frame corresponds to a waitlist file.
#' 
#' @export
.import_waitlistFiles = function(summary.files = NULL, waitlist.files = NULL, pathfiles = NULL, export.datafiles = 1, path_output = NULL) {
  
  print("Importing USDOS control and waitlist output files for waitlist-related figures. Importing these files takes awhile. Grab some coffee.")
  
  Anim.list <- list()
  
  #if (length(waitlist.files) != length(summary.files)) {stop("Waitlist and summary file lengths do not match.")}
  
  # Read through each summary/detail file 
  for (file in 1:length(waitlist.files)){
    # read summary file
    if((str_detect(waitlist.files[file], "cull") & str_detect(waitlist.files[file], "vax")) |
       str_detect(waitlist.files[file], "cullVax")) {
      cull_waitlist <- fread(cmd = paste('grep', 'waitlist.cull', paste0(pathfiles,waitlist.files[file])))
      vax_waitlist <- fread(cmd = paste('grep', 'waitlist.vax', paste0(pathfiles,waitlist.files[file])))
      run <- str_split(waitlist.files[file], "_waitlistSummary")[[1]][1]
      
      summary.file <- summary.files %>% str_subset(run) #The order of files is off, so make sure we are getting the right one
      
      vax <- fread(cmd = paste('grep', 'effective.vax', paste0(pathfiles,summary.file)))
      cull <- fread(cmd = paste('grep', 'effective.cull', paste0(pathfiles,summary.file)))
      sum.file <- bind_rows(cull, vax, cull_waitlist, vax_waitlist)
      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    } else if (str_detect(waitlist.files[file], "vax")) {
      vax_waitlist <- fread(cmd = paste('grep', 'waitlist.vax', paste0(pathfiles,waitlist.files[file])))
      run <- str_split(waitlist.files[file], "_waitlistSummary")[[1]][1]
      
      summary.file <- summary.files %>% str_subset(run) #The order of files is off, so make sure we are getting the right one
      
      vax <- fread(cmd = paste('grep', 'effective.vax', paste0(pathfiles,summary.file)))
      sum.file <- bind_rows(vax, vax_waitlist)
      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    } else if (str_detect(waitlist.files[file], "cull")) {
      cull_waitlist <- fread(cmd = paste('grep', 'waitlist.cull', paste0(pathfiles,waitlist.files[file])))
      run <- str_split(waitlist.files[file], "_waitlistSummary")[[1]][1]
      
      summary.file <- summary.files %>% str_subset(run) #The order of files is off, so make sure we are getting the right one
      
      cull <- fread(cmd = paste('grep', 'effective.cull', paste0(pathfiles,summary.file)))
      sum.file <- bind_rows(cull, cull_waitlist)
      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    } else {
      cull_waitlist <- tryCatch(fread(cmd = paste('grep', 'waitlist.cull', paste0(pathfiles,waitlist.files[file]))), error = function(e) NULL)
      vax_waitlist <- tryCatch(fread(cmd = paste('grep', 'waitlist.vax', paste0(pathfiles,waitlist.files[file]))), error = function(e) NULL)
      run <- str_split(waitlist.files[file], "_waitlistSummary")[[1]][1]
      
      summary.file <- summary.files %>% str_subset(run) #The order of files is off, so make sure we are getting the right one
      
      vax <- tryCatch(fread(cmd = paste('grep', 'effective.vax', paste0(pathfiles,summary.file))), error = function(e) NULL)
      cull <- tryCatch(fread(cmd = paste('grep', 'effective.cull', paste0(pathfiles,summary.file))), error = function(e) NULL)
      sum.file <- bind_rows(cull, vax, cull_waitlist, vax_waitlist)
      colnames(sum.file) <- c("Rep","tStep","farmID","FIPS","controlStatus")
    }
    
    # Select the right FLAPS file
    FLAP <- substr(unlist(strsplit(summary.files[file],"format_"))[2],1,4) 
    flaps <- get(paste0("f",as.numeric(FLAP)))
    
    # merge FLAPS with summary file to get # animals on seed farm
    sum.flaps <- merge(sum.file,flaps[,c("Id", "anim")],by.x="farmID",by.y="Id",all.x=TRUE)
    numReps <- 1:3049
    
    rep.totals <- sum.flaps %>% 
      group_by(Rep, farmID, anim) %>%
      summarise(lastStatus = tail(controlStatus, n = 1)) %>% #extract the last control status for each group
      filter(!lastStatus %in% c("effective.cull", "effective.vax"))
    
    run.anim=rep.totals
    
    rm(sum.flaps, rep.totals, cull, vax, sum.file)
    gc()
    
    run.anim$type <- waitlist.files[file]
    
    run.anim <- .extract_runDetailsFromType(run.anim)
    
    Anim.list[[file]] <- run.anim
    gc()
    print(file)
  } # End summary/detail file loop
  
  Anim.long <- bind_rows(Anim.list, .id = "id")
  
  setwd(path_output)
  if (export.datafiles == 1 | export.datafiles == 3) {write.csv(Anim.long,paste0(paste0("Waitlist_",Sys.Date()),".csv"),row.names=F)}
  
  return(Anim.long)
  print("Importing USDOS control and waitlist output complete.")
  
}


#' Extract run details from type
#' This function extracts run details from the type column of the input dataframe. It adds several columns to the dataframe including delay, type, cType, and trigger. The function also removes certain parts of the type column based on regular expressions.
#' @param df A dataframe containing the type column for a typical USDOS run.
#'
#' @return A dataframe with additional columns delay, type, cType, and trigger.
#'
#' @export
.extract_runDetailsFromType <- function (df) {
  
  if (any(str_detect(df, "newPremReportsOverX|percentIncrease|decrease|availability"))) {
    df <- df %>%
      mutate(type = str_replace(type, ".*_Infectious20days_", "") %>%
               str_remove("_MvmtBan_.*"),
             delay = as.integer(sub('.*_(\\d+)$', '\\1', type)),
             control_type = case_when(str_detect(type,"percentIncrease|availability|decrease") ~ "State-dependent",
                                      str_detect(type,"newPremReportsOverX") ~ "Static"),
             delay = if_else(is.na(delay), -1, delay),
             type = str_replace(type, "^[^_]+_[^_]+_", "")
    )
  } else if (any(str_detect(df, "noControl"))) {
    df <- df %>%
      mutate(delay = "No control",
             type = str_replace(type, "FMD_.*PTon_Infectious20days_", "") %>%
               str_remove("_noDiagnostics_.*"),
             cType = "No control",
             trigger = "No control"
      )
  } else {
    df <- df %>%
      mutate(type = str_replace(type, ".*_Infectious20days_", "") %>%
               str_remove("_FLAPS.*"))
  }
  return(df)
}

.extract_runDetailsFromTypeString <- function (df) {
  
  if (any(str_detect(df, "newRegionReportsOverX|newPremReportsOverX|percentIncrease|decrease|availability"))) {
    type <- df %>% str_replace(".*_Infectious20days_", "") %>%
      str_remove("_MvmtBan_.*")
    delay <- as.integer(sub('.*_(\\d+)$', '\\1', type))
    control_type <- case_when(str_detect(df,"percentIncrease|availability|decrease") ~ "Adaptive",
                              str_detect(df,"newPremReportsOverX") ~ "Fixed")
    delay <- if_else(is.na(delay), -1, delay)
    type <- str_replace(type, "^[^_]+_[^_]+_", "")
  } else if (any(str_detect(df, "noControl"))) {
    type <- df %>% str_replace(".*_Infectious20days_", "") %>%
      str_remove("_noDiagnostics_.*")
    delay <- as.integer(sub('.*_(\\d+)$', '\\1', type))
    control_type <- "No control"
    delay <- if_else(is.na(delay), -1, delay)
    type <- str_replace(type, "^[^_]+_[^_]+_", "")
  } else {
    df <- df %>%
      mutate(type = str_replace(type, ".*_Infectious20days_", "") %>%
               str_remove("_FLAPS.*"))
  }
  return(list(type = type, delay = delay, control_type = control_type))
}

#' Reshape data from wide to long format
#'
#' This function reshapes wide-format data to long format for plotting. It also converts column names to a more standardized format and exports the data as CSV files if desired.
#'
#' @param metric A string indicating the name of the metric being measured.
#' @param wide_data A data frame in wide format to be reshaped.
#' @param flex A logical indicating whether the type column should be separated into two columns.
#' @param export.datafiles An integer indicating which CSV files to export. 1 = wide format only, 2 = long format only, 3 = both.
#'
#' @return A list containing the wide and long format data frames.
#'
#' @export
.reshape_data = function (metric = NULL, wide_data = NULL, dataExist = FALSE, export.datafiles = 3) {
  
  cat("Reshaping", metric, "data.\n")
  
  names(wide_data)[seq(4,ncol(wide_data),2)] <- paste0("type_", sprintf("%03d", seq(1:((ncol(wide_data))/2-1))))
  names(wide_data)[seq(3,ncol(wide_data),2)] <- paste0("run_", sprintf("%03d", seq(1:((ncol(wide_data))/2-1))))
  
  # Convert to long shape for plotting  
  long_data <- reshape(wide_data, idvar = c("fips", "polyname"), direction = "long", 
                       v.names = c("type", "Value"), 
                       varying = list(c(grep("type_", colnames(wide_data))), 
                                      c(grep("run_", colnames(wide_data)))))
  
  rownames(long_data) <- seq(1:nrow(long_data))
  long_data <- long_data[,-3]
  long_data <- long_data[!is.na(long_data$Value),] 
  long_data$Value <- as.numeric(long_data$Value)
  long_data$type <- factor(long_data$type, levels = run.types)
  
  if(!dataExist){
    if (export.datafiles == 1 ) {
      write.csv(wide_data, paste0(metric,".csv"),row.names=F)
    } else if (export.datafiles == 2 ) {
      write.csv(long_data, paste0(metric,"_long.csv"), row.names = F)
    } else if (export.datafiles == 3) {
      write.csv(wide_data, paste0(metric,".csv"),row.names=F)
      write.csv(long_data, paste0(metric,"_long.csv"), row.names = F)
    }
  }
  
  return(list(wide_data,long_data))
  
}

.edit_runsInPlots = function(run.types = NULL, run_subset = NULL, custom_labels = NULL){
  
    cat("\n =============================================================================================== \n",
        "Custom labels option is on. \n",
        "Note: Press enter/return when prompted if you would like to skip the next two operations. \n",
        "Otherwise, follow the instructions in the prompts given. \n",
        "===============================================================================================")
    
    Sys.sleep(3)
    
    # Add numbers in front of each type
    numbered_types <- paste(seq_along(run.types), run.types, sep = '. ')
    
    cat("=========================================================== \n",
        paste(numbered_types, collapse = "\n"),
        "\n \n The order of violin plot labels is listed above.",
        "\n \n Would you like to import new names? If yes, enter a vector of names in the order of the old names (e.g. c(name1,name2,name3,...)",
        "\n If not, just press `enter` for default names to be used.",
        "\n ===========================================================")
    # Capture user input for new names
    custom_names <- readline(prompt = "")
    
    # Convert the user input to a vector
    custom_names <- eval(parse(text = custom_names))
    
    if (is.null(custom_names)) {
      warning("Vector is empty. Using default names", immediate. = TRUE)
      custom_names <- NULL # just for good measure
    } else if (length(custom_names) != length(run.types)) {       # Check if the length matches the number of levels
      warning("The length of the provided names vector does not match the number of levels. Using default names.", immediate. = TRUE)
      custom_names <- NULL
    }
    
    cat("\n =========================================================== \n",
        "Would you like to only plot a subset of USDOS runs? 
        \n If yes, enter a vector of the runs you would like to keep in the order listed (e.g. c(1,,3,6...)
        \n If not, just press `enter` for all runs to be plotted. \n",
        "===========================================================")
    # Capture user input for new names
    run_subset <- readline(prompt = "")
    
    # Convert the user input to a vector
    run_subset <- eval(parse(text = run_subset))
    
    if (is.null(run_subset)) {
      warning("Vector is empty. Plotting all runs", immediate. = TRUE)
      run_subset <- NULL # just for good measure
    } else if (length(run_subset) == length(run.types)) {
      warning("Subset of runs is equal to original length. Setting back to default.", immediate. = TRUE)
      run_subset <- NULL
    }
    
    if (!is.null(run_subset)) {
      run_subset <- run.types[run_subset]
    }
    
    #run_subset <- run.types[c(5,6,7,8,9,10,11,12,20,21,22,23,24,25,26,27)]
    
    return(list(custom_names = custom_names, run_subset = run_subset))
}

.import_detailFiles = function(){

  ## Read in and format all FLAPS files ##
  # These can be merged with detail files to identify characteristics of (potentially) exposed/infected farms.
  setwd(path0)
  for(flap in 1:10){
    flname=paste0("f",flap)
    assign(flname,fread(paste0("FLAPS/FLAPS12_Quarterly_USDOS_format_",sprintf("%04d",flap),".txt")))
  }

  for (f in 1:10){
    df=get(paste0("f",f))
    #  df$anim=df$V5+df$V6 Old FLAPS
    df$anim=df$b_Q3+df$d_Q3
    #  df$V2[which(df$V2==46113)]<- 46102 Old FLAPS
    df$County_fips[which(df$County_fips==46113)]<- 46102
    assign(paste0("f",f),df)
  }
  rm(df)

  # Initiate the county.anim file with the first single file
  # read the summary file
  setwd(pathfiles)
  sum.file <- fread(summary.files[1], header = TRUE, select = c("Rep","Num_Inf", "Seed_Farms", "Seed_FIPS"))

  setwd(path0)

  # Select the right FLAPS file
  FLAP <- substr(unlist(strsplit(summary.files[1],"format_"))[2],1,4)

  flaps <- get(paste0("f",as.numeric(FLAP)))

  # merge FLAPS with summary file to get # animals on seed farm
  sum.flaps <- merge(sum.file,flaps[,c("Id", "anim")],by.x="Seed_Farms",by.y="Id",all.x=TRUE)

  # Read detail file
  setwd(pathfiles)
  det.file <- fread(detail.fnames[1], header = TRUE, select = c("Rep","ExposedID", "ExposedCounty", "SourceID", "SourceCounty", "ControlPrevented"))

  # Filter on not prevented and unique ExposureID by Rep
  det.file <- det.file[det.file$ControlPrevented == "none",]
  det.file <- unique(as.data.table(det.file),by=c("Rep","ExposedID"))

  # Summarize the number of animals infected per Rep
  rep.totals <- merge(flaps[,c("Id", "County_fips", "anim")], det.file[,c("Rep","ExposedID", "ExposedCounty")],
                      by.x = c("Id", "County_fips"), by.y = c("ExposedID", "ExposedCounty"), all = TRUE)
  rep.totals <- as.data.frame(aggregate(rep.totals$anim, by = list(rep.totals$Rep), FUN = sum))
  colnames(rep.totals) <- c("Rep", "anim")

  # If a Rep's Num_Inf >1, get the additional (non seed farm) animals infected in that rep from the detail file
  for(row in 1:max(sum.flaps$Rep)){
    if(sum.flaps$Num_Inf[row]>1){sum.flaps$anim[row]=sum.flaps$anim[row]+rep.totals$anim[which(rep.totals$Rep==sum.flaps$Rep[row])]}
  }

  Anim=sum.flaps[,c("Seed_FIPS","anim")]

  # Get type
  Anim$type <- unlist(strsplit(summary.files[1], "_FLAPS"))[1]
  #Anim$type <- unlist(strsplit(Anim$type, "/"))[2]
  Anim<-as.data.frame(Anim)


  # Read through each summary/detail file
  for (file in 2:length(detail.fnames)){
    # read summary file
    sum.file <- fread(summary.files[file], header = TRUE, select = c("Rep","Num_Inf", "Seed_Farms", "Seed_FIPS"))

    # Select the right FLAPS file
    FLAP <- substr(unlist(strsplit(summary.files[file],"format_"))[2],1,4)
    flaps = get(paste0("f",as.numeric(FLAP)))

    # merge FLAPS with summary file
    sum.flaps=merge(sum.file,flaps[,c("Id", "anim")],by.x="Seed_Farms",by.y="Id",all.x=TRUE)

    # Read detail file
    det.file <- fread(detail.fnames[file], header = TRUE, select = c("Rep","ExposedID", "ExposedCounty", "SourceID", "SourceCounty", "ControlPrevented"))

    # Filter on not prevented and unique ExposureID by Rep
    det.file <- det.file[det.file$ControlPrevented == "none",]
    det.file <- unique(as.data.table(det.file),by=c("Rep","ExposedID"))

    # Summarize the number of animals infected per Rep
    rep.totals <- merge(flaps[,c("Id", "County_fips", "anim")], det.file[,c("Rep","ExposedID", "ExposedCounty")],
                        by.x = c("Id", "County_fips"), by.y = c("ExposedID", "ExposedCounty"), all = TRUE)
    rep.totals <- as.data.frame(aggregate(rep.totals$anim, by = list(rep.totals$Rep), FUN = sum))
    colnames(rep.totals) <- c("Rep", "anim")

    # If a Rep's Num_Inf >1, get the additional (non seed farm) animals infected in that rep from the detail file
    for(row in 1:max(sum.flaps$Rep)){
      if(sum.flaps$Num_Inf[row]>1){sum.flaps$anim[row]=sum.flaps$anim[row]+rep.totals$anim[which(rep.totals$Rep==sum.flaps$Rep[row])]}
    }

    run.anim=sum.flaps[,c("Seed_FIPS","anim")]

    # Get type
    run.anim$type <- unlist(strsplit(summary.files[file], "_FLAPS"))[1]
    #run.anim$type <- unlist(strsplit(run.anim$type, "/"))[2]
    run.anim <- as.data.frame(run.anim)

    Anim=merge(Anim,run.anim, by="Seed_FIPS")

  } # End summary/detail file loop
}

.exclude_adjacentTypeColumns = function(wide_data = NULL) {
  # Identify adjacent "type" columns and exclude them from the selection
  deleteme <- c(FALSE, diff(grep("type", colnames(wide_data))) == 1)
  wide_data <- wide_data[, !deleteme]
  
  deleteme=c(0,rep(NA,ncol(wide_data)-1))
  
  # this gets rid of adjacent "type" columns, which indicate that a movement ban wasn't implemented
  for(i in 2:ncol(wide_data)){
    deleteme[i]=ifelse(grepl("type", colnames(wide_data)[i-1]) & grepl("type", colnames(wide_data)[i])|
                         grepl("type", colnames(wide_data)[i]) & grepl("type", colnames(wide_data)[i+1]),1,0)
  }
  
  wide_data=wide_data[,deleteme==0]
  
  return(wide_data)
}

# # Identify adjacent "type" columns and exclude them from the selection
# deleteme <- c(FALSE, diff(grep("type", colnames(ReportedPrems))) == 1)
# ReportedPrems <- ReportedPrems[, !deleteme]
# 
# deleteme=c(0,rep(NA,ncol(ReportedPrems)-1))
# 
# # this gets rid of adjacent "type" columns, which indicate that a movement ban wasn't implemented
# for(i in 2:ncol(ReportedPrems)){
#   deleteme[i]=ifelse(grepl("type", colnames(ReportedPrems)[i-1]) & grepl("type", colnames(ReportedPrems)[i])|
#                        grepl("type", colnames(ReportedPrems)[i]) & grepl("type", colnames(ReportedPrems)[i+1]),1,0)
# }
# 
# ReportedPrems=ReportedPrems[,deleteme==0]

.generate_dataFiles = function(county.summary = NULL, metric = NULL, summary_file_colname = NULL, 
                                     export.datafiles = 3, dataExist = FALSE) {
  
  wide_data_name = paste0(metric,".csv")
  long_data_name = paste0(metric, "_long.csv")
  data_files_found = (wide_data_name %in% list.files()) & (long_data_name %in% list.files())
  
  if (dataExist & data_files_found) {
    wide_data <- read.csv(wide_data_name, header = TRUE)
    long_data <- read.csv(long_data_name, header = TRUE)
  } else { #otherwise proceed as usual
    wide_data=county.summary[,grepl(paste0("fips|polyname|",summary_file_colname,"|type") , names( county.summary ) )]
    
    if (any(grepl("Cull|Vax",metric))) {
      wide_data=wide_data[,!grepl( "DCSubset" , names( wide_data ) )]
    }
    
    #This function returns a wide dataframe without type columns next to eachother
    # wide_data = .exclude_adjacentTypeColumns(wide_data = wide_data)
    if (grepl("ReportedPremises|DiagTest|PremisesVax|PremisesCull",metric)) {
      if(grepl("ReportedPremises",metric)){
        # Identify adjacent "type" columns and exclude them from the selection
        deleteme <- c(FALSE, diff(grep("type", colnames(wide_data))) == 1)
        wide_data <- wide_data[, !deleteme] 
      }
      deleteme=c(0,rep(NA,ncol(wide_data)-1))
      
      # this gets rid of adjacent "type" columns, which indicate that a movement ban wasn't implemented
      for(i in 2:ncol(wide_data)){
        deleteme[i]=ifelse(grepl("type", colnames(wide_data)[i-1]) & grepl("type", colnames(wide_data)[i])|
                             grepl("type", colnames(wide_data)[i]) & grepl("type", colnames(wide_data)[i+1]),1,0)
      }
      
      wide_data=wide_data[,deleteme==0]
    }
    
    if(ncol(wide_data)>2){
      #This function returns a list of two data frames. The first dataframe is wide and the second dataframe is long
      data = .reshape_data(wide_data = wide_data, metric = metric, dataExist = dataExist, export.datafiles = export.datafiles)
    }
  }
  return(data)
}
