## USDOS Results Processing ##
# Generates plots, maps, summaries, and data files from USDOS summary and detail output files

# Code by Deedra Murrieta & Katie Owers - Sophie McKee

# Summary and Detail files must be saved in the directory "Files_To_Process" in the Post_Processing directory.


#==============================================================================================

#' Process USDOS output files
#'
#' Generate summaries of USDOS runs as figures, maps, files, and tables based on the model's summary and detail file outputs. Set the working directory as a folder containing the output files to be processed. 
#'
#' @param  export.datafiles Export the files used to calculate USDOS metrics? 0 = no (default), 1 = export wide files that contain each run's values in a column, followed by a column with the run type, 2 = export long files hat contain all run values in a single column wit another column of tun types, 3 = export both wide and long files
#' @param  results.report Should an html results report explaining results and plots be generated? (TRUE/FALSE, default = TRUE)
#' @param  summaryTable Generate a summary table for Duration, animals infected, premises infected, and epidemic extent? (TRUE/FALSE, default = TRUE)
#' @param  duration Generate figures for the duration of infection metric (as measured in model timesteps--days for FMD and months for bTB). (TRUE/FALSE, default = TRUE)
#' @param  Dur_min A mimumum duration of interest (default = 13)
#' @param  Dur_cutoff A duration above which an outreak would be considered "large" (default = 100)
#' @param  premInf Generate figures for the infected premises metric? (TRUE/FALSE, default = TRUE)
#' @param  PremInf_min A minimum number of premises infected of interest (default = 10)
#' @param  PremInf_cutoff A number of infected premises above which an outbreak would be considered 'large' (default = 1000)
#' @param  premReport Generate figures for the reported premises metric? This is only used for runs with diagnostic tests. It also generates figures comparing the numbers of reported and infected premises (TRUE/FALSE, default = TRUE)
#' @param  ReportedPrems_min A minimum number of premises reported that is of interest (default = 5)
#' @param  ReportedPrems_cutoff A number of infected premises above which an outbreak would be considered 'large' (default = 100)
#' @param  epidemicExtent Generate figures for the epidemic extent metric? (TRUE/FALSE, default = TRUE)
#' @param  EpidExt_min A minimum number of counties infected that is of interest (default = 1)
#' @param  EpidExt_cutoff A number of infected counties above which an outbreak would be considered 'large'
#' @param  movementBan  Generate figures for the number of geographies (states or counties as specified in the configuration file)? (TRUE/FALSE, default = TRUE)
#' @param  MB_min A minimum number of areas affected by a movement ban that would be of interest (default= 0)
#' @param  MB_cutoff A number of areas affected by a movement ban above which an outbreak would be considered 'large'
#' @param  premisesCulled Generate figures for the premises culled metric? Only used for runs with a control type that includes culling. (TRUE/FALSE, default = TRUE)
#' @param  PremCull_min A minimumm number of premises culled that is of interest (default = 0)
#' @param  PremCull_cutoff A number of culled premises above which an outbreak would be considered 'large' (default = 10)
#' @param  premisesVax Generate figures for the premises vaccinated metric? Only used for runs with a control type that includes vaccination. (TRUE/FALSE, default = TRUE)
#' @param  PremVax_min A minimum number of premises vaccinated that is of interest (default = 0)
#' @param  PremVax_cutoff A number of premises vaccinated above which an outbreak would be consdiered 'large'
#' @param  diagnosticTests Generate figures for the number of diagnostic tests completed? Only used for runs with diagnostic type including testing. (TRUE/FALSE, default = TRUE)
#' @param  animalsInfected Genreate figures for the number of animals infected metric? (TRUE/FALSE, default = TRUE)
#' @param  Anim_min A minimum number of animals infected that is of interest (default = 10)
#' @param  Anim_cutoff A number of animals infected above which an outbreak would be considered 'large' (default = 1000)
#' @param  countyRisk Generate figures for the county risk metric? (TRUE/FALSE, default = TRUE)
#' @param  CountyRisk_min A minimum county risk that is of interest (default = 0)
#' @param  CountyRisk_cutoff A county risk that is considered large (default = 0.001, crresponding to a county becoming infected when outbreaks are seeded in at least four other counties. (TRUE/FALSE, default = TRUE)
#' @param  localSpread Generate figures for the proportion of spread that is due to local spread (versus shipment). (TRUE/FALSE, default = TRUE)
#' @param  plot_color The color palette for maps other than local spread (see ls_match option below to change the local spread map). By default this is "color_red" (the RColorBrewer 'OrRd' palette). Other options are "color_blue" (RColorBrewer 'Blues'), "color_orange" ('Oranges'), "color_pink" ('RdPu'), "color_bluepurple" ('BuPu'), and "color_yellow" (custom)
#' @param  map_color The color palette for maps other than local spread (see ls_match option below to change the local spread map). By default this is "color_red" (the RColorBrewer 'OrRd' palette). Other options are "color_blue" (RColorBrewer 'Blues'), "color_orange" ('Oranges'), "color_pink" ('RdPu'), "color_bluepurple" ('BuPu'), and "color_yellow" (custom)
#' @param  ls_match Should the local spread map's color palette match the other maps (TRUE, the default value) or use its default of color_bluepurple (FALSE)
#' @param  controlValue Calculates the cost of control actions based upon the number of animals culled and vaccinated in a scenario
#' @param  animalsControlled Indicates that the number of animals controlled (culled or vaccinated) should be drawn from the _controlSummary.txt file. 
#' @param  waitlistInformation
#' @param  flexibleControl Indicates that there are flexible or state-dependent control runs that will be imported. This is will affect how run type names are parsed. 
#' @param  verbose Controls the amount of information printed as the function operates (0 = nothing, 1 = debug)
#' @param  usdos_output_file_path Path to USDOS summary and detail files if not located in a Files_To_Path directory in the current directory. Default is '.'. 

#' @details 
#' [metric]_min Allows users to plot results excluding simulations below a certain minimum value. For example, since the duration of outbreaks that do not spread is 13 days, setting a Dur_minimal of 13 would generate plots which exclude outbreaks that do not persist beyond the index infection.
#' [metric]_cutoff Allows users to set a cutoff over which an outbreak would be considered "large". Outbreaks that go above this threshold will be plotted separately fromthose that stay below it

#' @include Include_Files/Data_Manager.R

#' @author Send bug reports, suggestions, corrections, or comments to webblaboratory(at)gmail.com
#' 
#==============================================================================================
#==============================================================================================

#     Function       

#==============================================================================================
#==============================================================================================

processUSDOS = function(export.datafiles= 0,
                        results.report = FALSE,
                        summaryTable = TRUE,
                        duration = TRUE,
                        Dur_min = 23,
                        Dur_cutoff = 175,
                        premInf = TRUE,
                        PremInf_min = 10, 
                        PremInf_cutoff = 5000,
                        premReport = TRUE,
                        ReportedPrems_min = 5,
                        ReportedPrems_cutoff = 100,
                        epidemicExtent = TRUE,
                        EpidExt_min= 2,
                        EpidExt_cutoff=500,
                        movementBan = TRUE,
                        MB_min = 0,
                        MB_cutoff = 1,
                        premisesCulled = TRUE,
                        PremCull_min = 0,
                        PremCull_cutoff = 10,
                        premisesVax = TRUE,
                        PremVax_min = 0,
                        PremVax_cutoff = 2,
                        diagnosticTests = TRUE,
                        DiagTest_min = 25,
                        DiagTest_cutoff = 1000,
                        animalsInfected = TRUE,
                        Anim_min = 10,
                        Anim_cutoff = 5e6,
                        countyRisk = TRUE,
                        CountyRisk_min = 0,
                        CountyRisk_cutoff = 0.001,
                        localSpread = TRUE,
                        plot_color = "color_red",
                        map_color = "color_red",
                        ls_match = TRUE,
                        controlValue = FALSE,
                        animalsControlled = FALSE,
                        waitlistInformation = FALSE,
                        completionProportion = FALSE,
                        maps = FALSE,
                        plots = FALSE,
                        dataExist = FALSE,
                        flexibleControl = FALSE,
                        verbose = 1,
                        custom_labels = FALSE,
                        usdos_output_file_path = "."
                       )
{
#==============================================================================================
#==============================================================================================

#     Setup      

#==============================================================================================
#==============================================================================================
# This function finds the directory in which the the current file is located.
path0 <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path0)
# Where figures/csvs will be sent to
data_output <- file.path(path0, "Data/")
dir.create(data_output, showWarnings = FALSE) #create directory
# Where figures/csvs will be sent to
plot_output <- file.path(path0, "Figures/")
dir.create(plot_output, showWarnings = FALSE) #create directory
# Where figures/csvs will be sent to
map_output <- file.path(path0, "Maps/")
dir.create(map_output, showWarnings = FALSE) #create directory
# Source file directory location
source_files <- file.path(path0,"Source_Files/")
# Dependency directory location
dependencies <- file.path(path0,"Include_Files/")

## Identify all the summary files ##
if (usdos_output_file_path == ".") {
  ## Identify all the summary files ##
  pathfiles <- file.path(path0, "Files_To_Process/")
} else {
  pathfiles <- file.path(usdos_output_file_path)
  #pathfiles <- "/Volumes/webblab/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Diagnostics/Fall2023_Diagnostic_Runs/baseResults/"
} 

# Reset graphical parameters to ensure there are no issues plotting
graphics.off()

# assign verbose level to global environment so not all functions have to declare it
# assign("verbose",verbose, envir = .GlobalEnv)

## Load Packages, function to determine color palettes, geography files, functions for importing data, mapping functions ##
if (verbose > 0) {print("Loading packages (Package_Manager.R)...")}
source(paste0(dependencies,"Package_Manager.R"))
if (verbose > 0) {print("Loading color pallettes (Color_Manager.R)...")}
source(paste0(dependencies,"Color_Manager.R"))
if (verbose > 0) {print("Loading geography files...")}
source(paste0(dependencies,"load_geographyFiles.R"))
if (verbose > 0) {print("Loading functions to import summary, detail, and control files (Data_Manager.R)...")}
source(paste0(dependencies,"Data_Manager.R"))
if (verbose > 0) {print("Loading plotting functions (Plot_Manager.R)...")}
source(paste0(dependencies,"Plot_Manager.R"))
if (verbose > 0) {print("Loading mapping functions (Map_Manager.R & map_by_fips_standalone.R)...")}
source(paste0(dependencies,"map_by_fips_standalone.R"))
source(paste0(dependencies,"Map_Manager.R"))

# Read and format Summary Files #
summary.files <- list.files(path = pathfiles, recursive = TRUE, pattern = "_summary.txt", full.names = FALSE)

## Read in all the summary files ##
# Storing as a single file with many columns    

if (dataExist) {
  setwd(data_output)
  load("run_information.RData")
  county.summary <- read.csv("CountySummary.csv")
} else {
  if(detectCores() >= 4){
    # Read summary files in parallel. Uses n-2 cores available on your machine
    county.summary <- .import_summaryFiles_parallel(flexibleControl = flexibleControl,
                                                    summary.files = summary.files,
                                                    pathfiles = pathfiles,
                                                    export.datafiles = export.datafiles,
                                                    path_output = data_output) 
  } else{
    # If not enough cores are available, then import without parallelization. This will take much longer.
    print("Importing summary files without the use of parralelization. This operation may take a few minutes.")
    county.summary <- .import_summaryFiles(flexibleControl = flexibleControl,
                                           summary.files = summary.files,
                                           pathfiles = pathfiles,
                                           export.datafiles = export.datafiles,
                                           path_output = data_output)
  }
}

colors = .load_colorPalette(plot_color = plot_color, map_color = map_color, num_colors = length(run.types))

cbPalette = colors$plot_color
palette = colors$map_color
run_subset = NULL
custom_names = NULL

if (custom_labels == TRUE) {
  custom = .edit_runsInPlots(run.types = run.types, run_subset = run_subset, custom_labels = custom_names)
  custom_names = custom$custom_names
  run_subset = custom$run_subset
}

# Error if <100 summary files per run type
if( runs_per_ctrl_type<100 ) c(warning('At least 100 iterations are required to capture uncertainty in model predictions. Do not analyze fewer than 100 runs.'), rm(county.summary))

# KO: Here could parse run types into named bins (ex. it's an IPDCcull run, regardless of specifics)
    # or leave as-is (Not as clean, but people will be able to see exactly what each is, including MvmtBans)

#==============================================================================================
# Duration  (Summary Files)      
#==============================================================================================

if (duration == TRUE){
  if (verbose > 0) {print("Duration calculations")}
  
  setwd(data_output)
  data = .generate_dataFiles(county.summary = county.summary, metric = "Duration", summary_file_colname = "Duration",
                             dataExist = dataExist, export.datafiles = export.datafiles)
  
  Dur = data[[1]]
  Dur.long = data[[2]]
  
  if (plots == TRUE) {
    if (verbose > 1) {print("Creating duration plots.")}
    .plot(metric = "Duration", plot_output = plot_output, long_data = Dur.long, 
          cutoff = Dur_cutoff, min = Dur_min, cbPalette = cbPalette, custom_names = custom_names,
          run_subset = run_subset)
    if (verbose > 0) {print("Duration violin plots generated.")}
  }
  
  if (maps == TRUE) {
    if (verbose > 1) {print("Generating duration maps")}
    .map(metric = "Duration", long_data = Dur.long, wide_data = Dur, min_value = Dur_min,
         verbose = verbose, map_output = map_output, palette = palette, run.types = run.types, 
         runs_per_ctrl_type = runs_per_ctrl_type)
    if (verbose > 0) {print("Duration maps generated.")}
  }
  
}

#==============================================================================================
# Number of Premises Infected (Summary Files) 
#==============================================================================================

if (premInf == TRUE){
  
  if (verbose > 0) {print("Number of infected premises calculations")}
  
  setwd(data_output)
  data = .generate_dataFiles(county.summary = county.summary, metric = "PremisesInfected", summary_file_colname = "Num_Inf",
                             dataExist = dataExist, export.datafiles = export.datafiles)
  
  PremInf = data[[1]]
  PremInf.long = data[[2]]
  
  if (plots == TRUE) {
    if (verbose > 1) {print("Creating number of infected premises plots.")}
    .plot(metric = "PremInf", plot_output = plot_output, long_data = PremInf.long, 
          cutoff = PremInf_cutoff, min = PremInf_min, cbPalette = cbPalette, custom_names = custom_names,
          run_subset = run_subset)
    if (verbose > 0) {print("Number of infected premises plots generated.")}
  }
  
  if (maps == TRUE) {
    if (verbose > 1) {print("Generating number of infected premises maps")}
    .map(metric = "PremInf", long_data = PremInf.long, wide_data = PremInf, min_value = PremInf_min, 
         verbose = verbose, map_output = map_output, palette = palette, run.types = run.types, 
         runs_per_ctrl_type = runs_per_ctrl_type)
    if (verbose > 0) {print("Number of infected premises maps generated.")}
  }
}

#==============================================================================================
# Number of affected counties aka Epidemic Extent (nAffCounties in Summary Files) 
#==============================================================================================

if (epidemicExtent == TRUE){
  if (verbose > 0) {print("Number of affected county calculations")}
  
  setwd(data_output)
  data = .generate_dataFiles(county.summary = county.summary, metric = "EpidemicExtent", summary_file_colname = "nAffCounties",
                             dataExist = dataExist, export.datafiles = export.datafiles)
  
  EpidExt = data[[1]]
  EpidExt.long = data[[2]]
  
  if (plots == TRUE) {
    if (verbose > 1) {print("Creating number of affected county plots.")}
    .plot(metric = "EpidExt", plot_output = plot_output, long_data = EpidExt.long, 
          cutoff = EpidExt_cutoff, min = EpidExt_min, cbPalette = cbPalette, custom_names = custom_names,
          run_subset = run_subset)
    if (verbose > 0) {print("Number of affected county plots generated.")}
  }
  
  if (maps == TRUE) {
    if (verbose > 1) {print("Generating number of affected county maps")}
    .map(metric = "EpidExt", long_data = EpidExt.long, wide_data = EpidExt, min_value = EpidExt_min, 
         verbose = verbose, map_output = map_output, palette = palette, run.types = run.types, 
         runs_per_ctrl_type = runs_per_ctrl_type)
    if (verbose > 0) {print("Number of affected county maps generated.")}
  }
}

#==============================================================================================
# Number of Reported Premises (Summary Files) 
# Same (mis a part reporting delay) as infected unless there is "diagnostics" 
#==============================================================================================

if (premReport == TRUE){
  if (verbose > 0) {print("Number of reported premises calculations")}
  
  setwd(data_output)
  data = .generate_dataFiles(county.summary = county.summary, metric = "ReportedPremises", summary_file_colname = "Num_Reports",
                             dataExist = dataExist, export.datafiles = export.datafiles)
  ReportedPrems = data[[1]]
  ReportedPrems.long = data[[2]]
  
  if (ncol(ReportedPrems)>2) {
    
    if (plots == TRUE ) {
      if (verbose > 1) {print("Creating number of reported premises plots.")}
      .plot(metric = "ReportedPrems", plot_output = plot_output, long_data = ReportedPrems.long, 
            cutoff = ReportedPrems_cutoff, min = ReportedPrems_min, cbPalette = cbPalette, custom_names = custom_names,
            run_subset = run_subset)
      if (verbose > 0) {print("Number of reported premises plots generated.")}
    }
    
    if (maps == TRUE) {
      if (verbose > 1) {print("Generating number of infected premises maps")}
      .map(metric = "ReportedPrems", long_data = ReportedPrems.long, wide_data = ReportedPrems, 
           min_value = ReportedPrems_min, verbose = verbose, map_output = map_output, palette = palette, 
           run.types = run.types, runs_per_ctrl_type = runs_per_ctrl_type)
      if (verbose > 0) {print("Number of reported premises maps generated.")}
    }
  }
}

#==============================================================================================
#  Diagnostics tests completed (Summary file)    
#==============================================================================================

if (diagnosticTests == TRUE){
  #Check to ensure diagnostic testing was used in USDOS runs. If not, this section will be skipped
  diagnostics_on = grepl("Suspect|atRisk|elisa|pcr|highSens|lowSens",run.types) & !all(grepl("noDiagnostics",run.types))
  
  if (diagnostics_on) {
    if (verbose > 0) {print("Diagnostic test calculations")}
    
    setwd(data_output)
    data = .generate_dataFiles(county.summary = county.summary, metric = "DiagTest", summary_file_colname = "Complete",
                               dataExist = dataExist, export.datafiles = export.datafiles)
    
    DiagTest = data[[1]]
    DiagTest.long = data[[2]]
    
    if(ncol(DiagTest)>2){
      if (verbose > 1) {print("Creating number of diagnostic test plots.")}
      if (plots == TRUE){
        .plot(metric = "DiagTest", plot_output = plot_output, long_data = DiagTest.long, 
              cutoff = DiagTest_cutoff, min = DiagTest_min, cbPalette = cbPalette, custom_names = custom_names,
              run_subset = run_subset)
      }
      
      if (verbose > 0) {print("Diagnostic test plots generated.")}
      if (maps == TRUE) {
        if (verbose > 1) {print("Generating diagnostic test maps")}
        .map(metric = "DiagTest", long_data = DiagTest.long, wide_data = DiagTest, min_value = DiagTest_min, 
             verbose = verbose, map_output = map_output, palette = palette, run.types = run.types, 
             runs_per_ctrl_type = runs_per_ctrl_type)
        if (verbose > 0) {print("Diagnostic test maps generated.")}
      }
    } 
  }
}

#==============================================================================================
#  List detail files and load FLAPS      
#==============================================================================================

if(animalsInfected == TRUE | localSpread == TRUE | controlValue == TRUE | 
   animalsControlled == TRUE | completionProportion == TRUE) {
  setwd(path0)
  
  if (verbose > 0) {print("Finding detail file names and importing FLAPS files")}
  # Imports FLAPS files, finds detail file names, and assigns them all to global environment
  ## Find all the detail files in the Files_To_Process" directory ##
  # detail.fnames <- list.files(path = pathfiles, recursive = TRUE, pattern = "_detail.txt", full.names = FALSE)
  flaps_file_list <- .import_FLAPS(path0 = path0)
  if (verbose > 0) {print("Detail files found and FLAPS files imported.")}
}

#==============================================================================================
#  Number of animals controlled. Output file also used for cost calculations    
#==============================================================================================

if(controlValue == TRUE | animalsControlled == TRUE){
  print("Control value calculations are still under development. This operation will only produce a dataframe used to calculate value.")
  
  setwd(path0)
  ## Find all the detail files in the Files_To_Process" directory ##
  control.summary.files <- list.files(path = pathfiles, recursive = TRUE, pattern = "_controlSummary.txt", full.names = FALSE)
  summary.files <- list.files(path = pathfiles, recursive = TRUE, pattern = "_summary.txt", full.names = FALSE)
  # Read through each controlSummary file 
  # Filter summary files
  no_control <- summary.files[grep("noControl", summary.files)]
  control.summary.files <- control.summary.files[grep("cull_vax_0_-1_earliest_earliest", control.summary.files)]
  summary.files <- summary.files[grep("cull_vax_0_-1_earliest_earliest", summary.files)]
  control.summary.files <- control.summary.files[grep("newPremReportsOverX|percentIncrease|detail_20", control.summary.files)]
  summary.files <- summary.files[grep("newPremReportsOverX|percentIncrease|detail_20", summary.files)]
  
  detail.fnames <- list.files(path = pathfiles, recursive = TRUE, pattern = "_detail.txt", full.names = FALSE)
  detail.fnames <- detail.fnames[grep("cull_vax_0_-1_earliest_earliest|noControl", detail.fnames)]
  detail.fnames <- detail.fnames[grep("newPremReportsOverX|percentIncrease|noControl|detail_20", detail.fnames)]
  
  if(detectCores() >= 4){
    Anim.long <- .import_controlSummaryFiles_parallel(control_file_names = control.summary.files, 
                                                      summary_file_names = summary.files, 
                                                      no_control = no_control,
                                                      detail_file_names = detail.fnames,
                                                      pathfiles = pathfiles,
                                                      flaps_file_list = flaps_file_list,
                                                      export.datafiles = export.datafiles, 
                                                      path_output = data_output)
  } else {
    Anim.long <- .import_controlSummaryFiles(control.summary.files = control.summary.files, 
                                             summary.files = summary.files, 
                                             no_control = no_control,
                                             detail.fnames = detail.fnames,
                                             pathfiles = pathfiles,
                                             flaps_file_list = flaps_file_list,
                                             export.datafiles = export.datafiles, 
                                             path_output = data_output) 
  }
}


#==============================================================================================
#  Premises Culled (summary)      
#==============================================================================================

# Problems (fixed, see comments)
if (premisesCulled == TRUE) {
  if (verbose > 0) {print("Number of premises culled calculations")}
  
  setwd(data_output)
  
  if ((dataExist == TRUE) & ("PremisesCulled.csv" %in% list.files())) {
    PremCull <- read.csv("PremisesCulled.csv", header = TRUE)
  } else { #otherwise proceed as usual
    PremCull=county.summary[,grepl( "fips|polyname|cullImplemented|type" , names( county.summary ) )]
    PremCull=PremCull[,!grepl( "DCSubset" , names( PremCull ) )]
  }    
  
  # Identify adjacent "Type" columns and exclude them from the selection
  deleteme=c(0,rep(NA,ncol(PremCull)-1))
  for(i in 2:ncol(PremCull)){
    deleteme[i]=ifelse(grepl("Type", colnames(PremCull)[i-1]) & grepl("Type", colnames(PremCull)[i])|
                         grepl("Type", colnames(PremCull)[i]) & grepl("Type", colnames(PremCull)[i+1]),1,0)
  }
  PremCull=PremCull[,deleteme==0]
  
  if (ncol(PremCull)>2) {
    if (verbose > 0) {print("Number of reported premises calculations: ncol >2")}
    
    # This function returns a list of two data frames. The first dataframe is wide and the second dataframe is long
    data = .reshape_data(wide_data = PremCull, metric = "PremisesCulled", dataExist = dataExist, export.datafiles = export.datafiles)
    PremCull = data[[1]]
    PremCull.long = data[[2]]
    
    if (verbose > 1) {print("Creating number of culled premises plots.")}
    .plot(metric = "PremCull", plot_output = plot_output, long_data = PremCull.long, 
          cutoff = PremCull_cutoff, min = PremCull_min, cbPalette = cbPalette)
    
    if (verbose > 0) {print("Number of culled premises plots generated.")}
    
    if (maps == TRUE) {
      if (verbose > 1) {print("Generating number of culled premises maps")}
      .map(metric = "PremCull", long_data = PremCull.long, wide_data = PremCull, min_value = PremCull_min, 
           verbose = verbose, map_output = map_output, palette = palette)
      if (verbose > 0) {print("Number of culled premises maps generated.")}
    }
  }
}

#==============================================================================================
#  Premises Vaccinated (summary)      
#==============================================================================================

if (premisesVax == TRUE) {
  if (verbose > 0) {print("Number of premises vaccinated calculations")}
  
  setwd(data_output)
  
  if ((dataExist == TRUE) & ("PremisesVax.csv" %in% list.files())) {
    PremVax <- read.csv("PremisesVax.csv", header = TRUE)
  } else { #otherwise proceed as usual
    PremVax=county.summary[,grepl( "fips|polyname|vaxImplemented|type" , names( county.summary ) )]
    PremVax=PremVax[,!grepl( "DCSubset" , names( PremVax ) )]
  }    
  
  
  deleteme=c(0,rep(NA,ncol(PremVax)-1))
  # this gets rid of adjacent "type" columns, which indicate that a cull wasn't implemented
  for(i in 2:ncol(PremVax)){
    deleteme[i]=ifelse(grepl("Type", colnames(PremVax)[i-1]) & grepl("Type", colnames(PremVax)[i])|
                         grepl("Type", colnames(PremVax)[i]) & grepl("Type", colnames(PremVax)[i+1]),1,0)
  }
  PremVax=PremVax[,deleteme==0]
  
  if (ncol(PremVax)>2) {
    if (verbose > 0) {print("Number of reported premises calculations: ncol >2")}
    
    #This function returns a list of two data frames. The first dataframe is wide and the second dataframe is long
    data = .reshape_data(wide_data = PremVax, metric = "PremisesVax", dataExist = dataExist, export.datafiles = export.datafiles)
    PremVax = data[[1]]
    PremVax.long = data[[2]]
    
    if (verbose > 1) {print("Creating number of premises vaccinated plots.")}
    
    .plot(metric = "PremVax", plot_output = plot_output, long_data = PremVax.long, 
          cutoff = PremVax_cutoff, min = PremVax_min, cbPalette = cbPalette)
    
    if (verbose > 0) {print("Number of premises vaccinated plots generated.")}
    
    if (maps == TRUE) {
      if (verbose > 1) {print("Generating number of premises vaccinated maps")}
      .map(metric = "PremVax", long_data = PremVax.long, wide_data = PremVax, min_value = PremVax_min, 
           verbose = verbose, map_output = map_output, palette = palette)
      if (verbose > 0) {print("Number of premises vaccinated maps generated.")}
    }
  }
}

#==============================================================================================
#  Number of Animals Infected (Combines summary and Detail Files)      
# End result is one line per rep, so treat like a summary metric. 
#==============================================================================================

if(animalsInfected == TRUE){
  # setwd(path0)
  # ## Find all the detail files in the Files_To_Process" directory ##
  # summary.files <- list.files(path = pathfiles, recursive = TRUE, pattern = "_summary.txt", full.names = FALSE)
  # # Read through each controlSummary file 
  # # Filter summary files
  # summary.files <- summary.files[grep("cull_vax_0_-1_earliest_earliest|noControl", summary.files)]
  # summary.files <- summary.files[grep("newPremReportsOverX|percentIncrease|noControl", summary.files)]
  # 
  # detail.fnames <- list.files(path = pathfiles, recursive = TRUE, pattern = "_detail.txt", full.names = FALSE)
  # # Read through each controlSummary file 
  # # Filter summary files
  # detail.fnames <- detail.fnames[grep("cull_vax_0_-1_earliest_earliest|noControl", detail.fnames)]
  # detail.fnames <- detail.fnames[grep("newPremReportsOverX|percentIncrease|noControl", detail.fnames)]
  # source(paste0(source_files,"animalsInfected.R"))
}

#==============================================================================================
#  Type of spread aka proportion local transmission (Detail Files)  
#==============================================================================================

if (localSpread == TRUE){
  if (verbose > 0) {print("Importing local spread function...")}
  detail.fnames <- list.files(path = pathfiles, recursive = TRUE, pattern = "_detail.txt", full.names = FALSE)
  detail.fnames <- detail.fnames[grep("cull_vax_0_-1_earliest_earliest|noControl", detail.fnames)]
  detail.fnames <- detail.fnames[grep("newPremReportsOverX|percentIncrease|noControl", detail.fnames)]
  
  .localSpread(pathfiles = pathfiles, path0 = path0, path_output = plot_output,
               detail.fnames = detail.fnames, data_output = data_output, export.datafiles = export.datafiles, 
               run.types = run.types, runs_per_ctrl_type = runs_per_ctrl_type)
  
  if (verbose > 0) {print("Local spread calculations complete.")}
}

#==============================================================================================
# Summary table  
#==============================================================================================
if (summaryTable == TRUE){
  if (verbose > 0) {print("Generating summary table")}
  
  num.metrics = sum(exists("Anim_Median_RepType"),exists("Dur_Median_RepType"),exists("EpidExt_Median_RepType"),exists("PremInf_Median_RepType"))
  
  metrics = NULL
  if(exists("Dur_Median_RepType")) {metrics <- c(metrics,"Dur_Median_RepType","Dur_Upper_RepType")}
  if(exists("PremInf_Median_RepType")) {metrics <- c(metrics,"PremInf_Median_RepType","PremInf_Upper_RepType")}
  if(exists("Anim_Median_RepType")) {metrics <- c(metrics,"Anim_Median_RepType","Anim_Upper_RepType")}
  if(exists("EpidExt_Median_RepType")) {metrics <- c(metrics,"EpidExt_Median_RepType","EpidExt_Upper_RepType")}
  
  metric.alias = NULL
  if(exists("Dur_Median_RepType")) {metric.alias <- c(metric.alias,"Duration")}
  if(exists("PremInf_Median_RepType")) {metric.alias <- c(metric.alias,"Premises Infected")}
  if(exists("Anim_Median_RepType")) {metric.alias <- c(metric.alias,"Infected Animals")}
  if(exists("EpidExt_Median_RepType")) {metric.alias <- c(metric.alias,"Epidemic Extent (counties)")}
  
  sumtab=NULL
  
  for(i in 1:length(metrics)){
    m <-get(metrics[i])
    sumtab=cbind(sumtab,m) 
  }
  
  colnames(sumtab) <- rep(c("Median", "Upper 2.5%"),num.metrics)
  rownames(sumtab) <- run.types
  
  aboveheader = c( 1 , rep(2, times = length(metric.alias)))
  names(aboveheader ) = c( "" , metric.alias )
  
  setwd(path)
  save_kable(x= kable(sumtab, "html") %>%
               kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"), full_width = F, position = "float_right") %>%
               add_header_above(aboveheader),
             file=paste0("Summary_Table_",format(Sys.time(),'%Y%m%d_%H%M'),".jpeg"))
  
  if (verbose > 0) {print("Summary table complete")}
}

if (export.datafiles > 0) {
  setwd(data_output)
  save(list = ls(all.names = TRUE), file = "postProcessing_USDOSv2.1_Development.RData")
  save(run.types,runs_per_ctrl_type,file = "run_information.RData")
  print("Saving information from postProcessing environment for development")
} 

}
