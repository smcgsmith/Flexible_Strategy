#### USDOS Pre-Processing Function 

#####################################################################################################################################
## Working directory must contain "templates" folder containing the SH_FILE_TEMPLATE.txt as well as the template to be used for the 
##   config files. It also must either be or contain a destination folder for the files that will be generated. 

#####################################################################################################################################
## Make a dataframe with all of the appropriate information
##   The first column must contain the filenames of the configs that will be generated
##   The second column must contain the jobnames that will be displayed as each file is running
##   All other columns must contain the information to be inserted into the config files, with the column headers matching the
##     appropriate line number in the config file.

#' USDOS: Generate config files
#' 
#' A streamlined method for producing large numbers of USDOS config files and changing parameters. For additional details, refer to the USDOS User Manual. 
#' 
#' @param run.control The initial parameter to assign the control to be implemented. This value changes the default values for specific additional parameters. The default value is 'noControl', indicating no controls will be implemented. Other options include "MB" (movement ban), 'MB_IPcull' (movement ban + infected premises (IP) culling), 'MB_IPDCcull (movement ban  + culling of IPs and dangerous contacts (DCs))', 'MB_cullVax' (movement ban, IP culling, and vaccination either of DCs or in a ring), 'IPcull' (IP culling), and "other". For "other" runs, the user must specify all control-related inputs manually. 
#' @param parameter.sample Used only for sensitivity runs. Options are "spread" or "control". "spread" is used to provide variation in the tranmissibility constant (transm.const), K2, K3, latency, and infectious parameters, using values provided in a supplemental file called "LHS_Parameters.csv" in the inputfiles folder. "control" is used to provide variation in cull.rate, vax.rate, effective.vax, index.rep.time, rep.time, DC.rep.time, DC.sus, and DC.exp parameters, using values provided in a supplemental file called "LHS_Control_Parameters_040119.csv" in the inputfiles folder. The file must contain a number of rows equal to the total number of simulations being generated. Default is NA (a non-sensitivity run).
#' @param sens.number Used only for sensitivity runs.The row number in the sensitivity parameter file to use.
#' @param run.diagnostic The initial parameter to assign the diagnostics to be implemented. This value changes the default values for specific additional parameters. The default is "noDiagnostics". Other options are "Suspect_elisa" (test suspected premises (SP) with ELISA. Default parameters are for an FMD ELISA), "Suspect_pcr" (test suspected premises (SP) with PCR Default parameters are for an FMD PCR), "Suspect_atRisk_elisa_pcr" (test SPs with ELISA and premises identified as at risk with PCR), "atRisk_elisa" (test at-risk premises with ELISA), "atRisk_pcr" (test at-risk premises with PCR), "Suspect_lowSens" (test SPs with a low-sensitivity diagnostic series (parameterized by default for bTB)), "Suspect_highSens" (test SPs with a diagnostic test series with perfect sensitivity), "SP_atRisk_highSens_lowSens" (test SPs with a perfect diagnostic series and at-risk premises with a lower-sensitivity series) and "other". For "other" runs, the user must specify all diagnostic-related inputs manually. 
#' @param batch.name (config line 1) Batch name, used as prefix for output files. Only letters, numbers and underscore. If not specified, a name will be generated based on run options. 
#' @param summary (2) Generate a file with summary information about each seeded outbreak (0 = no, 1 = yes (default))
#' @param detail (3) Generate a detail file withinformation on each exposure event (0 = no, 1 = yes (default))
#' @param print.grid (4) Print grid cells on/off (0 = no (default), 1 = yes )
#' @param print.control (6) Print control summary (0=off, 1=premises level)
#' @param flaps (11) The subname of the FLAPS version being used. For example, 'flaps12_min' will use the files with a naming pattern 'flaps12_min_00XX.txt'
#' @param species (12) List of species for which counts are provided in premises file, comma-separated. Default is "beef,dairy".
#' @param timesteps (13) The number of timesteps to run for each seeded outbreak. The units are determined by disease:  days for foot and mouth disease (FMD) or months for bovine tuberculosis (bTB).
#' @param cutoff (14) The maximum number of infected premises a seeded outbreak may generate. If this is reached, the run will be terminated. '*' indicates no limit (default).
#' @param verbose (15) Output extra information to console as model runs: 0 = off, 1 = basic steps (default), 2 = detailed information for debugging
#' @param xy (17) Reverse x/y option: if input file is entered as lat/long (y/x) set to 1, and if long/lat (x/y) set to 0 (default)
#' @param fips.info (18) Name of file containing fips name, state name, area (m2), x, y. Tab separated. Default is "inputfiles/FIPS_updated_new.txt"
#' @param start.day (19) Day of the year to start the simulation (Jan 1 = day 1). Must be 0 - 365. 0 = random day [1-365] (default). For bTB, simulations are started at the beginning of the month that includes the start day.
#' @param disease (20) The type of disease to simulate: 0 = foot and mouth disease (FMD, default), 1 = bovine tuberculosis (bTB).
#' @param source (21) The source FIPS to be seeded. Either a filename or 'allFips' (default). 'allFips' will seed in all counties containing premises. Filename is of the tab-delimited file containing the FIPS codes or premisesIDs from which to seed infection, with one line per simulation. To seed from multiple premises at once, provide comma-separated premisesIDs in the file. 
#' @param filetype (22) The type of information provided in the file entered for "source". 'fips' (default) will choose 1 premises at random within the FIPS codes listed in the source parameter or in all FIPS, 'singlePremises' will use the premisesID provided by the source parameter, 'multiplePremises' will seed all comma separated premisesIDs in the source parameter.
#' @param market.within (23) Controls market behavior with regard to within-herd spread. For FMD, this is a real number probability (range [0,1], default = 1) that a shipment from an infected market transmits infection. For bTB, the two possible values are 0 (within-herd spread occurs at markets like other premises, default) and 1 (no within-herd spread at markets).
#' @param susc.exp.sp1 (24) Susceptibility exponent (power, q) for the first species listed on line 12 of the config file. Default species is beef, with power = 1. Only affects FMD runs. 
#' @param susc.exp.sp2 (24) Susceptibility exponent (power, q) for the second species listed on line 12 of the config file. Default species is dairy, with power = 1. Only affects FMD runs.  
#' @param transm.exp.sp1 (25) Transmissibility (infectiousness) exponent (power, p) for the first species listed on line 12 of the config file. Default species is beef, with power = 1. Only affects FMD runs.  
#' @param transm.exp.sp2 (25) Transmissibility (infectiousness) exponent (power, p) for the second species listed on line 12 of the config file. Default species is dairy, with power = 1. Only affects FMD runs.  
#' @param susc.const.sp1 (26) Susceptibility constant for the first species listed on line 12 of the config file. Default species is beef, with constant = 1. Only affects FMD runs. 
#' @param susc.const.sp2 (26) Susceptibility constant for the second species listed on line 12 of the config file. Default species is dairy, with constant = 1. Only affects FMD runs. 
#' @param transm.const.sp1 (27) Transmissibility (infectiousness) constant for the first species listed on line 12 of the config file. Default species is beef, with constant = 10.252. Only affects FMD runs.  
#' @param transm.const.sp2 (27) Transmissibility (infectiousness) constant for the second species listed on line 12 of the config file. Default species is dairy,  with constant = 10.252. Only affects FMD runs.  
#' @param kernel.type (28) Kernel type for local (diffusion) spread: 0: k1/(1 + (distance/k2)^k3), 1: data file provided with data.kernel.file , 2: k1/(1+d/k2)^k3. Only affects FMD runs. 
#' @param k1 (29) Kernel parameter 1. Default is 1.46e-08. Only affects FMD runs. 
#' @param k2 (29) Kernel parameter 2. Default is 1686.155. Only affects FMD runs. 
#' @param k3 (29) Kernel parameter 3. Default is 2.267. Only affects FMD runs. 
#' @param data.kernel.file (30) Name of file containing data-based local spread probabilities by distance, ex inputfiles/UKDataKernel.txt. Only affects FMD runs. 
#' @param latency (31) The mean days from premises exposure to infectiousness. Default is 5 days. Only affects FMD runs. 
#' @param latency.sd (31) The standard deviation days from premises exposure to infectiousness. Default is 0 days. Only affects FMD runs. 
#' @param infectious (32) The mean days from premises infectiousness to immunity. Default is 20 days. Only affects FMD runs. 
#' @param infectious.sd (32) The standard deviation days from premises infectiousness to immunity. Default is 0 days. Only affects FMD runs. 
#' @param partial (33) The partial transition flag (0 = off, 1 = on (default)). Only affects FMD runs. 
#' @param partial.param (34) Partial transition parameters (of which there are 5). Either enter all 5 (comma separated) or "*" if not using partial transition. Only affects FMD runs. 
#' @param grid.file (36) Filename containing grid cells (will override other grid-related options)
#' @param grid.side (37) Length of cell side for uniform cells (will override grid density options grid.max.farms and grid.min.side)
#' @param grid.max.farms (38) Max farms per grid cell (default 500)
#' @param grid.min.side (38) Grid cell size minimum side length in meters (default 100,000)
#' @param shipment.gen (41) Method to generate county-to-county shipments with USAMM, comma-separated (0: shipments off; 1: USAMMv1; 2: USAMMv3 (default); 3: USAMMv2 kernel 1; 4: USAMMv2 kernel 2; 5: USAMMv2 kernel 3)
#' @param shipment.scale (42) Shipment scaling factor (default 1)
#' @param usamm.posteriors (44) USAMM posterior files. Must match method selected in shipment.gen. Comma-separated, one file for each species. Default is inputfiles/USAMMv3_beef_final.posterior, inputfiles/USAMMv3_dairy_final.posterior.
#' @param usamm.order (45) The order in which the temporal switching of USAMM parameters should happen. Comma separated, time periods exactly as the temporal component of the USAMM parameter names in the usamm.posterior file(s). Default is Q1,Q2,Q3,Q4.
#' @param usamm.day (46) Day of the year (Jan 1 = day 1) where each time period in usamm.order begins. Comma separated integers. Assume no leap years. Default is 1,91,182,274.
#' @param usamm.origin.cov (47) Origin covariates for all counties. One file for each species, comma separated. Must have header: FIPS, name1, name2... Names must match name component of covariate parameters in USAMM parameter file. Default is inputfiles/county_covs_mean_zero.txt, inputfiles/county_covs_mean_zero.txt.
#' @param usamm.dest.cov (48) Destination covariates for all counties. One file for each species, comma separated. Must have header: FIPS, name1, name2... Names must match name component of covariate parameters in USAMM parameter file. Default is inputfiles/county_covs_mean_zero.txt, inputfiles/county_covs_mean_zero.txt.
#' @param exp.ship (50) Exposed shipments: shipping from a farm with status exposed will cause the receiver to become exposed as well (0 = off, 1 = on (default)). Affects FMD runs only.
#' @param ctrl.type (51) The control type(s) implemented. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param ctrl.constraint.type (52) Control constraint functions. Options are noLimit, dailyLimit, stateSum (cull only), and nationalLimit (vax only). Set by default with run.control.
#' @param ctrl.constraint (53) Control constraint parameters, comma-separated parameters, SEMICOLON-separated by type. Movement bans have a single value (which is 0 by default), whereas other controls require two values: a mean and standard deviation. Set by default with run.control. Required when run.control = "other".
#' @param cull.rate (53) The mean number of animals per timestep that can be culled on a premises. Set by default with run.control. Required when run.control = "other".
#' @param cull.rate.sd (53) The standard deviation number of animals per timestep that can be culled on a premises. Set by default with run.control. Required when run.control = "other".
#' @param vax.rate (53) The mean number of animals per timestep that can be vaccinated on a premises. Set by default with run.control. Required when run.control = "other".
#' @param vax.rate.sd (53) The standard deviation number of animals per timestep that can be vaccinated on a premises. Set by default with run.control. Required when run.control = "other".
#' @param ctrl.scale (54) Spatial scale at which control is applied, comma-separated (fixed options: premises, county (shipBan only), state (shipBan only)). Set by default with run.control. Required when run.control = "other".
#' @param ctrl.constraint.files (55) List of files containing control constraints. Only used when run.control= "other". 
#' @param landfill.files (55) Set to TRUE if there is a unique landfill file for each FLAPS file (default). If landfill files are not named "landfills_flaps12_{flaps number}.txt" (e.g. 'landfills_flaps12_0001.txt'), specify naming convention with landfill.file.name. Otherwise, set to FALSE and the generic 'inputfiles/landfills_formatted.txt' file will be used. Set by default with run.control. 
#' @param landfill.file.name (55) Specify prefix for the landfill filenames (i.e. the part before "_{FLAPS number}.txt"). By default, "landfills_flaps12". 
#' @param vaccine.file (55) Set to the filename if there is a vaccination constraint file. The default for when vaccination is on will be a vaccination file named "vaccineBankUpdated.txt" in the inputfiles folder. Otherwise, default is "NA" for no vaccination file.
#' @param ctrl.constraint.filetypes (56) List of filetypes for control constraints listed in ctrl.constraint.files. Only used when run.control= "other"
#' @param effective.mean (57) Implemented to effective: mean number of timesteps, comma-separated for each control type. Only used for run.control ="other"
#' @param effective.mb (57) Implemented to effective (mean number of timesteps) for a movement ban. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param effective.cull (57) Implemented to effective (mean number of timesteps) for culling. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param effective.vax (57) Implemented to effective (mean number of timesteps) for vaccination. The timesteps between vaccination and vaccine effectiveness. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param effective.sd (58) Implemented to effective: standard deviation number of timesteps, comma-separated for each control type. Only used for run.control ="other"
#' @param effective.mb.sd (58) Implemented to effective (standard deviation number of timesteps) for a movement ban. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param effective.cull.sd (58) Implemented to effective (standard deviation number of timesteps) for culling. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param effective.vax.sd (58) Implemented to effective (standard deviation number of timesteps) for vaccination. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param inactive.mean (59) Effective to inactive: mean number of timesteps, comma-separated for each control type. Only used for run.control ="other".
#' @param inactive.mb (59) Effective to inactive (mean number of timesteps) for a movement ban. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param inactive.cull (59) Effective to inactive (mean number of timesteps) for culling. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param inactive.vax (59) Effective to inactive (mean number of timesteps) for vaccination. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param inactive.sd (60) Effective to inactive: standard deviation number of timesteps, comma-separated for each control type. Only used for run.control ="other".
#' @param inactive.mb.sd (60) Effective to inactive (standard deviation number of timesteps) for a movement ban, Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param inactive.cull.sd (60) Effective to inactive (standard deviation number of timesteps) for culling. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param inactive.vax.sd (60) Effective to inactive (standard deviation number of timesteps) for vaccination. Set by default with run.control, but can be specified using this option when run.control = "other". 
#' @param ctrl.eff (61) Control effectiveness (including compliance) of control types as proportion, comma-separated probability of preventing exposure given exposure and probability of transmission given infectiousness. SEMICOLON-separated by type. Only used for run.control="other"
#' @param mb.eff (61) The effectiveness of the movement ban put into place on shipments. The value is between 0 and 1. Set by default with run.control.
#' @param cull.eff (61) The effectiveness of culling. The value is between 0 and 1. Set by default with run.control. 
#' @param vax.eff (61) The effectiveness of the vaccination. The value is between 0 and 1. Set by default with run.control.
#' @param flex.file (62) Specify "flex" in this line if you would like control actions to be imported via an input file.
#' @param ctrl.triggers (63) Triggers for control implementation, comma-separated (fixed options: newRegionReportsOverX,newPremReportsOverX). '*' will turn off control. Set by default with run.control, required for run.control = "other".
#' @param ctrl.trigger.threshold (64) Control trigger thresholds, comma-separated (numeric). How many of "ctrl.triggers" must occur for control to be implemented? Set by default with run.control, required for run.control = "other".
#' @param ctrl.trigger.response (65) What measures are taken in response to meeting the control trigger threshold? Must exist in ctrl.type. Comma-separated. Set by default with run.control, required for run.control = "other".
#' @param ctrl.response.target (66) The response targets, comma-separated. -1 = DCs, 0 = triggers only, # = radius in units of x/y coordinates of premises. Only used if run.control="other"
#' @param cull.range (66) The range of the cull ring. The default value is '-1', indicating that the cull is applied to dangerous contacts, and not in a ring. A positive number indicates a ring vaccination, in meters. This is only in effect if a vaccination control type is selected.
#' @param vax.range (66) The range of the vaccination ring. The default value is '-1', indicating that the vaccination is applied to dangerous contacts, and not in a ring. A positive number indicates a ring vaccination, in meters. This is only in effect if a vaccination control type is selected.
#' @param ctrl.response.priority (67) Method for prioritizing premises for control. Currently options of "earliest", "closest" (for ring cull/vax only), "farthest" (for ring cull/vax only),"largest", "smallest" for all controls. 
#' @param flex.file.name (68) List the flexible control file location (likely: inputfiles/flex.txt) if "flex" specified on line 62. Control actions included in this file should be listed on line 51 as well. This means that constraints must also be included accordingly.   
#' @param index.rep.time (71) The mean timesteps from index premises exposure to reporting. Default is 15 (FMD) or 1000 (bTB).
#' @param index.rep.time.sd (71) The standard deviation timesteps from index premises exposure to reporting.  Default is 0 for both diseases.
#' @param rep.time (72) The mean timesteps from non-dangerous contact premises exposure to reporting. Default is 8 (FMD) or 1000 (bTB).
#' @param rep.time.sd (72) The standard deviation timesteps from non-dangerous contact premises exposure to reporting. Default is 0 for both diseases.  
#' @param DC.rep.time (73) The mean timesteps from dangerous contact premises exposure to reporting. Default is 2 (FMD) or 1000 (bTB).
#' @param DC.rep.time.sd (73) The standard deviation timesteps from dangerous contact premises exposure to reporting. Default is 0 for both diseases.
#' @param DC.sus (74) The scale for tracing DCs of a susceptible premises. The default value is 4 to make the mean DCs per reported premises between 1.5-2 when combined with the exposed value.
#' @param DC.exp (74) The scale for tracing DCs of a exposed premises. The default value is 5 to make the mean DCs per reported premises between 1.5-2 when combined with the susceptible value.
#' @param test.suspects (78) Are suspected premises given diagnostic testing before they are reported? 1= yes, "*" = no. Set by default with run.control, required for run.control = "other".
#' @param diag.names (79) Names for unique diagnostic types, comma-separated (fixed options: pcr, elisa, penSide, series, diva (FMD only)) '*' will turn off diagnostics. Set by default with run.control, required for run.control = "other".
#' @param diag.constraint (80) Diagnostic constraint function types, comma-separated, options: diagnosticDailyLimit or noLimit. Set by default with run.control, required for run.control = "other".
#' @param diag.constraint.params (81) Diagnostic constraint parameters, comma-separated parameters, SEMICOLON-separated by type. Set by default with run.control, required for run.control = "other".
#' @param diag.scale (82) Spatial scale at which diagnostics are applied, comma-separated (fixed options: premises). Set by default with run.control, required for run.control = "other".
#' @param diag.constraint.files (83) List of additional files for diagnostic constraints, default is NA and is not currently active
#' @param diag.constraint.file.types (84) File types for diagnostic constraint files in (83) default is NA and is not currently active
#' @param diag.test.lag.mean (85) Test start to complete lag: mean number of timesteps, comma-separated for each diagnostic type. Set by default with run.control, required for run.control = "other".
#' @param diag.test.lag.sd (86) Test start to complete lag: standard deviation number of timesteps, comma-separated for each diagnostic type. Set by default with run.control, required for run.control = "other".
#' @param diag.test.alpha (87) Alpha value of each diagnostic test sensitivity beta distribution, comma separated. Set by default with run.control, required for run.control = "other".
#' @param diag.test.beta (88) Beta value of each diagnostic test sensitivity beta distribution, comma separated. Set by default with run.control, required for run.control = "other".
#' @param diag.trigger (91) Diagnostic triggers, comma-separated (fixed options). '*' will turn off diagnostics. Options are newPremReportsOverX or newPremSuspectsOverX (if test.suspects is 1). Set by default with run.control, required for run.control = "other".
#' @param diag.trigger.threshold (92) Diagnostic trigger thresholds, comma-separated (numeric). Set by default with run.control, required for run.control = "other".
#' @param diag.trigger.response (93) Diagnostic trigger responses, comma-separated (must exist in diag.names). Set by default with run.control, required for run.control = "other".
#' @param diag.response.targets (94) Diagnostic response targets, comma-separated. -1 = DCs, 0 = triggers only, # = radius in units of x/y coordinates of premises. Set by default with run.control, required for run.control = "other".
#' @param diag.response.priority (95) Method for prioritizing premises for diagnostics. Currently a fixed option of "earliest" for all diagnostics. Set by default with run.control, required for run.control = "other". 
#' @param index.investigate.delay (100) Mean days from index premises exposure to investigate, FMD-like infection only and only when IPs are a target of diagnostics (line 91 is set to 0). Default is 15, except when IPs are not targets and the default is 366. 
#' @param index.investigate.delay.sd (100) Standard deviation days from index premises exposure to investigate, FMD-like infection only and only when IPs are a target of diagnostics. Default is 0. 
#' @param nonindex.investigate.delay (101) Mean days from non-index premises exposure to investigate, FMD-like infection only and only when IPs are a target of diagnostics. Default is 8.
#' @param nonindex.investigate.delay.sd (101) Standard deviation days from non-index premises exposure to investigate, FMD-like infection only and only when IPs are a target of diagnostics. Default is 0. 
#' @param dc.investigate.delay (101) Mean days from dangerous contact premises exposure to investigate, FMD-like infection only and only when IPs are a target of diagnostics. Default is 2.
#' @param dc.investigate.delay.sd (102) Standard deviation days from dangerous contact premises exposure to investigate, FMD-like infection only and only when IPs are a target of diagnostics. Default is 0. 
#' @param slaughtershed.file (110) Slaughtershed matrix file detailing the probability of sending to various facilities based on origin county as well as facility-specific parameters for lesion detection. Requires header. Columns are slaughter plant ID (1), alpha (2) and beta (3) for the distribution for P(Lesion submitted|Lesion of detectable size) and 3062 counties. Only used for bTB. 
#' @param props.to.slaughter (111) List of proportions of shipments that go to slaughter and proportions of inventory that is send to slaughter each year for different premises types. See manual for detailed format. Only used for bTB. 
#' @param tb.local.kernel (122) Local kernel component quarterly parameters, parameters separated by comma, quarters separated by semicolon (e.g. [p1q1, p2q1; p1q2, p2q2; ...], make sure any parameter with spatial units are expresed in meters). Only used for bTB. 
#' @param tb.wildlife.kernel (123) Wildlife kernel component quarterly parameters, parameters separated by comma, quarters separated by semicolon (e.g. [p1q1, p2q1; p1q2, p2q2; ...], make sure any parameter with spatial units are expresed in meters). Only used for bTB. 
#' @param tb.wildlife.weight (124) Relative weight given to the wildlife kernel component compared to the local kernel component. Only used for bTB. 
#' @param wildlife.file (129) Wildlife density file. Two tab-separated columns with header = { County_FIPS Density }. Only used for bTB. 
#' @param dairy.birth (132) Dairy birth rate in the bTB within-herd spread model.  
#' @param mortality.rate (133) Mortality rate in the bTB within-herd spread model.  for each species, comma separated
#' @param import.rate (134) Import rate in the bTB within-herd spread model.  for each species, comma separated
#' @param transm.rate (135) Transmission rate in the bTB within-herd spread model.  
#' @param contact.rate (136) Contact rate within cattle in the bTB within-herd spread model.  
#' @param gamma.e1 (138) Mean and rate of gamma distributed transition rate from class E1 to E2U or E2R (comma-separated) in the bTB within-herd spread model.    
#' @param gamma.e2r (139) Mean and rate of gamma distributed transition rate from class E2R to IR (comma-separated) in the bTB within-herd spread model.  
#' @param adjust.e2u.trans (140) Adjustment factor of how many times longer it takes to transition from E2U compared to the time it takes to transition from E2R in the bTB within-herd spread model.  
#' @param adjust.e2u.react (141) Adjustment factor of how many times longer it takes for an animal in E2U to become reactive compared to the time it takes to transition from E1 in the bTB within-herd spread model.
#' @param adjust.iu.react (142) Adjustment factor of how many times longer it takes for an animal in IU to become reactive compared to the time it takes to transition from E2R in the bTB within-herd spread model.  
#' @param detect.exp (143) Adjustment factor of the probability of successfully detecting an exposed animal that mounts an immune response (leaves E1U) when testing (goes into E2R) in the bTB within-herd spread model.  
#' @param detect.exp.nonreact (144) Adjustment factor of the probability of successfully detecting an unreactive exposed animal with immune response (E2U), meaning it moves into E2R in the bTB within-herd spread model.  .
#' @param detect.inf.nonreact (145) Adjustment factor of the probability of successfully detecting an infectious animal that is not reactive to testing (IU, goes into IR) in the bTB within-herd spread model.  .
#' @param test.sens (146) Test sensitivity in the bTB within-herd spread model.  
#' @param template.config.location The location of the template for the config file. Default is "templates/USDOS_CONFIG_TEMPLATE_General.txt".
#' @param sh.template The destination of the SH_FILE_TEMPLATE.txt file. Default is 'templates/SH_FILE_TEMPLATE.txt'.
#' @param jobfile.name The name of the jobfile to output. Default is 'USDOS_Sim.job'. 
#' @param destination.folder The location to put all generated files. Folder must exist. Default is the current working directory.
#' @param replacement.df The dataframe containing all information to use to modify the default config file. This is created with the time, date, and replacement_df as its title.
#' @param slurm If running on a SLURM system, set option to 1 to generate BATCH files and a jobfile that lists BATCH file names. Setting to 0 (default) indicates files will not be used on a SLURM system. BATCH files will not be genreated and the jobfile will reference config file names. 
#' @param reps The number of times each FLAPS will be repeated. The default value is 10, for a total of 100 runs. This is the minimum number of runs that should be interpreted to adequately capture uncertainty. 

#' @return This code will generate configuration (config) files and other necessary files to run USDOS (job and BATCH files), as well as a csv of the replacement data frame with the options used to fill in the configs.
#' @author Send bug reports, suggestions, corrections, or comments to Katie Owers.

#' @export

#
######################################################
## Start Function ##

createConfigs <- function(run.control = "noControl", run.diagnostic = "noDiagnostics", disease = 0, flex.file.name = NA,...){ 
  options(scipen=3)  
    
  ### Set default values for config files: 
  
  # Inputs
  template.config.location <- "templates/USDOS_CONFIG_TEMPLATE_General.txt"
  sh.template <- "templates/SH_FILE_TEMPLATE.txt"
  jobfile.name <- "USDOS_Sim.job"
  batch.name <- ""  
  slurm <- 0
  
  # Outputs
  destination.folder <- getwd()
  replacement.df <- data.frame()
  
  # General
  summary <- 1
  detail <- 1
  print.grid <- 0
  print.control <- 0
  
  flaps <- "FLAPS12_Quarterly_USDOS_format"
  species <- "beef,dairy"
  
  verbose <- 1
  reps <- 10
  cutoff <- "*"  
  
  fips.info <- "inputfiles/FIPS_updated_new.txt"  
  xy <- 0
  start.day <- 0
  
  source <- "allFips"
  filetype <- "fips"
  
  kernel.type <- 0
  data.kernel.file <- "*"
  
  landfill.files <- TRUE
  landfill.file.name <- "landfills_flaps12"
  
  # grid
  grid.file <- "*"
  grid.side <- "*"
  grid.max.farms <-500
  grid.min.side <- 100000
  
  # Shipment
  shipment.gen <- 2
  shipment.scale <- 1
  usamm.posteriors <- "inputfiles/USAMMv3_beef_final.posterior, inputfiles/USAMMv3_dairy_final.posterior"
  usamm.order <- "Q1,Q2,Q3,Q4"
  usamm.day <- "1,91,182,274"
  usamm.origin.cov <- "inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt"
  usamm.dest.cov <-   "inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt"
  
  exp.ship <-1 # only affects FMD
  
  # Sensitivity 
  parameter.sample <- "NA"
  sens.number <- NA
  
  # Disease settings. Note: Many settings are only used by one disease, so we can set defaults regardless of the disease ####
  DC.sus <- 4
  DC.exp <- 5  
  index.rep.time.sd <- 0
  rep.time.sd <- 0
  DC.rep.time.sd <- 0
  cull.rate.sd <-0
  
  # Settings that only affect FMD runs
   
        k1 <- 1.46e-08
        k2 <- 1686.155
        k3 <- 2.267
        latency <- 5
        latency.sd <- 0
        infectious <- 20 
        infectious.sd <- 0
        susc.exp.sp1 <- 1
        susc.exp.sp2 <- 1
        transm.exp.sp1 <- 1
        transm.exp.sp2 <- 1
        susc.const.sp1 <- 1
        susc.const.sp2 <- 1
        transm.const.sp1 <- 10.252
        transm.const.sp2 <- 10.252
        
        # Partial Transition
        partial <- 1
        partial.param <- "0.05,0.006,0.44,4,6.30852"
   
        index.investigate.delay <- 15
        nonindex.investigate.delay <- 8        
        dc.investigate.delay <- 2        
        index.investigate.delay.sd <- 0
        nonindex.investigate.delay.sd <- 0
        dc.investigate.delay.sd <- 0
    
    # Settings that only affect bTB runs 
        
        slaughtershed.file <- "inputfiles/Slaughtershed_and_SlaughterSubmissionRate_USDOS_format.txt"
        props.to.slaughter <- "inputfiles/ProportionstoSlaughter_USDOS_format.txt"
        
        tb.local.kernel <- "0.05, 0.00146; 0.05, 0.00146; 0.05, 0.00146; 0.05, 0.00146"
        tb.wildlife.kernel <- "73.39, 280.60; 67.21, 233.25; 70.03, 223.69; 112.13, 307.68"
        tb.wildlife.weight <- 0.5
        wildlife.file <- "inputfiles/white_tailed_deer_density.txt"
        
  
  # Settings that need to be disease-specific
  if(disease == 0){ # FMD
    timesteps <- 365
    market.within <- 1
    index.rep.time <- 15
    rep.time <- 8
    DC.rep.time <- 2
    cull.rate <- 240 } 
        
  if(disease == 1){ # btb
          timesteps <- 60
          market.within <- 0
          index.rep.time <- 1000
          rep.time <- 1000
          DC.rep.time <- 1000
          cull.rate <- 7200
      }
  

  ### Default control settings for a base (noControl) run
  ctrl.type <- "*"      
  mb.eff <- 0
  cull.eff <- 1
  ctrl.constraint.type <-  "noLimit"
  ctrl.constraint <- 0 
  ctrl.scale <- "state"
  ctrl.constraint.files <- NULL
  ctrl.constraint.filetypes <- NULL
  
  flex.file <- NA
  ctrl.triggers <- "*" 
  ctrl.trigger.threshold <- 0
  ctrl.trigger.response <-"*"
  ctrl.response.target <- 0
  ctrl.response.priority <- "earliest"
  
  effective.mean <-NULL
  effective.sd <- NULL
  effective.mb <- 0
  effective.mb.sd <- 0
  effective.cull <- 0
  effective.cull.sd <- 0
  effective.vax <- 11 
  effective.vax.sd <- 0
  
  inactive.mean <- NULL
  inactive.sd <- NULL
  inactive.mb <- 366
  inactive.mb.sd <- 0
  inactive.cull <- 366
  inactive.cull.sd <- 0
  inactive.vax <- 183
  inactive.vax.sd <- 0
  
  vax.rate <- NULL
  vax.rate.sd <- NULL
  vax.eff <- 0
  cull.range <- NULL
  vax.range <- NULL
  vaccine.file <- "NA"

  ### Default diagnostic settings for a base (noDiagnostics) run
  test.suspects <- "*"
  diag.names <- "*"
  diag.constraint <- NA
  diag.constraint.params <- NA
  diag.scale <- "premises"
  diag.constraint.files <- NA
  diag.constraint.file.types <- NA
  diag.test.lag.mean <-NA
  diag.test.lag.sd <- NA
  diag.test.alpha <- NA
  diag.test.beta <- NA
  diag.trigger <- NA
  diag.trigger.threshold <- 0
  diag.trigger.response <-NA
  diag.response.targets <- 0
  diag.response.priority <- "earliest"
  

  ## bTB within-herd model settings
  dairy.birth <- 0.05
  mortality.rate <- "0.02, 0.05"
  import.rate <- "0.49, 0.09"
  transm.rate <- "3.0"
  contact.rate <- "1.0"
  gamma.e1 <- "9.75, 0.21"
  gamma.e2r <- "1.6, 1.85"
  adjust.e2u.trans <- 0.5
  adjust.e2u.react <- 1
  adjust.iu.react <- 1
  detect.exp <- 0.9
  detect.exp.nonreact <- 0.5
  detect.inf.nonreact <- 0.5
  test.sens <- 0.84
  
  

################################################################################################################
  
  ### Set the control type with if statements. ####
  
  # Movement ban only 
  if(run.control == "MB"){
    ctrl.type <- "shipBan"
    ctrl.constraint.type <-  "noLimit"
    ctrl.scale <- "state"
    mb.eff <- 0.75 
    ctrl.triggers <- "newRegionReportsOverX"
    ctrl.trigger.threshold <- 0
    ctrl.trigger.response <- "shipBan"
   }
  

  if(run.control == "MB_IPcull"){
    ctrl.type <- "shipBan,cull"
    ctrl.constraint.type <-  "noLimit,stateSum"
    ctrl.scale <- "state,premises"
    mb.eff <- 0.75 
    cull.eff <- 1
    ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX"
    ctrl.trigger.threshold <- "0,0"
    ctrl.trigger.response <- "shipBan,cull"
  }

  
  if(run.control == "MB_IPDCcull"){
    ctrl.type <- "shipBan,cull"
    ctrl.constraint.type <-  "noLimit,stateSum"
    ctrl.scale <- "state,premises"
    mb.eff <- 0.75 
    cull.eff <- 1
    ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX,newPremReportsOverX"
    ctrl.trigger.threshold <- "0,0,0"
    ctrl.trigger.response <- "shipBan,cull,cull"
  }
  
  #if(run.control == "MB_cullVax"){
    # if(run.control == '3km_cullVax'){
    #   ctrl.type <- "shipBan,cull,cull,vax"
    #   ctrl.constraint.type <-  "noLimit,stateSum,stateSum,nationalLimit"
    #   vax.rate <- 6804 
    #   vax.rate.sd <- 0
    #   ctrl.scale <- "state,premises,premises,premises"
    #   mb.eff <- 0.75 
    #   cull.eff <- 1
    #   vax.eff <- 0.9
    #   cull.range <- c(0,3000) #default to IP cull
    #   vax.range <- 10000 # default to DC vaccination
    #   ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX,newPremReportsOverX,newPremReportsOverX"
    #   ctrl.trigger.threshold <- "0,0,0,0"
    #   ctrl.trigger.response <- "shipBan,cull,cull,vax"
    #   vaccine.file <-  "vaccineBankUpdated.txt"
    #   run.control <- "MB_cullVax"
    # } 
  
  # if(run.control == '10km_cull'){
  #     ctrl.type <- "shipBan,cull,cull"
  #     ctrl.constraint.type <-  "noLimit,stateSum,stateSum"
  #     ctrl.scale <- "state,premises,premises"
  #     mb.eff <- 0.75 
  #     cull.eff <- 1
  #     cull.range <- c(0,10000) #default to IP cull
  #     ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX,newPremReportsOverX"
  #     ctrl.trigger.threshold <- "0,0,0"
  #     ctrl.trigger.response <- "shipBan,cull,cull"
  #     run.control <- "MB_IPDCcull"
  #   }
  
  # if(run.control == '3kmCull_10kmVax'){
  #     ctrl.type <- "shipBan,cull,cull,vax"
  #     ctrl.constraint.type <-  "noLimit,stateSum,stateSum,nationalLimit"
  #     vax.rate <- 6804 
  #     vax.rate.sd <- 0
  #     ctrl.scale <- "state,premises,premises,premises"
  #     mb.eff <- 0.75 
  #     cull.eff <- 1
  #     vax.eff <- 0.9
  #     cull.range <- c(0,10000) #default to IP cull
  #     vax.range <- 10000 # default to DC vaccination
  #     ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX,newPremReportsOverX,newPremReportsOverX"
  #     ctrl.trigger.threshold <- "0,0,0,0"
  #     ctrl.trigger.response <- "shipBan,cull,cull,vax"
  #     vaccine.file <-  "vaccineBankUpdated.txt"
  #     run.control <- "MB_cullVax"
  #   } 
  
  if(run.control == 'MB_cullVax'){
      ctrl.type <- "shipBan,cull,vax"
      ctrl.constraint.type <-  "noLimit,stateSum,nationalLimit"
      vax.rate <- 6804 
      vax.rate.sd <- 0
      ctrl.scale <- "state,premises,premises"
      mb.eff <- 0.75 
      cull.eff <- 1
      vax.eff <- 0.9
      cull.range <- 0 #default to IP cull
      vax.range <- -1 # default to DC vaccination
      ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX,newPremReportsOverX"
      ctrl.trigger.threshold <- "0,0,0"
      ctrl.trigger.response <- "shipBan,cull,vax"
      vaccine.file <-  "vaccineBankUpdated.txt"
    }
  #}
  
  if(run.control == "IPcull"){
    ctrl.type <- "cull"
    ctrl.constraint.type <-  "stateSum"
    ctrl.scale <- "premises"
    cull.eff <- 1
    ctrl.triggers <- "newRegionReportsOverX"
    ctrl.trigger.threshold <- "0"
    ctrl.trigger.response <- "cull"
  }
  
  if(run.control == "flex_cull"){
    ctrl.type <- "cull"
    ctrl.constraint.type <-  "stateSum"
    ctrl.scale <- "premises"
    cull.eff <- 1
    flex.file <- "flex"
    ctrl.triggers <- "newRegionReportsOverX"
    ctrl.trigger.threshold <- "0"
    ctrl.trigger.response <- "cull"
    flex.file.name <- "inputfiles/flex.txt"
  }
  
  if(run.control == "flex_cullVax"){
    ctrl.type <- "cull,vax"
    ctrl.constraint.type <-  "stateSum,nationalLimit"
    vax.rate <- 6804 
    vax.rate.sd <- 0
    ctrl.scale <- "premises,premises"
    cull.eff <- 1
    vax.eff <- 0.9
    cull.range <- 0 #default to IP cull
    vax.range <- -1 # default to DC vaccination
    flex.file <- "flex"
    flex.file.name <- "inputfiles/flex.txt"
    ctrl.triggers <- "newPremReportsOverX,newPremReportsOverX"
    ctrl.trigger.threshold <- "0,0"
    ctrl.trigger.response <- "cull,vax"
    vaccine.file <-  "vaccineBankUpdated.txt"
  }
  
  if(run.control == "flex_MBcullVax"){
    ctrl.type <- "shipBan,cull,vax"
    ctrl.constraint.type <-  "noLimit,stateSum,nationalLimit"
    vax.rate <- 6804 
    vax.rate.sd <- 0
    ctrl.scale <- "state,premises,premises"
    mb.eff <- 0.75 
    cull.eff <- 1
    vax.eff <- 0.9
    cull.range <- 0 #default to IP cull
    vax.range <- -1 # default to DC vaccination
    flex.file <- "flex"
    flex.file.name <- "inputfiles/flex.txt"
    ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX,newPremReportsOverX"
    ctrl.trigger.threshold <- "0,0,0"
    ctrl.trigger.response <- "shipBan,cull,vax"
    vaccine.file <-  "vaccineBankUpdated.txt"
  }
  
  if(run.control == "flex_MBcull"){
    ctrl.type <- "shipBan,cull"
    ctrl.constraint.type <-  "noLimit,stateSum"
    ctrl.scale <- "state,premises"
    mb.eff <- 0.75 
    cull.eff <- 1
    flex.file <- "flex"
    flex.file.name <- "inputfiles/flex.txt"
    ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX"
    ctrl.trigger.threshold <- "0,0"
    ctrl.trigger.response <- "shipBan,cull"
  }
  
  if(run.control == "flex_MBvax"){
    ctrl.type <- "shipBan,vax"
    ctrl.constraint.type <-  "noLimit,nationalLimit"
    vax.rate <- 6804 
    vax.rate.sd <- 0
    ctrl.scale <- "state,premises"
    cull.eff <- 1
    vax.eff <- 0.9
    cull.range <- 0 #default to IP cull
    vax.range <- -1 # default to DC vaccination
    flex.file <- "flex"
    flex.file.name <- "inputfiles/flex.txt"
    ctrl.triggers <- "newRegionReportsOverX,newPremReportsOverX"
    ctrl.trigger.threshold <- "0,0"
    ctrl.trigger.response <- "shipBan,vax"
    vaccine.file <-  "vaccineBankUpdated.txt"
  }
  
  
  if(run.control == "flex"){
    #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    #flex.file.name <- "inputfiles/flex_PI_Decrease.txt"
    flexFile <- read.table(flex.file.name, header = F)
    ctrl.type <- gsub(" ","",toString(unique(flexFile$V3)))
    ctrl.trigger.response <- gsub(" ","",toString(unique(flexFile$V3)))
    ctrl.response.priority <- gsub(" ","",toString(flexFile$V5[1:length(unique(flexFile$V3))]))
    ctrl.triggers <- gsub(" ","",toString(flexFile$V1[1:length(unique(flexFile$V3))]))
    ctrl.trigger.threshold <- gsub(" ","",toString(flexFile$V2[1:length(unique(flexFile$V3))]))
    flex.file <- "flex"
    #flex.file.name <- "inputfiles/flex.txt"
    ctrl.response.target <- gsub(" ","",toString(flexFile$V4[1:length(unique(flexFile$V3))]))
    mb.eff <- 0.75 
    
    if("cull" %in% unique(flexFile$V3)){
      cull.eff <- 1
        if(all(flexFile[flexFile$V3=="cull",]$V4>1)){
          cull.range <- unique(flexFile[flexFile$V3=="cull",]$V4)
        }else{
          cull.range <- 0 #default to IP cull
        }
      ctrl.constraint.type <-  "noLimit,stateSum"
      ctrl.scale <- "state,premises"
    }
    
    if("vax" %in% unique(flexFile$V3)){
      vaccine.file <-  "vaccineBankUpdated.txt"
      vax.rate <- 6804 
      vax.rate.sd <- 0
      vax.eff <- 0.9
      if(all(flexFile[flexFile$V3=="vax",]$V4>1)){
        vax.range <- unique(flexFile[flexFile$V3=="vax",]$V4)
      }else{
        vax.range <- -1 # default to DC vaccination
      }
      ctrl.constraint.type <-  "noLimit,nationalLimit"
      ctrl.scale <- "state,premises"
    }
    
    if("vax" %in% (flexFile$V3) & "cull" %in% unique(flexFile$V3)){
      ctrl.constraint.type <-  "noLimit,stateSum,nationalLimit"
      ctrl.scale <- "state,premises,premises"
    }
    
    # if(length(flexFile$V3[flexFile$V3== "cull"]) == 2){
    #   ctrl.constraint.type <-  "noLimit,stateSum,stateSum"
    #   ctrl.scale <- "state,premises,premises"
    # } 
    # 
    # if(length(flexFile$V3[flexFile$V3== "vax"]) == 2){
    #   ctrl.constraint.type <-  "noLimit,nationalLimit,nationalLimit"
    #   ctrl.scale <- "state,premises,premises"
    # }
    
    flexFile[] <- lapply(flexFile, as.character)
    if(dim(flexFile)[1]>3){
      flex.sens.control = sapply(flexFile[(dim(flexFile)[1]-1):dim(flexFile)[1],], as.character)
      flex.sens.control = toString(flex.sens.control)
      flex.control <- gsub(" ", "", gsub(",","_", flex.sens.control))
    }else{
      flex.sens.control = sapply(flexFile[2:dim(flexFile)[1],], as.character)
      flex.sens.control = toString(flex.sens.control)
      flex.control <- gsub(" ", "", gsub(",","_", flex.sens.control))
    }
  }
  
##########################################################
# Diagnostics types ####
   
  # Diagnostic patterns used for FMD 
  
  if(run.diagnostic == "Suspect_elisa"){
    test.suspects <- 1
    diag.names <- "elisa"
    diag.constraint <- "diagnosticDailyLimit"
    diag.constraint.params <- "1000,0"
    diag.test.lag.mean <- 5
    diag.test.lag.sd <- 0
    diag.test.alpha <- 240
    diag.test.beta <- 2
    diag.trigger <- "newPremSuspectsOverX"
    diag.trigger.response <- "elisa"
  } 
  
  if(run.diagnostic == "Suspect_pcr"){
    test.suspects <- 1
    diag.names <- "pcr"
    diag.constraint <- "diagnosticDailyLimit"
    diag.constraint.params <- "1000,0"
    diag.test.lag.mean <- 0
    diag.test.lag.sd <- 0
    diag.test.alpha <- 20
    diag.test.beta <- 4
    diag.trigger <- "newPremSuspectsOverX"
    diag.trigger.response <- "pcr"
  } 
  
  if(run.diagnostic == "Suspect_atRisk_elisa_pcr"){
    test.suspects <- 1
    diag.names <- "elisa,pcr"
    diag.constraint <- "diagnosticDailyLimit,diagnosticDailyLimit"
    diag.constraint.params <- "1000,0;1000,0"
    diag.test.lag.mean <- "5,0"
    diag.test.lag.sd <- "0,0"
    diag.test.alpha <- "240,20"
    diag.test.beta <- "2,4"
    diag.trigger <- "newPremSuspectsOverX,newPremReportsOverX "
    diag.trigger.threshold <- "0,0"
    diag.trigger.response <- "elisa,pcr"
    diag.response.targets <- "0,-1"
    diag.scale <- "premises,premises"
  	diag.constraint.files <- "NA;NA"
  	diag.constraint.file.types <- "NA;NA"
  }  
  
  if(run.diagnostic == "atRisk_elisa"){
    diag.names <- "elisa"
    diag.constraint <- "diagnosticDailyLimit"
    diag.constraint.params <- "1000,0"
    diag.test.lag.mean <- 5
    diag.test.lag.sd <- 0
    diag.test.alpha <- 240
    diag.test.beta <-2
    diag.trigger <- "newPremReportsOverX"
    diag.trigger.response <- "elisa"
    diag.response.targets <- "-1"
    DC.rep.time <- 366
  }  
  
  if(run.diagnostic == "atRisk_pcr"){
    diag.names <- "pcr"
    diag.constraint <- "diagnosticDailyLimit"
    diag.constraint.params <- "1000,0"
    diag.test.lag.mean <- 0
    diag.test.lag.sd <- 0
    diag.test.alpha <- 20
    diag.test.beta <- 4
    diag.trigger <- "newPremReportsOverX"
    diag.trigger.response <- "pcr"
    diag.response.targets <- "-1"
    DC.rep.time <- 366
  }  
  
 #### Diagnostics patterns used for bTB

  if(run.diagnostic == "Suspect_lowSens"){
    test.suspects <- 1
    diag.names <- "series"
    diag.constraint <- "noLimit"
    diag.constraint.params <- "0"
    diag.test.lag.mean <- 1
    diag.test.lag.sd <- 0
    diag.test.alpha <- 7.6
    diag.test.beta <- 4.9
    diag.trigger <- "newPremSuspectsOverX"
    diag.trigger.response <- "series"
    diag.response.targets <- 0
  } 
  
  if(run.diagnostic == "Suspect_highSens"){
    test.suspects <- 1
    diag.names <- "series"
    diag.constraint <- "noLimit"
    diag.constraint.params <- "0"
    diag.test.lag.mean <- 1
    diag.test.lag.sd <- 0
    diag.test.alpha <- 10000
    diag.test.beta <- 0.1
    diag.trigger <- "newPremSuspectsOverX"
    diag.trigger.response <- "series"
    diag.response.targets <- 0
  } 
  
  if(run.diagnostic == "SP_atRisk_highSens_lowSens"){
    test.suspects <- 1
    diag.names <- "series,series"
    diag.constraint <- "noLimit,noLimit"
    diag.constraint.params <- "0;0"
    diag.test.lag.mean <- "1,1"
    diag.test.lag.sd <- "0,0"
    diag.test.alpha <- "10000,7.6"
    diag.test.beta <- "0.1,4.9"
    diag.trigger.threshold <- "0,0"
    diag.response.targets <- "0,-1"
    diag.trigger <- "newPremSuspectsOverX,newPremReportsOverX"
    diag.trigger.response <- "series,series"
    diag.scale <- "premises,premises"
  	diag.constraint.files <- "NA;NA"
  	diag.constraint.file.types <- "NA;NA"}  
  
  
################################
## Input checks ####

  ## List of all allowed function inputs 
  additional.inputs = list(...)
  parameter.options = c("template.config.location", "sh.template", "jobfile.name", "summary","detail","print.grid","print.control","flaps", 
                        "species","xy","start.day","landfill.files", "landfill.file.name","vaccination.file",
                        "fips.info", "reps", "cutoff", "timesteps", "source", "filetype", "parameter.sample", 
                         "k1", "k2", "k3","susc.exp.sp1","susc.exp.sp2","transm.exp.sp1", "susc.const.sp1","susc.const.sp2","transm.const.sp1","transm.const.sp2", 
                        "transm.exp.sp2","kernel.type","data.kernel.file","latency",'latency.sd', "infectious", "infectious.sd",
                        "grid.file","grid.side","grid.max.farms","grid.min.size",
                        "shipment.gen","shipment.scale","usamm.posteriors","usamm.order","usamm.day","usamm.origin.cov","usamm.dest.cov",
                        "mb.eff", "cull.rate", "cull.rate.sd","vax.rate","vax.rate.sd", "ctrl_type", 
                        "ctrl.constraint.files","ctrl.constraint.filetypes", "cull.eff", "ctrl.eff", "flex.file",
                        "effective.mean", "effective.sd" , "effective.mb" , "effective.mb.sd" , "effective.cull" , "effective.cull.sd" , "effective.vax" ,
                        "effective.vax.sd","inactive.mean","inactive.sd","inactive.mb" , "inactive.mb.sd" , "inactive.cull" , "inactive.cull.sd" ,
                        "inactive.vax" ,"inactive.vax.sd","vax.eff", "cull.range","vax.range", "partial.param", "partial", "index.rep.time", "rep.time", 
                        "DC.rep.time","index.rep.time.sd", "rep.time.sd","DC.rep.time.sd", "DC.sus", "DC.exp",
                        "destination.folder", "replacement.df", "sens.number","exp.ship","verbose",
                        "test.suspects","diag.names", "diag.constraint", "diag.constraint.params" , 
                        "diag.test.lag.mean" , "diag.test.alpha" , "diag.test.beta","diag.trigger","diag.trigger.threshold" , "diag.trigger.response",
                        "diag.response.targets", "index.investigate.delay","nonindex.investigate.delay","dc.investigate.delay","index.investigate.delay.sd","nonindex.investigate.delay.sd","dc.investigate.delay.sd",
                        "batch.name","market.within","slaughershed.file","props.to.slaughter",
                        "tb.local.kernel",  "tb.wildlife.kernel",  "tb.wildlife.weight",  "wildlife.file","disease", "ctrl.type",
                        "ctrl.constraint.type","ctrl.constraint","ctrl.scale","ctrl.triggers","ctrl.trigger.threshold",
                        "ctrl.trigger.response","diag.scale","diag.constraint.files","diag.constraint.file.types","diag.test.lag.sd","ctrl.response.target", "ctrl.response.priority", "flex.file.name","slurm",
                        'diag.response.priority', "dairy.birth","mortality.rate","import.rate","transm.rate","contact.rate",
                        "gamma.e1", "gamma.e2r", "adjust.e2u.trans","adjust.e2u.react","adjust.iu.react", "detect.exp","detect.exp.nonreact",
                        "detect.inf.nonreact","test.sens")
  
  ## Return error if additional inputs aren't potential parameters
  unknown.inputs = additional.inputs[!names(additional.inputs) %in% parameter.options]
  if(length(unknown.inputs) > 0){
    error.message = paste0("----------------------------------------------------\n",
                           "These arguments aren't recognized: \n",
                           paste(names(unknown.inputs), collapse = "\n"), "\n",
                           "Files not generated. \n",
                           "----------------------------------------------------\n")
    return(cat(error.message))
  }
  ## Use only the additional arguments that match the possible parameter names
  additional.inputs = additional.inputs[names(additional.inputs) %in% parameter.options]
 
  ## Override values based on additional arguments passed to the function. 
  for(parameter in names(additional.inputs)){
    assign(parameter, additional.inputs[parameter][[1]]) 
  }
  
  ## For sensitivity runs, create parameter values based on the latin hypercube sampling ####
  if(parameter.sample == "spread"){
    lh <- read.csv("inputfiles/LHS_Parameters.csv", header = TRUE)
    transm.const.sp1  <- lh$Tc[sens.number]
    k1 <- lh$k1[sens.number]
    k2 <- lh$k2[sens.number]
    k3 <- lh$k3[sens.number]
    latency <- lh$lat[sens.number]
    infectious <- lh$inf[sens.number]
  }
  
  if(parameter.sample == "control"){
    lh <- read.csv("inputfiles/LHS_Control_Parameters_040119.csv", header = TRUE)
    cull.rate <- lh$cull_rate[sens.number]
    vax.rate <- lh$vax_rate[sens.number]
    effective.vax <- lh$vax_delay[sens.number]
    index.rep.time <- lh$index_rep_time[sens.number]
    rep.time <- lh$rep_time[sens.number]
    DC.rep.time <- lh$DC_rep_time[sens.number]
    DC.sus <- lh$DC_sus[sens.number]
    DC.exp <- lh$DC_exp[sens.number]
    mb.eff <- lh$shipban[sens.number]
    if(run.control == "IPDCcull" | run.control == "IPcull"|run.control == "MB_IPDCcull" | run.control == "flex_MBcull" | run.control == "MB_IPcull" | run.control == "flex_cull"){vax.rate = NULL}
    if(run.control == "IPDCcull" | run.control == "IPcull"|run.control == "MB_IPDCcull" | run.control == "flex_MBcull" |run.control == "MB_IPcull" | run.control == "flex_cull"){effective.vax = NULL}
  } 
  
  if(parameter.sample == "flex"){
    lh <- read.csv("inputfiles/LHS_Parameters_flexibleSensitivity_080620.csv", header = TRUE)
    flexFile <- data.frame("newRegionReportsOverX",0,"shipBan",0,"earliest")
    colnames(flexFile) <- names(lh)
    flexFile <- rbind(flexFile, lh[sens.number,])
    flexFile <- rbind(flexFile, lh[sens.number+1,])
    flexFile <- rbind(flexFile, lh[sens.number+2,])
    flexFile <- rbind(flexFile, lh[sens.number+3,])
    #ctrl.triggers <- gsub(" ", "", toString(flexFile[,1]))
    #ctrl.trigger.threshold <- gsub(" ", "", toString(flexFile[,2]))
    if(length(levels(factor(flexFile[,3]))) == 3){
      ctrl.type <- gsub(" ", "", toString(c("shipBan",unname(paste(sort(trimws(strsplit(toString(unique(flexFile[,3])[2:3]), ',')[[1]])), collapse=',')))))
    }else{
      ctrl.type <- gsub(" ", "", toString(c("shipBan",unname(paste(sort(trimws(strsplit(toString((flexFile[,3])[2:3]), ',')[[1]])), collapse=',')))))
    }
    #ctrl.response.priority <- gsub(" ", "", toString(flexFile[,4]))
    #ctrl.response.target <- gsub(" ", "", toString(flexFile[,5]))
    flexFile[] <- lapply(flexFile, as.character)
    flex.sens.control = paste(toString(flexFile[2,]), sep = "_")
    flex.sens.control <- gsub(" ", "", gsub(",","_", flex.sens.control))
    write.table(flexFile, file = paste0(paste(getwd(), "/inputfiles/", sep = ""),flex.sens.control, sep = "_" , "flex.txt"), quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)
    flex.file.name = paste0("inputfiles", sep = "/", flex.sens.control, sep = "_" , "flex.txt")
    flex.file = "flex"
  }
  
  ##########################################
  ## Creating FLAPS names and Landfill files ####
  ## Create list of FLAPS files from the inputted FLAPS name ("flaps" argument)
  flaps.names <- paste0(flaps,"_",  sprintf("%04d", 1:10),".txt", sep = "")
  all.flaps.names <- rep(flaps.names, each = reps)
  
  ## Create list of additional control files from the inputted control argument  
  if(landfill.files == TRUE){
    landfill.file.list <- paste0(landfill.file.name,"_", sprintf("%04d", 1:10), ".txt", sep = "")
    landfill.file.names <- rep(landfill.file.list, each = reps)} else {
      landfill.file.names <- rep('landfills_formatted.txt', each = 10 * reps)}
  
  ##########################################
  ## Create file naming conventions #### 
  
  # cull ring radius
  # vaccine ring radius (also includes check for allowable values of vax.range)
  
  cull_rng <-if(is.null(cull.range) == T){-1}else if(cull.range == -1){-1} else if(cull.range>=0){cull.range} else stop ("Vaccination Range must be positive or '-1' if not using ring vaccination")
  vax_rng <-if(is.null(vax.range) == T){-1}else if(vax.range == -1){-1} else if(vax.range>=0){vax.range} else stop ("Vaccination Range must be positive or '-1' if not using ring vaccination")
  
  if(run.control != "flex"){
    cull <- if(cull_rng>0){paste0(cull_rng/1000,"kmCull_")}else{""} 
    vax <- if(vax_rng>=0){paste0(vax_rng/1000,"kmVax_")}else{""} 
  }
  
  
  # Movement ban effectiveness
  MvmtBan <- if(mb.eff!=0){paste0("MvmtBan_",100*mb.eff,"_")}else{""}
  
  # Shipments off
  ShipOff <-if(shipment.gen == 0) {"ShipmentsOff_"} else{""}
  
  # Errors for parameters that are out of bounds and warnings for unique situations
  if(mb.eff < 0 | mb.eff > 1) stop ('Movement Ban Effectiveness (mb.eff) must be between 0 and 1')
  if(vax.eff < 0 | vax.eff > 1) stop ('Vaccine Effectiveness (vax.eff) must be between 0 and 1')
  
 # if(run.control == "flex"){
  #  file = read.table("inputfiles/flex.txt", header = F, colClasses = "character")
  #  run.control = paste(c(toString(file[2,]), toString(file[3,]), toString(file[4,]), toString(file[5,])), collapse = "")
#  }
  
  # Generate batch.name if one isn't provided. Add pieces to output filenames here (ex for FMD, whether partial transition is on and the ifnectous period).
  # This batch.name is what's used in post-processing to determine unique run types
  if(parameter.sample == "flex"){
    batch.name = ifelse(batch.name =="",paste0(ifelse(disease==0,"FMD","bTB"),"_",
                 ifelse(parameter.sample == "flex", flex.sens.control, run.control),"_",
                 ifelse(cull == "","",cull),
                 ifelse(vax == "","",vax),
                 ifelse(MvmtBan == "","",MvmtBan),
                 ifelse(ShipOff == "","",ShipOff)),batch.name)
  }else if(run.control == "flex"){
    batch.name = ifelse(batch.name =="",paste0(ifelse(disease==0,"FMD","bTB"),"_",
                                               ifelse(disease==0,paste0(ifelse(partial==1,"PTon","PToff"),"_Infectious",infectious,"days")),"_",
                                               ifelse(run.control == "flex", flex.control, run.control),"_",
                                               ifelse(MvmtBan == "","",MvmtBan),
                                               ifelse(ShipOff == "","",ShipOff), run.diagnostic), batch.name)
  }else{
    batch.name = ifelse(batch.name =="",paste0(ifelse(disease==0,"FMD","bTB"),"_",
                 ifelse(disease==0,paste0(ifelse(partial==1,"PTon","PToff"),"_Infectious",infectious,"days")),"_",
                 ifelse(run.control == "flex", flex.control, run.control),"_",
                 ifelse(cull == "","",cull),ifelse(vax == "","",vax),ifelse(MvmtBan == "","",MvmtBan),
                 ifelse(ShipOff == "","",ShipOff), run.diagnostic), batch.name)
  }

  ## Create the replacement_df data frame that contains all of the information created by this function.  ####
  config.fname <- paste0("config_", batch.name, "_",
                         gsub(".txt", "", all.flaps.names), "_", sprintf("%02d", rep(1:10, 10)), 
                         format(Sys.time(), '_%Y%m%d'), ifelse(is.na(sens.number), "", paste0("_",sens.number)), ".txt")
  
  jobname <- paste0("MI_",batch.name,"_f", sprintf("%02d", rep(1:10, each=10)), "_", 
                    sprintf("%02d", rep(1:10),"_", sens.number), format(Sys.time(), '_%Y%m%d'))
  
  replacement.df <- data.frame(config.fname, jobname, 
                               summary = summary,
                               detail = detail,
                               print.grid = print.grid,
                               print.control = print.control,
                               FLAPS.locations = paste0("FLAPS/", all.flaps.names), 
                               species = species,
                               timesteps = paste0(timesteps),
                               cutoff = paste0(cutoff),
                               verbose=verbose,
                               xy = xy,
                               fips.info = paste0(fips.info),
                               start.day = start.day,
                               disease = disease,
                               source = paste0(source), 
                               filetype = paste0(filetype),
                               market.within=market.within,
                               susceptibility.exponents=paste0(susc.exp.sp1,",",susc.exp.sp2),
                               infectiousness.exponents=paste0(transm.exp.sp1,",",transm.exp.sp2),
                               susceptibility.constants=paste0(susc.const.sp1,",",susc.const.sp2),
                               infectiousness.constants=paste0(transm.const.sp1,",",transm.const.sp2),
                               kernel.type = kernel.type,
                               kernels = paste0(k1,",", k2, ",", k3), 
                               data.kernel.file = data.kernel.file,
                               latency.dist= paste0(latency, ",",latency.sd), 
                               infectious.dist = paste0(infectious, ",",infectious.sd),
                               partial = partial, 
                               partial.param = partial.param,
                               grid.file = grid.file,
                               grid.side = grid.side,
                               grid.params = paste0(grid.max.farms,",",grid.min.side),
                               shipment.gen = paste0(shipment.gen),
                               shipment.scale = paste0(shipment.scale),
                               usamm.posteriors = paste0(usamm.posteriors),
                               usamm.order = paste0(usamm.order),
                               usamm.day = paste0(usamm.day),
                               usamm.origin.cov = paste0(usamm.origin.cov),
                               usamm.dest.cov = paste0(usamm.dest.cov),
                               exp.ship=exp.ship,
                               ctrl.type = ctrl.type,
                               ctrl.constraint.type = ctrl.constraint.type,
                               ctrl.constraint = if(run.control == "other") {ctrl.constraint <- ctrl.constraint} else 
                                            if (run.control == "noControl") {0} else
                                            if (run.control == "MB") {0} else
                                            if (run.control == "MB_IPcull" | run.control == "MB_IPDCcull" | run.control == "flex_MBcull"){paste0("0;",cull.rate,",",cull.rate.sd)} else
                                            if (run.control == "MB_cullVax" ){paste0("0;",cull.rate,",",cull.rate.sd,";",vax.rate,",",vax.rate.sd)} else
                                            if (run.control == "flex_cullVax" ){paste0(cull.rate,",",cull.rate.sd,";",vax.rate,",",vax.rate.sd)} else
                                            if (run.control == "IPcull" | run.control == "flex_cull") {paste0(cull.rate,",",cull.rate.sd)} else
                                            if (run.control == "flex_MBcullVax"){paste0("0;",cull.rate,",",cull.rate.sd,";",vax.rate,",",vax.rate.sd)} else
                                            if (run.control == "flex_MBvax"){paste0("0;",vax.rate,",",vax.rate.sd)} else
                                            if (run.control == "flex"){
                                              if("shipBan,cull" == ctrl.type){paste0("0;",cull.rate,",",cull.rate.sd)} else
                                              if("shipBan,vax,vax" == ctrl.type){paste0("0;",vax.rate,",",vax.rate.sd,";",vax.rate,",",vax.rate.sd)} else 
                                              if("shipBan,cull,vax" == ctrl.type){paste0("0;",cull.rate,",",cull.rate.sd,";",vax.rate,",",vax.rate.sd)}},
                               ctrl.scale = ctrl.scale,
                               ctrl.constraint.files = if(run.control == "other") {ctrl.constraint.files} else 
                                            if (run.control == "noControl") {"NA"} else
                                            if (run.control == "MB") {"NA"} else
                                            if (run.control == "MB_IPcull" |run.control == "MB_IPDCcull" | run.control == "flex_MBcull"){paste0("NA;inputfiles/",landfill.file.names)} else
                                            if (run.control == "MB_cullVax"){paste0("NA;inputfiles/",landfill.file.names,";inputfiles/",vaccine.file)} else
                                            if (run.control == "flex_cullVax"){paste0("inputfiles/",landfill.file.names,";inputfiles/",vaccine.file)} else
                                            if (run.control == "IPcull" |  run.control == "flex_cull") {paste0("inputfiles/", landfill.file.names)} else
                                            if (run.control == "flex_MBcullVax"){paste0("NA;inputfiles/",landfill.file.names,";inputfiles/",vaccine.file)} else
                                            if (run.control == "flex_MBvax"){paste0("NA;inputfiles/",vaccine.file)} else
                                            if (run.control == "flex"){
                                              if("shipBan,cull" == ctrl.type){paste0("NA;inputfiles/", landfill.file.names)} else
                                                if("shipBan,vax" == ctrl.type){paste0("NA;inputfiles/",vaccine.file)} else 
                                                  if("shipBan,cull,vax" == ctrl.type){paste0("NA;inputfiles/",landfill.file.names,";inputfiles/",vaccine.file)}},
                               ctrl.constraint.filetypes = if(run.control == "other") {ctrl.constraint.filetypes} else 
                                            if (run.control == "noControl") {"NA"} else
                                            if (run.control == "MB") {"NA"} else
                                            if (run.control == "MB_IPcull" |run.control == "MB_IPDCcull" | run.control == "flex_MBcull"){"NA;resourceLocs"} else
                                            if (run.control == "MB_cullVax"){"NA;resourceLocs;resourceBoosts"} else
                                            if (run.control == "flex_cullVax"){"resourceLocs;resourceBoosts"} else
                                            if (run.control == "IPcull" | run.control == "flex_cull") {"resourceLocs"} else
                                            if (run.control == "flex_MBcullVax"){"NA;resourceLocs;resourceBoosts"} else
                                            if (run.control == "flex_MBvax"){"NA;resourceBoosts"} else
                                             if (run.control == "flex"){
                                               if("shipBan,cull" == ctrl.type){"NA;resourceLocs"} else
                                                 if("shipBan,vax" == ctrl.type){"NA;resourceBoosts"} else 
                                                   if("shipBan,cull,vax" == ctrl.type){"NA;resourceLocs;resourceBoosts"}},
                               effective.mean = if(run.control == "other") {effective.mean} else 
                                            if (run.control == "noControl") {0} else
                                            if (run.control == "MB") {effective.mb} else
                                            if (run.control == "MB_IPcull" ) {paste0(effective.mb,",",effective.cull)} else
                                            if (run.control == "MB_IPDCcull" | run.control == "flex_MBcull") {paste0(effective.mb,",",effective.cull)} else
                                            if (run.control == "MB_cullVax") {paste0(effective.mb,",",effective.cull,",",effective.vax)} else
                                            if (run.control == "flex_cullVax") {paste0(effective.cull,",",effective.vax)} else
                                            if (run.control == "IPcull" |  run.control == "flex_cull") {effective.cull} else
                                            if (run.control == "flex_MBcullVax") {paste0(effective.mb,",",effective.cull,",",effective.vax)} else
                                            if (run.control == "flex_MBvax") {paste0(effective.mb,",",effective.vax)} else
                                              if (run.control == "flex"){
                                                if("shipBan,cull" == ctrl.type){paste0(effective.mb,",",effective.cull)} else
                                                  if("shipBan,vax" == ctrl.type){paste0(effective.mb,",",effective.vax)} else 
                                                    if("shipBan,cull,vax" == ctrl.type){paste0(effective.mb,",",effective.cull,",",effective.vax)}},
                               effective.sd = if(run.control == "other") {effective.mean.sd} else 
                                            if (run.control == "noControl") {0} else
                                            if (run.control == "MB") {effective.mb.sd} else
                                            if (run.control == "MB_IPcull" ) {paste0(effective.mb.sd,",",effective.cull.sd)} else
                                            if (run.control == "MB_IPDCcull" | run.control == "flex_MBcull") {paste0(effective.mb.sd,",",effective.cull.sd)} else
                                            if (run.control == "MB_cullVax") {paste0(effective.mb.sd,",",effective.cull.sd,",",effective.vax.sd)} else
                                            if (run.control == "flex_cullVax") {paste0(effective.cull.sd,",",effective.vax.sd)} else
                                            if (run.control == "IPcull" |  run.control == "flex_cull") {effective.cull.sd} else
                                            if (run.control == "flex_MBcullVax") {paste0(effective.mb.sd,",",effective.cull.sd,",",effective.vax.sd)} else
                                            if (run.control == "flex_MBvax") {paste0(effective.mb.sd,",",effective.vax.sd)} else
                                              if (run.control == "flex"){
                                                if("shipBan,cull" == ctrl.type){paste0(effective.mb.sd,",",effective.cull.sd)} else
                                                  if("shipBan,vax" == ctrl.type){paste0(effective.mb.sd,",",effective.vax.sd)} else 
                                                    if("shipBan,cull,vax" == ctrl.type){paste0(effective.mb.sd,",",effective.cull.sd,",",effective.vax.sd)}},
                               inactive.mean = if(run.control == "other") {inactive.mean} else 
                                            if (run.control == "noControl") {0} else
                                            if (run.control == "MB") {inactive.mb} else
                                            if (run.control == "MB_IPcull" ) {paste0(inactive.mb,",",inactive.cull)} else
                                            if (run.control == "MB_IPDCcull" | run.control == "flex_MBcull") {paste0(inactive.mb,",",inactive.cull)} else
                                            if (run.control == "MB_cullVax") {paste0(inactive.mb,",",inactive.cull,",",inactive.vax)} else
                                            if (run.control == "flex_cullVax") {paste0(inactive.cull,",",inactive.vax)} else
                                            if (run.control == "flex_MBcullVax") {paste0(inactive.mb,",",inactive.cull,",",inactive.vax)} else
                                            if (run.control == "flex_MBvax") {paste0(inactive.mb,",",inactive.vax)} else
                                            if (run.control == "IPcull" |  run.control == "flex_cull") {inactive.cull} else
                                              if (run.control == "flex"){
                                                if("shipBan,cull" == ctrl.type){paste0(inactive.mb,",",inactive.cull)} else
                                                  if("shipBan,vax" == ctrl.type){paste0(inactive.mb,",",inactive.vax)} else 
                                                    if("shipBan,cull,vax" == ctrl.type){paste0(inactive.mb,",",inactive.cull,",",inactive.vax)}},
                               inactive.sd = if(run.control == "other") {inactive.mean.sd} else 
                                            if (run.control == "noControl") {0} else
                                            if (run.control == "MB") {inactive.mb.sd} else
                                            if (run.control == "MB_IPcull" ) {paste0(inactive.mb.sd,",",inactive.cull.sd)} else
                                            if (run.control == "MB_IPDCcull" | run.control == "flex_MBcull") {paste0(inactive.mb.sd,",",inactive.cull.sd)} else
                                            if (run.control == "MB_cullVax") {paste0(inactive.mb.sd,",",inactive.cull.sd,",",inactive.vax.sd)} else
                                            if (run.control == "flex_cullVax") {paste0(inactive.cull.sd,",",inactive.vax.sd)} else    
                                            if (run.control == "flex_MBcullVax") {paste0(inactive.mb.sd,",",inactive.cull.sd,",",inactive.vax.sd)} else
                                            if (run.control == "flex_MBvax") {paste0(inactive.mb.sd,",",inactive.vax.sd)} else
                                            if (run.control == "IPcull" |  run.control == "flex_cull") {inactive.cull.sd} else 
                                              if (run.control == "flex"){
                                                if("shipBan,cull" == ctrl.type){paste0(inactive.mb.sd,",",inactive.cull.sd)} else
                                                  if("shipBan,vax" == ctrl.type){paste0(inactive.mb.sd,",",inactive.vax.sd)} else 
                                                    if("shipBan,cull,vax" == ctrl.type){paste0(inactive.mb.sd,",",inactive.cull.sd,",",inactive.vax.sd)}},
                               ctrl.response.target = if(run.control == "other") {ctrl.response.target} else 
                                            if (run.control == "noControl") {0} else
                                            if (run.control == "MB") {0} else
                                            if (run.control == "MB_IPcull" | run.control == "flex_MBcull") {"0,0"} else
                                            if (run.control == "MB_IPDCcull") {"0,0,-1"} else
                                            if (run.control == "MB_cullVax") {paste0("0,",cull.range,",",vax.range)} else
                                            if (run.control == "flex_cullVax") {paste0(cull.range,",",vax.range)} else
                                            if (run.control == "flex_MBcullVax") {paste0("0,", cull.range,",",vax.range)} else
                                            if (run.control == "flex_MBvax") {paste0("0,",vax.range)} else
                                            if (run.control == "IPcull" |  run.control == "flex_cull") {"0"} else 
                                              if (run.control == "flex"){ctrl.response.target},
                               flex.file = flex.file,
                               ctrl.triggers = ctrl.triggers,
                               ctrl.eff = if(run.control == "other") { ctrl.eff} else 
                                           if (run.control == "noControl") {0} else
                                           if (run.control == "MB") {paste0(mb.eff, ",", mb.eff)} else
                                           if (run.control == "MB_IPcull" |run.control == "MB_IPDCcull" | run.control == "flex_MBcull"){paste0(mb.eff, ",", mb.eff,";",cull.eff, ",", cull.eff)} else
                                           if (run.control == "MB_cullVax"){paste0(mb.eff, ",", mb.eff,";",cull.eff, ",", cull.eff,";", vax.eff, ",", vax.eff)} else
                                           if (run.control == "flex_cullVax"){paste0(cull.eff, ",", cull.eff,";", vax.eff, ",", vax.eff)} else
                                           if (run.control == "flex_MBcullVax" ){paste0(mb.eff, ",", mb.eff,";",cull.eff, ",", cull.eff,";", vax.eff, ",", vax.eff)} else
                                           if (run.control == "flex_MBvax" ){paste0(mb.eff, ",", mb.eff,";", vax.eff, ",", vax.eff)} else
                                           if (run.control == "IPcull" | run.control == "flex_cull") {paste0(cull.eff, ",", cull.eff)} else 
                                             if (run.control == "flex"){
                                               if("shipBan,cull" == ctrl.type){paste0(mb.eff, ",", mb.eff,";",cull.eff, ",", cull.eff)} else
                                                 if("shipBan,vax" == ctrl.type){paste0(mb.eff, ",", mb.eff,";", vax.eff, ",", vax.eff)} else 
                                                   if("shipBan,cull,vax" == ctrl.type){paste0(mb.eff, ",", mb.eff,";",cull.eff, ",", cull.eff,";", vax.eff, ",", vax.eff)}},
                               ctrl.trigger.threshold = ctrl.trigger.threshold,
                               ctrl.trigger.response = ctrl.trigger.response,
                               ctrl.response.priority = if (run.control == "other" | run.control == "flex") {ctrl.response.priority} else
                                          if (run.control == "noControl" | run.control == "MB" | run.control == "IPcull" |  run.control == "flex_cull") {"earliest"} else
                                          if (run.control == "MB_IPcull" | run.control == "flex_MBcull" | run.control == "flex_cullVax" | run.control == "flex_MBvax") {paste0("earliest",",","earliest")} else
                                          if (run.control == "MB_IPDCcull" | run.control == "MB_cullVax" | run.control == "flex_MBcullVax") {paste0( "earliest",",","earliest",",","earliest")},
                               flex.file.name = flex.file.name,
                               index.rep.time = paste0(index.rep.time,",",index.rep.time.sd ),
                               rep.time = paste0(rep.time,",",rep.time.sd),
                               DC.rep.time = paste0(DC.rep.time, ",",DC.rep.time.sd),
                               DC_scaling = paste0("sus,", DC.sus, ";exp,", DC.exp),
                               test.suspects = test.suspects,
                               diag.names = diag.names,
                               diag.constraint = diag.constraint,
                               diag.constraint.params = diag.constraint.params,
                               diag.scale = diag.scale,
                               diag.constraint.files = diag.constraint.files,
                               diag.constraint.file.types = diag.constraint.file.types,
                               diag.test.lag.mean = diag.test.lag.mean,
                               diag.test.lag.sd = diag.test.lag.sd,
                               diag.test.alpha = diag.test.alpha,
                               diag.test.beta = diag.test.beta,
                               diag.trigger = diag.trigger,
                               diag.trigger.threshold = diag.trigger.threshold,
                               diag.trigger.response = diag.trigger.response,
                               diag.response.targets = diag.response.targets,
                               diag.response.priority = if (run.diagnostic == "other") {diag.response.priority} else
                                          if (run.diagnostic == "noDiagnostics" | run.diagnostic == "Suspect_elisa" | run.diagnostic == "Suspect_pcr"|run.diagnostic == "atRisk_elisa" | 
                                              run.diagnostic == "atRisk_pcr" | run.diagnostic == "Suspect_lowSens" | run.diagnostic == "Suspect_highSens") {"earliest"} else
                                          if (run.diagnostic == "Suspect_atRisk_elisa_pcr" | run.diagnostic == "SP_atRisk_highSens_lowSens") {paste0("earliest",",","earliest")},
                               index.investigate.delay =paste0(index.investigate.delay,",",index.investigate.delay.sd),
                               nonindex.investigate.delay =paste0(nonindex.investigate.delay,",",nonindex.investigate.delay.sd),
                               dc.investigate.delay =paste0(dc.investigate.delay,",",dc.investigate.delay.sd),
                               slaughtershed.file = paste0(slaughtershed.file),
                               props.to.slaughter = paste0(props.to.slaughter),
                               tb.local.kernel =paste0(tb.local.kernel),
                               tb.wildlife.kernel = tb.wildlife.kernel,
                               tb.wildlife.weight = tb.wildlife.weight,
                               wildlife.file = paste0(wildlife.file),
                               dairy.birth = dairy.birth,
                               mortality.rate = mortality.rate,
                               import.rate = import.rate,
                               transm.rate = transm.rate,
                               contact.rate = contact.rate,
                               gamma.e1 = gamma.e1,
                               gamma.e2r = gamma.e2r,
                               adjust.e2u.trans = adjust.e2u.trans,
                               adjust.e2u.react = adjust.e2u.react,
                               adjust.iu.react = adjust.iu.react,
                               detect.exp = detect.exp,
                               detect.exp.nonreact = detect.exp.nonreact,
                               detect.inf.nonreact = detect.inf.nonreact,
                               test.sens = test.sens,
                               stringsAsFactors = FALSE)
  
  replacement.df$full.batch.name = gsub("config_", "", replacement.df$config.fname)
  replacement.df$full.batch.name = gsub(".txt", "", replacement.df$full.batch.name)
  
  columns <- length(replacement.df)
  for(i in 3:columns){
            if (colnames(replacement.df)[i] == "full.batch.name"){colnames(replacement.df)[i] = 1} else 
            if (colnames(replacement.df)[i] == "summary"){colnames(replacement.df)[i] = 2} else 
            if (colnames(replacement.df)[i] == "detail"){colnames(replacement.df)[i] = 3} else 
            if (colnames(replacement.df)[i] == "print.grid"){colnames(replacement.df)[i] = 4} else 
            if (colnames(replacement.df)[i] == "print.control"){colnames(replacement.df)[i] = 6} else
            if (colnames(replacement.df)[i] == "FLAPS.locations"){colnames(replacement.df)[i] = 11} else 
            if (colnames(replacement.df)[i] == "species"){colnames(replacement.df)[i] = 12} else 
            if (colnames(replacement.df)[i] == "timesteps"){colnames(replacement.df)[i] = 13} else 
            if (colnames(replacement.df)[i] == "cutoff"){colnames(replacement.df)[i] = 14} else 
            if (colnames(replacement.df)[i] == "verbose"){colnames(replacement.df)[i] = 15} else 
            if (colnames(replacement.df)[i] == "xy"){colnames(replacement.df)[i] = 17} else
            if (colnames(replacement.df)[i] == "fips.info"){colnames(replacement.df)[i] = 18} else
            if (colnames(replacement.df)[i] == "start.day"){colnames(replacement.df)[i] = 19} else
            if (colnames(replacement.df)[i] == "disease"){colnames(replacement.df)[i] = 20} else
            if (colnames(replacement.df)[i] == "source"){colnames(replacement.df)[i] = 21} else 
            if (colnames(replacement.df)[i] == "filetype"){colnames(replacement.df)[i] = 22} else 
            if (colnames(replacement.df)[i] == "market.within"){colnames(replacement.df)[i] = 23} else
            if (colnames(replacement.df)[i] == "susceptibility.exponents"){colnames(replacement.df)[i] = 24} else
            if (colnames(replacement.df)[i] == "infectiousness.exponents"){colnames(replacement.df)[i] = 25} else
            if (colnames(replacement.df)[i] == "susceptibility.constants"){colnames(replacement.df)[i] = 26} else
            if (colnames(replacement.df)[i] == "infectiousness.constants"){colnames(replacement.df)[i] = 27} else
            if (colnames(replacement.df)[i] == "kernel.type"){colnames(replacement.df)[i] = 28} else
            if (colnames(replacement.df)[i] == "kernels"){colnames(replacement.df)[i] = 29} else
            if (colnames(replacement.df)[i] == "data.kernel.file"){colnames(replacement.df)[i] = 30} else
            if (colnames(replacement.df)[i] == "latency.dist"){colnames(replacement.df)[i] = 31} else
            if (colnames(replacement.df)[i] == "infectious.dist"){colnames(replacement.df)[i] = 32} else
            if (colnames(replacement.df)[i] == "partial"){colnames(replacement.df)[i] = 33} else
            if (colnames(replacement.df)[i] == "partial.param"){colnames(replacement.df)[i] = 34} else
            if (colnames(replacement.df)[i] == "grid.file"){colnames(replacement.df)[i] = 36} else
            if (colnames(replacement.df)[i] == "grid.side"){colnames(replacement.df)[i] = 37} else
            if (colnames(replacement.df)[i] == "grid.params"){colnames(replacement.df)[i] = 38} else
            if (colnames(replacement.df)[i] == "shipment.gen"){colnames(replacement.df)[i] = 41} else
            if (colnames(replacement.df)[i] == "shipment.scale"){colnames(replacement.df)[i] = 42} else
            if (colnames(replacement.df)[i] == "usamm.posteriors"){colnames(replacement.df)[i] = 44} else
            if (colnames(replacement.df)[i] == "usamm.order"){colnames(replacement.df)[i] = 45} else
            if (colnames(replacement.df)[i] == "usamm.day"){colnames(replacement.df)[i] = 46} else
            if (colnames(replacement.df)[i] == "usamm.origin.cov"){colnames(replacement.df)[i] = 47} else
            if (colnames(replacement.df)[i] == "usamm.dest.cov"){colnames(replacement.df)[i] = 48} else
            if (colnames(replacement.df)[i] == "exp.ship"){colnames(replacement.df)[i] = 50} else
            if (colnames(replacement.df)[i] == "ctrl.type"){colnames(replacement.df)[i] = 51} else
            if (colnames(replacement.df)[i] == "ctrl.constraint.type"){colnames(replacement.df)[i] = 52} else
            if (colnames(replacement.df)[i] == "ctrl.constraint"){colnames(replacement.df)[i] = 53} else
            if (colnames(replacement.df)[i] == "ctrl.scale"){colnames(replacement.df)[i] = 54} else
            if (colnames(replacement.df)[i] == "ctrl.constraint.files"){colnames(replacement.df)[i] = 55} else 
            if (colnames(replacement.df)[i] == "ctrl.constraint.filetypes"){colnames(replacement.df)[i] = 56} else
            if (colnames(replacement.df)[i] == "effective.mean"){colnames(replacement.df)[i] = 57} else
            if (colnames(replacement.df)[i] == "effective.sd"){colnames(replacement.df)[i] = 58} else
            if (colnames(replacement.df)[i] == "inactive.mean"){colnames(replacement.df)[i] = 59} else
            if (colnames(replacement.df)[i] == "inactive.sd"){colnames(replacement.df)[i] = 60} else
            if (colnames(replacement.df)[i] == "ctrl.eff"){colnames(replacement.df)[i] = 61} else
            if (colnames(replacement.df)[i] == "flex.file"){colnames(replacement.df)[i] = 62} else
            if (colnames(replacement.df)[i] == "ctrl.triggers"){colnames(replacement.df)[i] = 63} else
            if (colnames(replacement.df)[i] == "ctrl.trigger.threshold"){colnames(replacement.df)[i] = 64} else
            if (colnames(replacement.df)[i] == "ctrl.trigger.response"){colnames(replacement.df)[i] = 65} else
            if (colnames(replacement.df)[i] == "ctrl.response.target"){colnames(replacement.df)[i] = 66} else
            if (colnames(replacement.df)[i] == "ctrl.response.priority"){colnames(replacement.df)[i] = 67} else
            if (colnames(replacement.df)[i] == "flex.file.name"){colnames(replacement.df)[i] = 68} else 
            if (colnames(replacement.df)[i] == "index.rep.time"){colnames(replacement.df)[i] = 71} else
            if (colnames(replacement.df)[i] == "rep.time"){colnames(replacement.df)[i] = 72} else
            if (colnames(replacement.df)[i] == "DC.rep.time"){colnames(replacement.df)[i] = 73} else
            if (colnames(replacement.df)[i] == "DC_scaling"){colnames(replacement.df)[i] = 74} else
            if (colnames(replacement.df)[i] == "test.suspects"){colnames(replacement.df)[i] = 78} else
            if (colnames(replacement.df)[i] == "diag.names"){colnames(replacement.df)[i] = 79} else
            if (colnames(replacement.df)[i] == "diag.constraint"){colnames(replacement.df)[i] = 80} else
            if (colnames(replacement.df)[i] == "diag.constraint.params"){colnames(replacement.df)[i] = 81} else
            if (colnames(replacement.df)[i] == "diag.scale"){colnames(replacement.df)[i] = 82} else
            if (colnames(replacement.df)[i] == "diag.constraint.files"){colnames(replacement.df)[i] = 83} else
            if (colnames(replacement.df)[i] == "diag.constraint.file.types"){colnames(replacement.df)[i] = 84} else
            if (colnames(replacement.df)[i] == "diag.test.lag.mean"){colnames(replacement.df)[i] = 85} else
            if (colnames(replacement.df)[i] == "diag.test.lag.sd"){colnames(replacement.df)[i] = 86} else
            if (colnames(replacement.df)[i] == "diag.test.alpha"){colnames(replacement.df)[i] = 87} else
            if (colnames(replacement.df)[i] == "diag.test.beta"){colnames(replacement.df)[i] = 88} else
            if (colnames(replacement.df)[i] == "diag.trigger"){colnames(replacement.df)[i] = 91} else
            if (colnames(replacement.df)[i] == "diag.trigger.threshold"){colnames(replacement.df)[i] = 92} else
            if (colnames(replacement.df)[i] == "diag.trigger.response"){colnames(replacement.df)[i] = 93} else
            if (colnames(replacement.df)[i] == "diag.response.targets"){colnames(replacement.df)[i] = 94} else
            if (colnames(replacement.df)[i] == "diag.response.priority"){colnames(replacement.df)[i] = 95} else
            if (colnames(replacement.df)[i] == "index.investigate.delay"){colnames(replacement.df)[i] = 100} else
            if (colnames(replacement.df)[i] == "nonindex.investigate.delay"){colnames(replacement.df)[i] = 101} else
            if (colnames(replacement.df)[i] == "dc.investigate.delay"){colnames(replacement.df)[i] = 102} else
            if (colnames(replacement.df)[i] == "slaughtershed.file"){colnames(replacement.df)[i] = 110} else
            if (colnames(replacement.df)[i] == "props.to.slaughter"){colnames(replacement.df)[i] = 111} else
            if (colnames(replacement.df)[i] == "tb.local.kernel"){colnames(replacement.df)[i] = 122} else
            if (colnames(replacement.df)[i] == "tb.wildlife.kernel"){colnames(replacement.df)[i] = 123} else
            if (colnames(replacement.df)[i] == "tb.wildlife.weight"){colnames(replacement.df)[i] = 124} else
            if (colnames(replacement.df)[i] == "wildlife.file"){colnames(replacement.df)[i] = 129} else
            if (colnames(replacement.df)[i] == "dairy.birth"){colnames(replacement.df)[i] = 132} else
            if (colnames(replacement.df)[i] == "mortality.rate"){colnames(replacement.df)[i] = 133} else
            if (colnames(replacement.df)[i] == "import.rate"){colnames(replacement.df)[i] = 134} else
            if (colnames(replacement.df)[i] == "transm.rate"){colnames(replacement.df)[i] = 135} else
            if (colnames(replacement.df)[i] == "contact.rate"){colnames(replacement.df)[i] = 136} else
            if (colnames(replacement.df)[i] == "gamma.e1"){colnames(replacement.df)[i] = 138} else
            if (colnames(replacement.df)[i] == "gamma.e2r"){colnames(replacement.df)[i] = 139} else
            if (colnames(replacement.df)[i] == "adjust.e2u.trans"){colnames(replacement.df)[i] = 140} else
            if (colnames(replacement.df)[i] == "adjust.e2u.react"){colnames(replacement.df)[i] = 141} else
            if (colnames(replacement.df)[i] == "adjust.iu.react"){colnames(replacement.df)[i] = 142} else
            if (colnames(replacement.df)[i] == "detect.exp"){colnames(replacement.df)[i] = 143} else
            if (colnames(replacement.df)[i] == "detect.exp.nonreact"){colnames(replacement.df)[i] = 144} else
            if (colnames(replacement.df)[i] == "detect.inf.nonreact"){colnames(replacement.df)[i] = 145} else
            if (colnames(replacement.df)[i] == "test.sens"){colnames(replacement.df)[i] = 146} else
        warning(paste0(colnames(replacement.df)[i]," has not been added to this function. Change the line 
                value in the template file then rerun this function without that variable."))
  }
  
  ## Write out the data frame with the associated parameters ####
  print(head(replacement.df))
  str1 <- paste0('replacementDF_',batch.name,'.csv') 
  write.csv(replacement.df, paste0(format(Sys.time(),'%Y%m%d_%H%M_'), sub('\\..*', '', str1), ".csv"), row.names = FALSE)
  
  ## Generate the Config, Batch, and Job files.
  destination.folder = paste0(destination.folder, "/")
  config = readLines(template.config.location)
  sh.template = readLines(sh.template)
  for(i in 1:dim(replacement.df)[1]){  
    config = readLines(template.config.location)
    for(j in 3:dim(replacement.df)[2]){
      replacement.value = replacement.df[i,j]  
      # Find the appropriate line:
      replacement.expression = paste0("\\#\\(", colnames(replacement.df)[j], ")")
      replacement.line.no = grep(replacement.expression, config)
      
      replacement.line = config[replacement.line.no]
      comment.string = strsplit(replacement.line, replacement.expression)[[1]][2]
      config[replacement.line.no] = 
        paste0(replacement.value," ", gsub("\\\\", "", replacement.expression), comment.string)
      
    }
    fname = replacement.df$config.fname[i]
    cat(config, file=paste0(destination.folder, fname), sep="\n")
    
    jobname = replacement.df$jobname[i]
    
    if (slurm == 1){
    #Generate a batch (.sh) file for each config:
    sh.file = gsub("JOBNAME", jobname, sh.template)
    sh.file = gsub("CONFIGNAME", fname, sh.file)
    fname.sh = gsub(".txt", ".sh", fname)
    fname.sh = gsub("config", "BATCH",fname.sh)
    cat(sh.file, file = paste0(destination.folder, fname.sh), sep = "\n")
    
    ### Add a line to the jobfile so the Batch file will run:
    cat(paste0("sbatch ", fname.sh, "\n"), file=paste0(destination.folder, jobfile.name), append=TRUE) 
    } else{
      # make jobfile that refers to configs instead of BATCH files
      cat(paste0(fname, "\n"), file=paste0(destination.folder, jobfile.name), append=TRUE)
      
    }
  }
}





####################################################################################################################################
####################################################################################################################################

## Example commands ##
  
# # Testing on Windows
# setwd("D:/Postdoc/Projects/USDOS_TestRun")

# createConfigs(destination.folder="./Generated_configs/TestBucket")
# # FMD base  
# createConfigs(destination.folder="./Generated_configs/TestBucket") 
# createConfigs(destination.folder="./Generated_configs/TestBucket",slurm = 1) # for Summit
# createConfigs(run.control = "noControl",run.diagnostic = "noDiagnostics",destination.folder="./Generated_configs/TestBucket") 
# 
# # FMD test suspects IPMB Elisa
# createConfigs(run.control = "MB_IPcull",run.diagnostic ="Suspect_elisa",destination.folder="./Generated_configs/TestBucket") 
# createConfigs(run.control = "MB_IPcull", mb.eff=0.9, run.diagnostic ="Suspect_elisa",destination.folder="./Generated_configs/TestBucket") 
# 
# # FMD with suspect testing on, IP cull and movement ban (75% effective), elisa with noLimit constraint value, alpha =25,beta=16
# createConfigs(run.control = "MB_IPcull",
#               run.diagnostic ="Suspect_elisa",
#               mb.eff = 0.75,
#               diag.constraint ="noLimit",
#               diag.test.alpha =25,
#               diag.test.beta=16,
#               destination.folder="./Generated_configs/TestBucket")
# 
# # FMD test suspects IPMB PCR
# createConfigs(run.control = "MB_IPcull",run.diagnostic ="Suspect_pcr",destination.folder="./Generated_configs/TestBucket") 
# 
# 
# # FMD SP atRisk elisa,pcr
# createConfigs(run.control = "MB_IPcull",run.diagnostic ="Suspect_atRisk_elisa_pcr",destination.folder="./Generated_configs/TestBucket") 
# 
# # FMD with suspect and atRisk testing, IP cull & Movement ban (75%), elisa&pcr testing, 
#     # exposed shipments off, verbose=2,
#     # alpha (elisa)=15, alpha(pcr)=30, diagnostic constraints of 50 on the ELISA,
#     # test lag of 3 days instead of 5 for the elisa, using an atRisk 0.5km ring instead of DCs, index exposure to investigate =16, nonindex=9.
# createConfigs(run.control = "MB_IPcull",
#               run.diagnostic ="Suspect_atRisk_elisa_pcr",
#               mb.eff = 0.75,
#               exp.ship=0,
#               verbose=2,
#               # note two values for diagnostic parameters since there are two tests
#               diag.test.alpha = "15,30",
#               diag.test.lag.mean ="3,0",
#               diag.constraint.params = "50,0;1000,0",
#               diag.response.targets = "0,500",
#               index.investigate.delay = 16,
#               nonindex.investigate.delay =9,
#               destination.folder="./Generated_configs/TestBucket")
# 
# 
# 
# # FMD atRisk elisa
# createConfigs(run.control = "MB_IPcull",run.diagnostic ="atRisk_elisa",destination.folder="./Generated_configs/TestBucket") 
# 
# # FMD atRisk pcr
# createConfigs(run.control = "MB_IPcull",run.diagnostic ="atRisk_pcr",destination.folder="./Generated_configs/TestBucket") 
# 
# ###################### bTB ################
# ## bTB base
# createConfigs(disease = 1, destination.folder="./Generated_configs/TestBucket")
# 
# # bTB SP IP low Sens
# createConfigs(disease = 1, run.control ="IPcull",run.diagnostic = "Suspect_lowSens",
#               destination.folder="./Generated_configs/TestBucket")
# 
# # bTB SP IP highSens
# createConfigs(disease = 1, run.control ="IPcull",run.diagnostic = "Suspect_highSens",
#               destination.folder="./Generated_configs/TestBucket")
# 
# # btb SPatRisk IP lowSens,highSense
# createConfigs(disease = 1, run.control ="IPcull",run.diagnostic = "SP_atRisk_highSens_lowSens",
#               destination.folder="./Generated_configs/TestBucket")
# 
# 
# ######################################
# 
# 
# # Sensitivity
# setwd("~/Desktop/USDOSPipeline")
# for(i in 1:100){
#     createConfigs(run.control = "flex_MBcullVax",
#                   slurm = 1,
#                   print.control = 3,
#                   partial =1,
#                   source = "inputfiles/SensCounty_shortlist_0219.txt",
#                   parameter.sample = "flex",
#                   sens.number = i,
#                   jobfile.name = paste0("USDOS_Sens_", i, ".job"),
#                   partial.param="0.05,0.006,0.44,4,6.27237",
#                   usamm.posteriors = "inputfiles/beef_USAMMv3_publ_posterior.txt, inputfiles/dairy_USAMMv3_publ_posterior.txt", 
#                   market.within = 0.5) 
# }
# 
# 
# 
# ###################### Other run types ################
# 
# # FMD IPcull DCvax
# createConfigs(run.control = "MB_cullVax",destination.folder="./Generated_configs/TestBucket") 
# 
# # FMD IPcull ring vax
# createConfigs(run.control = "MB_cullVax",destination.folder="./Generated_configs/TestBucket",vax.range = 3000) 
# 
# # Shipments off, base run 
# createConfigs(shipment.gen = 0,destination.folder="./Generated_configs/TestBucket") 
# 
# # Only movement ban
# createConfigs(run.control = "MB",destination.folder="./Generated_configs/TestBucket") 

###################
##################
### For runs on on Summit ###

# createConfigs(slurm=1)
# createConfigs(slurm=1,
#		run.control="MB_IPcull",
#		partial=1,
#		partial.param="0.05,0.006,0.44,4,6.27237", 
#		usamm.posteriors = "inputfiles/beef_k3_cov.post, inputfiles/dairy_k3_cov.post", 
#		market.within = 0.5,
#		shipment.gen=5,
#		usamm.origin.cov="inputfiles/county_covariates_scaled_mean_zero.txt,inputfiles/county_covariates_scaled_mean_zero.txt",
#		usamm.dest.cov="inputfiles/county_covariates_scaled_mean_zero.txt,inputfiles/county_covariates_scaled_mean_zero.txt",
#		infectious=20,
#		shipment.scale=0.25)
# setwd("~/Desktop/USDOSPipeline")
# 
# createConfigs(run.control = "flex", destination.folder="./Generated_configs/TestBucket")

# createConfigs(slurm=1,
#               verbose=0,
#               run.control = "flex",
#               flex.file.name = "inputfiles/IPCull_DCVax_Largest.txt",
#               partial.param="0.05,0.006,0.44,4,6.27237",
#               usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt", 
#               usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               market.within = 0.5)
# 
# createConfigs(slurm=1,
#               verbose=0,
#               run.control = "flex",
#               flex.file.name = "inputfiles/IPCull_DCVax_newPrem_Decrease.txt",
#               partial.param="0.05,0.006,0.44,4,6.27237",
#               usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt", 
#               usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               market.within = 0.5)
# 
# createConfigs(slurm=1,
#               verbose=0,
#               run.control = "flex",
#               flex.file.name = "inputfiles/IPCull_3kmVax_Smallest.txt",
#               partial.param="0.05,0.006,0.44,4,6.27237",
#               usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt",
#               usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               market.within = 0.5)
# 
# createConfigs(slurm=1,
#               verbose=0,
#               run.control = "flex",
#               flex.file.name = "inputfiles/IPCull_3kmVax_Largest.txt",
#               partial.param="0.05,0.006,0.44,4,6.27237",
#               usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt",
#               usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               market.within = 0.5)
# 
# createConfigs(slurm=1,
#               verbose=0,
#               run.control = "flex",
#               flex.file.name = "inputfiles/IPCull_3kmVax_Closest.txt",
#               partial.param="0.05,0.006,0.44,4,6.27237",
#               usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt",
#               usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               market.within = 0.5)
# 
# createConfigs(slurm=1,
#               verbose=0,
#               run.control = "flex",
#               flex.file.name = "inputfiles/IPCull_3kmVax_Farthest.txt",
#               partial.param="0.05,0.006,0.44,4,6.27237",
#               usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt",
#               usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
#               market.within = 0.5)
createConfigs(slurm=1,
              verbose=0,
              run.control = "flex",
              flex.file.name = "inputfiles/IPcull_3kmVax.txt",
              partial.param="0.05,0.006,0.44,4,6.27237",
              usamm.posteriors="inputfiles/USAMMv3_beef_posterior.txt,inputfiles/USAMMv3_dairy_posterior.txt",
              usamm.origin.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
              usamm.dest.cov="inputfiles/USAMMv3_cattle_covariates.txt, inputfiles/USAMMv3_cattle_covariates.txt",
              market.within = 0.5)