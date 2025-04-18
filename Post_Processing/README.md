# United States Disease Outbreak Simulation (USDOS) Post-Processing
This project contains R scripts to process data files from USDOS simulations.

## Folder Structure
The project is organized into the following directories:
- `Include_Files`: Contains dependencies necessary to run the `processUSDOS()` function, such as R packages, functions, etc.
- `Files_To_Process`: USDOS data files should be put here.
- `FLAPS`: Contains FLAPS (projected cattle/swine premises size and location) files used to determine the number.
- `Maps`: Generated by running `processUSDOS()` and will include maps of simulation output.
- `Data`: Generated by running `processUSDOS()` and includes concatenated long and wide format .csv files used to produce the figures.
- `Figures`: Generated by running `processUSDOS()` and includes concatenated long and wide format .csv files used to produce the figures.

## Files
The following files are included in the project:
- `postProcessing_USDOSv2.1.R`: The R script that contains a function to process USDOS runs. This function can be thought of like main() function.
- `run_postProcessing_USDOSDv2.1.R`: A shell script for running the post-processing function, processUSDOS(), and generating FMD outbreak cost figures.

## Getting Started
To use this project, clone the repository to your local machine. The post-processing scripts require R and several R packages, including `dplyr`, `tidyr`, `reshape2`, `rgdal`, and `ggplot2`. Required packages will be automatically installed if they are not already present on your machine.

## Running `processUSDOS()`
To run the post-processing function, `processUSDOS()`, navigate to the `run_postProcessing_USDOSDv2.1.R` script, set the working directory (line 10), load the `processUSDOS()` function (line 51), and execute the function (lines 53 - 64). Running this script will generate directories labeled `Data`, `Figures`, and `Maps` with desired outputs.

If you are interested in generating figures in the manuscript, the `FlexibleControlStrategy_PostProcessing.R` script will use the post-processing function to generate data files and then includes code necessary to reproduce these figures.

## Important Notes`
Within the `processUSDOS()` function, all functions that begin with a `.` (e.g. .reshape_data() or .extract_runDetailsFromType()) are custom and can be found in the `Include_Files` directory. `processUSDOS()` will dynamically determine whether or not a machine is capable of using parallelized code and the number of cores to use for processing (n-2).

## Contact Information
If you have any questions or comments about this project, please contact the project owner, sm.smith@colostate.edu

## Directory Tree
```bash
.
├── Include_Files
│   ├── Color_Manager.R
│   ├── Data_Manager.R
│   ├── load_geographyFiles.R
│   ├── map_by_fips_standalone.R
│   ├── MAP_county_census2016_5m.RData
│   ├── Map_Manager.R
│   ├── MAP_state_census2016_5m.RData
│   ├── Package_Manager.R
│   └── Plot_Manager.R
├── Files_To_Process
│   └── [USDOS output files (summary, detail, etc.) go here]
├── FLAPS
│   ├── FLAPS12_Quarterly_USDOS_format_0001.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0002.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0003.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0004.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0005.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0006.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0007.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0008.txt
│   ├── FLAPS12_Quarterly_USDOS_format_0009.txt
│   └── FLAPS12_Quarterly_USDOS_format_0010.txt
├── Maps
│   └── [Outbreak maps are put here]
├── Data
│   └── [Dataframes used for plotting are put here]
├── Figures
│   └── [Histogram and violin plots are put here]
├── postProcessing_USDOSv2.1.R
└── run_postProcessing_USDOSDv2.1.R
```
