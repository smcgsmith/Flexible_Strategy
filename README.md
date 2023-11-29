# Supporting Emergency Disease Management with State-Dependent Control

## Abstract

Control interventions aim to alter the transmission dynamics of an infectious disease outbreak. Policy makers can either implement these interventions all at once (static) or gradually in response to changes in the outbreak (state-dependent). State-dependent control policies may help reduce the impacts of an infectious disease outbreak by allowing policy makers to tailor control policies to a given outbreak and prevent causing unnecessary socioeconomic harm. State-dependent control policies are often in official outbreak response plans, such as for foot and mouth disease in the US  and UK, but it remains unclear whether the benefits of state-dependent control policies outweigh the potential costs...

## Repository Contents

- **USDOSv2.2.1:** United States Disease Outbreak Simulation (USDOS) version 2.2.1 module.
- **Post_Processing:** R scripts and files for processing USDOS simulation results.
- **Sensitivity_Analysis:** R scripts for Latin Hypercube Sampling (LHS) of parameter sets and analyzing USDOS sensitivity runs.

## Getting Started

To utilize the contents of this repository, follow these steps:

1. **Run USDOSv2.2.1 Module:**
   - Navigate to the `USDOSv2.2.1` directory and follow the instructions in its README.md to run the USDOS simulation module.

2. **Place Output Files in Post_Processing/Files_To_Process:**
   - After running USDOSv2.2.1, locate the generated output files in the USDOSv2.2.1 folder.
   - Place these files in the `Post_Processing/Files_To_Process` directory.

3. **Run Post-Processing Module:**
   - In the `Post_Processing` directory, execute the `run_postProcessing_USDOSv2.2.1.R` script.
   - This script will process the USDOS output files by generating datafiles, histograms, violin plots, and maps summarizing each simulation scenario.

For more detailed information on each component, refer to the README.md files within the respective subdirectories.

If you encounter any issues or have questions, please contact the project owner, webblaboratory@gmail.com.
