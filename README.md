# Potential benefits of adaptive control strategies are outweighed by costs of infrequent, but dramatically larger disease outbreaks
## Abstract

Control is used to alter the transmission dynamics of an infectious disease outbreak in order to prevent further spread of an infection. Managers can either implement all available control actions at once (fixed) or gradually in response to changes in the outbreak (adaptive). Adaptive control strategies may help reduce the impacts of an infectious disease outbreak by allowing policymakers to tailor control strategies to a given outbreak and prevent causing unnecessary socioeconomic harm. Adaptive control strategies are often in official outbreak response plans, such as for foot and mouth disease in the U.S. and U.K., but it remains unclear whether the benefits of adaptive control strategies outweigh the potential of under-reacting and resulting in larger outbreaks. To weigh this trade-off, we used a realistic and validated national scale foot and mouth disease transmission model to compare how adaptive and fixed control strategies as well as various attributes of the control process affect outbreak size. We find that adaptive control strategies do not cost less for the vast majority of outbreaks, but infrequently result in much larger and more costly outbreaks due to decision-making time and case reporting lags. Thus, this study emphasizes the cost of under-reacting to a disease outbreak and that minimizing decision-making time should be a key consideration when developing outbreak response guidelines.

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
