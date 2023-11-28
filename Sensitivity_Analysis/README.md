{\rtf1\ansi\ansicpg1252\cocoartf2706
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww21860\viewh13640\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Analyzing United States Disease Outbreak Simulation (USDOS) Sensitivity Analysis Results\
This project contains R scripts to process data files from USDOS simulations.\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 \
This set of R scripts performs sensitivity analyses on United States Disease Outbreak Simulation (USDOS) data. The analyses include:\
\
1. Processing USDOS data\
2. Running sensitivity models\
3. Generating plots and tables\
\
## Folder Structure\
The project assumes the following directory structure:\
\
- `Dependencies`: Contains necessary R scripts for sensitivity analyses.\
- `Files_To_Process`: Directory for storing USDOS data files.\
- `Output_Files`: Output directory for saving figures and tables.\
\
## Files\
The main scripts included in the project are:\
\
- `FlexibleControl_SensitivityAnalysis_Final.R`: Shell script for importing USDOS sensitivity runs, running PRCC and linear models on model output, and generating figures.\
\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 `FlexibleControl_SensitivityAnalysis_Final.R` depends on the following R scripts to complete these tasks:\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 - `loadDependencies_postProcessing.R`: Loads necessary dependencies.\
- `import_SensitivityRuns.R`: Imports sensitivity runs from the `Files_To_Process` directory.\
- `run_sensitivityModels.R`: Runs PRCC and linear models.\
\
## Getting Started\
To use this project:\
\
1. Clone the repository to your local machine.\
2. Set the working directory to the project location.\
3. Execute the provided R scripts in the specified order.\
\
Note: Required packages will be automatically installed and loaded.\
\
## Important Notes\
- Data files from importing and concatenating USDOS output will be left in the directory in which `FlexibleControl_SensitivityAnalysis_Final.R` is in.\
\
## Contact Information\
For questions or comments, please contact the project owner:\
- Email: sm.smith@colostate.edu\
}