# Selfing-rate
R functions used to perform the analysis of MLTR, COLONY and RMES output, and COLONY simulation.

Links to software:  
- *MLTR*: http://genetics.forestry.ubc.ca/ritland/programs.html  
- *COLONY2*: https://www.zsl.org/science/software/colony  
- *RMES*: http://www.cefe.cnrs.fr/index.php/fr/recherche/accueil-dpt-ecologie-evolutive/genetique-et-ecologie-evolutive/800-gge/gge-chercheurs/196-patrice-david  

# Examples:
## Parsing results produced with MLTR v.3.4
Single population - run `MLTR_extract_single-pop.R`
Example files: MLTR_sample1a.out, MLTR_sample1b.out, MLTR_sample1c.out)

Multiple populations (MLTR grouping was enabled) - run `MLTR_extract_mult-pop.R`
Example file: MLTR_sample2.out

## Parsing results produced with RMES
Copy results from the RMES output window and save them to file, run `RMES_extract.R`
Example file: RMES_default_output.txt

## Extraction of selfers from the COLONY simulation
To determine which individuals are the results of selfing in the COLONY simulation - run `Colony_Selfer.R`
Example file: COLONY_simulation_Selfer.Selfer

## Preparation of selfing matrix for COLONY simulation
`Colony_PrepareRandomMatingMatrix.R` - these are internal functions which are used in `COLONY_PrepareInputFiles.R`
Input parameters for the mating.matrix function:
 - n = dimensions of mating matrix (for monoecious plants number of males = number of females)
 - s = desired selfing rate (diagonal elements)
 - cs = vector of known offspring numbers from female (sum of columns)

## Batch preparation of input files for COLONY simulation
Directory with input data should contain:
 - Design matrix ("COLONY_simulation_Descr.txt")
 - Genetic markers information for each population ("Markers_<PopulationName>.txt")
 - Allele frequency data for each population ("<PopulationName>_AlleleFreq.txt")

*NB:* Population names should be the same as in design matrix ('Meadow' column)

Run `COLONY_PrepareInputFiles.R`

Several folders (5 replicates by default) will be created for each population.

Copy COLONY files (impi.dll, libguide40.dll, libiomp5md.dll, colony2s.exe, simu2.exe) to each folder with input data, run simu2.exe (from batch file).

**NB.** Some of the parameters (path to the COLONY folder, etc.) are hard-coded in `COLONY_PrepareInputFiles.R` and should be tuned for your current project.

## Accuracy estimation of COLONY simulation
Run `Colony_AccuracyEstimation.R`
Example file: COLONY_simulation_Accuracy.Accuracy



# Prerequisites:
Packages [plyr](https://cran.r-project.org/web/packages/plyr/index.html) and [xlsx](https://cran.r-project.org/web/packages/xlsx/index.html)

 `install.packages(c("plyr", "xlsx"))`




