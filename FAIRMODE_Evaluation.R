#*************************************#
# FAIRMODE-Plots - Model Benchmark    #
#*************************************#

## Author:  Christopher Andersen
## Address: Aarhus University, Department of Environmental Science
## Email:   chan@envs.au.dk

## References:

# Janssen, S., Thunis, P., FAIRMODE Guidance Document on Modelling Quality Objectives and Benchmarking (version 3.3), 
# EUR 31068 EN, Publications Office of the European Union, Luxembourg, 2022, ISBN 978-92-76-52425-0, doi:10.2760/41988, JRC129254

# Thunis, P., Cuvelier, C., DELTA Version 7.0 - Concepts / User's Guide / Diagrams, 2022. 

#********************************************************************************************************************************#

# Source the given file to obtain necessary functions:
source("FAIRMODE_Evaluation_functions.R")

# Load packages, set correct working directory, and set a common plotting theme: 
ProgramInitialization() 

UsePrint <- TRUE # If TRUE: Print additional information when calling "ReadDELTAData()" and "FAIRMODEStat()"

# Read DELTA tool example data (if required): 
Data <- ReadDELTAData(UsePrint)

## Setup: ####

Pol        <- "O3" # Pollutant. Choose between "NO2", "O3", "PM2.5", "PM10"
OutputDir  <- "FAIRMODE_Evaluation_Plots/" # Name of the relative output directory of plots to be saved
OutputFile <- FALSE # If not FALSE, "OutputFile" overwrites the default file name. If FALSE, the default file name is used
SavePlot   <- TRUE # TRUE: Saves the plots

# Format DELTA tool example data (if required): 
Data2 <- FormatDELTAData(Data, Pol, UsePrint) 

## MQI and other statistics: ####

# To create a target diagram and a summary report, start from a data set "Data2" with this overall structure (DELTA tool example).
# The order of the column is not important:

# NO2: 

# date      mod   obs             Station StationInfo
# <POSc>    <num> <num>              <char>      <char>
# 1: 2005-01-01 01:00:00 49.34645    67 MODENA_XX_SETTEMBRE       DELTA
# 2: 2005-01-01 02:00:00 47.94696    67 MODENA_XX_SETTEMBRE       DELTA
# 3: 2005-01-01 03:00:00 46.77987    61 MODENA_XX_SETTEMBRE       DELTA
# 4: 2005-01-01 04:00:00 45.49080    55 MODENA_XX_SETTEMBRE       DELTA
# 5: 2005-01-01 05:00:00 44.92197    57 MODENA_XX_SETTEMBRE       DELTA
# 6: 2005-01-01 06:00:00 45.48383    61 MODENA_XX_SETTEMBRE       DELTA

# Return a report with FAIRMODE statistics/quality indicators:
StatRep <- FAIRMODEStat(Data2, U_Par, Pol)

## Target plot: ####

Version   <- "DELTA" # Write a version name to be used for target diagram title
PlotPoint <- 2 # 1: Standard plot. 2: Each station point is uniquely identifiable (only applicable for fewer than app. 100 stations)

TargetPlot(StatRep, Version, PlotPoint, OutputDir, OutputFile, SavePlot)

## Summary report: ####

Version   <- "DELTA" # Write a version name to be used for target diagram title
PointSize <- 1.5 # Size of points in each subplot of the summary report

SummaryReport(StatRep, Pol, Version, PointSize, OutputDir, OutputFile, SavePlot)
