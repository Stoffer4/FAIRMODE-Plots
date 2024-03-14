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

Pol        <- "PM10" # Pollutant. Choose between "NO2", "O3", "PM2.5", "PM10"
OutputDir  <- "FAIRMODE_Evaluation_Plots/" # Name of the relative output directory of plots to be saved
OutputFile <- FALSE # If not FALSE, "OutputFile" overwrites the default file name. If FALSE, the default file name is used
SavePlot   <- TRUE # TRUE: Saves the plots

# Format DELTA tool example data (if required): 
Data2 <- FormatDELTAData(Data, Pol, UsePrint) 

## MQI and other statistics: ####

# To create a target diagram and a summary report, start from a data set "Data2" with this overall structure (DELTA tool example).
# The order of the column is not important:

# NO2 (hourly averages): 

#    date                Station               StationInfo  obs  mod
#    <POSc>              <char>                <char>      <num> <num>
# 1: 2005-01-01 00:00:00 MODENA_XX_SETTEMBRE   DELTA        67   49.34645
# 2: 2005-01-01 01:00:00 MODENA_XX_SETTEMBRE   DELTA        67   47.94696
# 3: 2005-01-01 02:00:00 MODENA_XX_SETTEMBRE   DELTA        61   46.77987
# 4: 2005-01-01 03:00:00 MODENA_XX_SETTEMBRE   DELTA        55   45.49080
# 5: 2005-01-01 04:00:00 MODENA_XX_SETTEMBRE   DELTA        57   44.92197
# 6: 2005-01-01 05:00:00 MODENA_XX_SETTEMBRE   DELTA        61   45.48383

# PM2.5/PM10/O3 (daily averages):

#   date                Station                StationInfo  obs   mod
#   <dttm>              <chr>                  <chr>        <dbl> <dbl>
# 1 2005-01-01 00:00:00 ALESSANDRIA_Liberta    DELTA        66.5  29.1
# 2 2005-01-01 00:00:00 ALESSANDRIA_Nuova_orti DELTA        92    29.1
# 3 2005-01-01 00:00:00 ASTI_DACQUISTO         DELTA        37    27.0
# 4 2005-01-01 00:00:00 Alba                   DELTA        41    20.1
# 5 2005-01-01 00:00:00 Arese                  DELTA       242    61.6
# 6 2005-01-01 00:00:00 BIELLA_Sturzo          DELTA        38    13.9

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
