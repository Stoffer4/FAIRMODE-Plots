#*************************************#
# FAIRMODE - Model Benchmark          #
#*************************************#

## References:

# Janssen, S., Thunis, P., FAIRMODE Guidance Document on Modelling Quality Objectives and Benchmarking (version 3.3), 
# EUR 31068 EN, Publications Office of the European Union, Luxembourg, 2022, ISBN 978-92-76-52425-0, doi:10.2760/41988, JRC129254

# Thunis, P., Cuvelier, C., DELTA Version 7.0 - Concepts / User's Guide / Diagrams, 2022. 

#********************************************************************************************************************************#

# Source the given file to obtain necessary functions:
source("FAIRMODE_Evaluation_functions.R")

ProgramInitialization() # Load packages, set correct working directory, and set a common plotting theme

UsePrint <- TRUE # If TRUE: Print additional information when calling "ReadDELTAData()" and "FAIRMODEStat()"

# Read DELTA tool demo time series data if necessary:
Data <- ReadDELTAData(UsePrint)

## Setup: ####

Pol        <- "NO2" # Pollutant. Choose between "NO2", "O3", "PM2.5", "PM10"
OutputDir  <- "FAIRMODE_Evaluation_Plots/" # Name of the relative output directory of plots to be saved
OutputFile <- FALSE # If not FALSE, "OutputFile" overwrites the default file name. If FALSE, the default file name is used
SavePlot   <- TRUE # TRUE: Saves the plots

## Format the data: 

Data2 <- FormatDELTAData(Data, Pol, UsePrint) 

## MQI and other statistics: ####

# Return a report with FAIRMODE statics:
StatRep <- FAIRMODEStat(Data2, U_Par, Pol)

## Target plot: ####

PlotPoint <- 2 # 1: Standard plot. 2: Each station point is uniquely identifiable (only applicable for fewer than app. 100 stations)

TargetPlot(StatRep, PlotPoint, NStations, OutputDir, OutputFile, SavePlot)

## Summary report: ####

PointSize <- 1.5 # Size of points in each subplot of the summary report

SummaryReport(StatRep, Pol, PointSize, OutputDir, OutputFile, SavePlot)
