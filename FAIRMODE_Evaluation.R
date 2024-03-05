#*************************************#
# FAIRMODE evaluation - Thunis, 2022  #
#*************************************#

# Source the given file to obtain necessary functions:
source("FAIRMODE_Evaluation_functions.R")

ProgramInitialization() # Load packages, set correct working directory, and set a common plotting theme

## Setup: ####

Pol      <- "NO2" # "NO2", "O3", "PM2.5", "PM10"
SavePlot <- TRUE # TRUE: Saves the plots

# Read and format time series data if necessary:
Data <- ReadDELTAData(Pol)

Data <- FormatDELTAData(Data, Pol)

## MQI and other statistics: ####

# Return a report with FAIRMODE statics:
StatRep <- FAIRMODEStat(Data, U_Par, Pol)

## Target plot: ####

PlotPoint <- 2 # 1: Standard plot. 2: Each station point is uniquely identifiable (only applicable for fewer than app. 100 stations)

TargetPlot(StatRep, PlotPoint, NStations, SavePlot)

## Summary report: ####

PointSize <- 1.5 # Size of points in each subplot of the summary report

SummaryReport(StatRep, Pol, PointSize, SavePlot)