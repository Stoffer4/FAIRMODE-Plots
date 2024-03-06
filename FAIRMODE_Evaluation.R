#*************************************#
# FAIRMODE evaluation - Thunis, 2022  #
#*************************************#

# Source the given file to obtain necessary functions:
source("FAIRMODE_Evaluation_functions.R")

ProgramInitialization() # Load packages, set correct working directory, and set a common plotting theme

# Read DELTA tool demo time series data if necessary:
Data <- ReadDELTAData()

## Setup: ####

Pol        <- "NO2" # Pollutant. Choose between "NO2", "O3", "PM2.5", "PM10"
OutputDir  <- "FAIRMODE_Evaluation_Plots/" # Name of the relative output directory of plots to be saved
OutputFile <- FALSE # If not FALSE, "OutputFile" overwrites the default file name. If FALSE, the default file name is used
SavePlot   <- TRUE # TRUE: Saves the plots

## Format the data: 

Data <- FormatDELTAData(Data, Pol) 

# Compute the daily maximum of 8H rolling averages (only for O3):
if (Pol == "O3") Data <- DailyMaxAvg8h(Data, GroupedCols = c("Station", "StationInfo"), mod, obs, date, Pol)

## MQI and other statistics: ####

# Return a report with FAIRMODE statics:
StatRep <- FAIRMODEStat(Data, U_Par, Pol)

## Target plot: ####

PlotPoint <- 2 # 1: Standard plot. 2: Each station point is uniquely identifiable (only applicable for fewer than app. 100 stations)

TargetPlot(StatRep, PlotPoint, NStations, OutputDir, OutputFile, SavePlot)

## Summary report: ####

PointSize <- 1.5 # Size of points in each subplot of the summary report

SummaryReport(StatRep, Pol, PointSize, OutputDir, OutputFile, SavePlot)
