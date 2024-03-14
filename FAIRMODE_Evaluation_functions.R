#### User-defined functions: ####

# Function to load packages, set correct working directory, and define a common plotting theme:
ProgramInitialization <- function(){
  
  # List of required packages: 
  ListofPackages <- c("ggplot2", "RColorBrewer", "ggprism", "patchwork", "cowplot", "dplyr", "zoo", "stringr", "data.table", 
                      "tidyr", "lubridate", "ncdf4", "DescTools")
  
  # Packages not installed: 
  NewPackages <- ListofPackages[!(ListofPackages %in% installed.packages()[ , "Package"])]
  
  if(length(new.packages)) install.packages(NewPackages) # Install required packages
  
  invisible(sapply(ListofPackages, library, character.only = TRUE)) # Load packages
  
  # Plotting theme for target diagram:
  ThemeAnalysisTarget <<- theme_bw() +
    theme(
      text                  = element_text(size = 12),
      plot.title            = element_text(size = 12, hjust = 0.5), # Center plot title
      legend.box.background = element_rect(fill = alpha("white", 0.6), linewidth = 0.5),
      legend.background     = element_rect(fill = alpha("white", 0.6)),
      legend.key            = element_rect(fill = alpha("white", 0.6)),
      legend.text           = element_text(hjust = 0), # Left align legend labels
      plot.margin           = margin(t = 2.5, r = 37.5, b = 2.5, l = 2.5, "mm"),
      legend.title          = element_blank()
    )
  
  # Plotting theme for summary report:
  ThemeAnalysisSummary <<- ThemeAnalysisTarget +
    theme(axis.text.y      = element_blank(), # Remove axis text and ticks on y axis
          axis.ticks.y     = element_blank(),
          axis.title.y     = element_text(angle = 0, size = 10, vjust = 0.5, hjust = 1, margin = margin(t = 0, r = 25, b = 0, l = 0)),
          axis.text.x      = element_text(size = 10),
          plot.title       = element_text(hjust = 0.5, size = 12),
          plot.margin      = margin(t = 2.5, r = 35, b = 2.5, l = 2.5, "mm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  
  ## FAIRMODE uncertainty parameters and percentile (quantile) values for all pollutants (FAIRMODE 2022, table 7):
  alpha <- c(0.2, 0.79, 0.25, 0.50) # alpha**2 is the fraction of the uncertainty around the reference value which is non-proportional
  # to the concentration level
  beta  <- 2 # Constant species-independent value chosen by FAIRMODE
  RV    <- c(200, 120, 50, 25) # Reference values (RV) (thresholds)
  U_RV  <- c(0.24, 0.18, 0.28, 0.36) # Relative measurement uncertainty estimated around a reference value (RV) for a given time averaging
  Np    <- c(5.2, 11, 20, 20) # Coefficient only used for seasonal/yearly averages accounting for the compensation of errors.
  # Np is the same for PM10 and PM2.5 due to insufficient data for PM2.5
  Nnp   <- c(5.5, 3, 1.5, 1.5) # Coefficient only used for seasonal/yearly averages accounting for the compensation of errors.
  # Nnp is the same Same for PM10 and PM2.5 due to insufficient data for PM2.5
  Perc  <- c(0.998, 0.929, 0.901, 0.901) # Percentile (quantile) values for all pollutants (FAIRMODE, chapter 7.1)
  
  # Collect all uncertainty parameters and percentile (quantile) values in a data.frame:
  U_Par      <<- data.frame(Pol = c("NO2", "O3", "PM10", "PM2.5"),
                            alpha = alpha, beta = beta, RV = RV, U_RV = U_RV, Np = Np, Nnp = Nnp, Perc = Perc)
  
  ## Threshold values. PM10: Daily, NO2: Hourly, O3: 8h daily maximum. No threshold for PM2.5:
  Thresholds <<- data.frame(PM10 = 50, NO2 = 200, O3 = 120)
  
} # End "ProgramInitialization()"

ReadDELTAData <- function(UsePrint){
  
  ## Observed concentrations:
  
  Files <- list.files(path = "FAIRMODE_DELTA_Data/monitoring/demo", pattern = ".csv", full.names = T) # Observations files to read
  
  Columns <- c("year", "month", "day", "hour", "Station", "NO2", "O3", "PM10", "PM2.5") # Columns to keep
  
  # Read all the observation files and create a column for each file, specifying its station:
  Tmp <- lapply(Files, function(x) {fread(x, fill = TRUE) %>%
      # The last column of each file will be NA due to the end-of-line ";". We therefore, only select columns where all values are not NA:
      select(where(~ !all(is.na(.)))) %>%
      filter(year != "<EOF>") %>% # Some "EOFs" have to be removed
      # Create a station column from each file name:
      mutate(Station = str_remove(str_split(x, pattern = "/", simplify = TRUE) # Split by "/"
                                  [length(str_split(Files[1], pattern = "/", simplify = TRUE))], ".csv"))}) # Remove ".csv"
  
  Tmp2 <- rbindlist(Tmp, fill = TRUE) # Rowbind all the files
  
  colnames(Tmp2) <- str_replace(colnames(Tmp2), "PM25", "PM2.5") # Change PM2.5 column header to the correct name format
  
  # Format the observations (select required columns, etc):
  Obs <- Tmp2 %>% select(all_of(Columns)) %>%
    mutate(hour = hour - 1) %>% # Format the hour column such that the first and last day contain the correct 24 hours 
    tidyr::unite("date", c(year, month, day, hour), sep = "_") %>%
    mutate(date = ymd_h(date)) %>%
    select(date, Station, c(NO2, O3, PM2.5, PM10)) %>% # Select important pollutants
    mutate(across(.cols = c(NO2, O3, PM2.5, PM10), .fns = ~ na_if(., -999))) # Convert -999 for all species (representing NAs) to NA
  
  ## Modeled concentrations:
  
  nc_file <- nc_open("FAIRMODE_DELTA_Data/modeling/demo/2005_MOD1_TIME.cdf") # Open the file with modeled concentrations
  
  VarNames <- names(nc_file$var) # Get the variables of the netCDF file
  
  if (UsePrint){
    print("Variables:")
    print(VarNames)
    print("Dimensions:")
    print(names(nc_file$dim)) # Check the dimensions of the netCDF file
  }
  
  Att <- ncatt_get(nc_file, varid = 0) # Return global attributes of the netCDF file (this contains ASCII for column names)
  
  # Create a vector of all date times:
  DateVector <- seq.POSIXt(as.POSIXct(paste0(Att$Year, "-01-01 00:00:00"), tz = "UTC"), by = "hour", length.out = Att$EndHour + 1)
  
  ColNamesASCII <- Att$Parameters # Obtain the column names in ASCII code
  ColNames      <- AscToChar(ColNamesASCII[-length(ColNamesASCII)]) # Return a character for each ASCII code (integer) supplied (we also
  # delete the last whitespace)
  ColNames      <- unlist(str_split(ColNames, " ")) # Convert into character vector
  ColNames      <- str_replace(ColNames, "PM25", "PM2.5") # Change PM2.5 column header to the correct name format
  
  Mod <- vector(mode = "list", length = length(VarNames)) # List to store the modeled data.frame of each station
  cnt <- 1 # Counter
  for (ii in VarNames){ # Read data for each station
    
    Tmp <- ncvar_get(nc_file, varid = ii) # Obtain the 2D field of the given variable
    Tmp <- data.frame(t(Tmp)) # Convert to tibble
    
    colnames(Tmp) <- ColNames
    
    Mod[[cnt]] <- Tmp %>% mutate(Station = ii) %>% # Create a station column
      mutate(date = DateVector) %>% # Create a date column
      select(date, Station, c(NO2, O3, PM2.5, PM10)) # Select only relevant pollutants
    
    cnt <- cnt + 1
  }
  
  nc_close(nc_file) # Close the netCDF file
  
  Mod <- rbindlist(Mod) # Concatenate all data.frames
  
  Stations  <- unique(Mod$Station) # Unique stations for where modeled data is available
  
  Data <- full_join(Mod, Obs, by = c("Station", "date"), suffix = c("_DELTA", "_Obs")) %>%
    filter(Station %in% Stations) %>% # Filter for stations where modeled data is available
    # Rename the concentration columns by transforming each suffix to a prefix:
    rename_with(.fn = function(x) {sapply(str_split(x, "_"), function(y) paste0(y[2], "_", y[1]))},
                .cols = contains(c("NO2", "O3", "PM2.5", "PM10"))) %>%
    mutate(StationInfo = "DELTA") # Placeholder
  
} # End "ReadDELTAData()"

# Function to format the data. The function creates required columns, compute daily averages for PM2.5 and PM10, and filter for stations
# with enough data coverage:
FormatDELTAData <- function(Data, Pol, UsePrint){
  
  # If the observed variable does not exist, terminate the program:
  if (!paste0("Obs_", Pol) %in% names(Data)){
    stop(paste0(Pol, " measurements do not exist for the given stations (file) for the given period!!!"))
  }
  
  Data2 <- Data %>% select(date, mod = paste0("DELTA_", Pol), obs = paste0("Obs_", Pol), Station, StationInfo) %>%
    mutate(date2 = floor_date(date, "day")) # Create a day column
  
  # If the pollutant is either "PM10" or "PM2.5": 
  if (Pol %in% c("PM10", "PM2.5")){
    
    # Determine the data coverage for each station and each day:
    DataCoverage <- Data2 %>% group_by(Station, StationInfo, date2) %>%
      summarize(DataCoverage = sum(!is.na(obs))/n()) %>%
      arrange(desc(DataCoverage))
    
    # Compute daily means of observed and modeled concentrations:
    Data2 <- Data2 %>% mutate(date = floor_date(date, "day")) %>%
      group_by(date, Station, StationInfo) %>%
      summarize(obs = mean(obs, na.rm = TRUE), mod = mean(mod, na.rm = TRUE))
    
    # Join with coverage data and set "obs" for all days for each station with less than 75% observations to NA
    # (we still need to keep these records for later  computing the data coverage for the number of days in the period):
    Data2 <- left_join(Data2, DataCoverage, by = c("Station", "StationInfo", "date" = "date2")) %>%
      mutate(obs = ifelse(DataCoverage < 0.75, NA, obs))
    
  # Take appropriate averages for "O3" and take into account the data coverage for averaging:
  } else if (Pol == "O3"){
    
    # Compute the daily maximum of 8H rolling averages:
    Data2 <- DailyMaxAvg8h(Data = Data2, GroupedCols = c("Station", "StationInfo"), mod, obs, date)
    
  } # End if statement over "PM10", "PM2.5", and "O3"
    
  ## Determine the data coverage of each station and remove stations with less than 75%:
  DataCoverage <- Data2 %>% group_by(Station) %>%
    summarize(DataCoverage = sum(!is.na(obs))/n()) %>% 
    arrange(desc(DataCoverage)) # Data coverage
  
  if (UsePrint){
    print("DataCoverage:")
    print(DataCoverage)
  }

  # Find stations which have at least 75% data coverage: 
  StationsToKeep <- DataCoverage %>% filter(DataCoverage >= 0.75) %>% 
    pull(Station) 
  
  if (UsePrint){
    print("Stations which have 75% data coverage in the given period:")
    print(StationsToKeep)
  }

  Data2 <- Data2 %>% filter(Station %in% StationsToKeep) %>% # Remove stations with less than 75% data coverage:
    select(-any_of(c("DataCoverage", "date2"))) # Remove the "DataCoverage" column if it exists
  
  NStations <- length(unique(Data2$Station)) # Number of stations
  
  # Stop validation if less than 5 stations:
  if (NStations < 5){
    stop(paste0("Only ", NStations, "stations for validation. The minimum is 5. The code is terminated"))
  } else if (UsePrint){
    print("")
    print("Enough stations to proceed validation")
  }
  
  # Reorder columns: 
  Data2 <- Data2 %>% relocate(date, Station, StationInfo, obs, mod) %>% 
    ungroup()
  
  return(Data2)
  
} # End "FormatData()"

# Function to compute daily maximum of 8H rolling averages (only for O3):
DailyMaxAvg8h <- function(Data, GroupedCols, mod, obs, date){
  
  # Function to determine the mean if at least 6 concentrations (out of 8) are non-NA:
  mean2 <- function(x){
    if (sum(!is.na(x)) >= 6){ # If at least 6 concentrations are non-NA:
      mean(x, na.rm = TRUE)
    } else {
      NA
    }
  }
  
  # Function to compute the maximum of a vector. If all values are "NA", return "NA" instead of "inf" as is the usual behavior 
  # of "max"(): 
  max2 <- function(x) ifelse( !all(is.na(x)), max(x, na.rm = TRUE), NA) 
  
  # Determine the 8H backward rolling average of measured and modeled concentrations:
  Data2 <- Data %>% group_by(across(all_of(GroupedCols))) %>%
    mutate(obs_8HourMean = rollapply({{obs}}, width = 8, FUN = mean2, partial = TRUE,
                                     align = "right"),
           mod_8HourMean = rollapply({{mod}}, width = 8, FUN = mean2, partial = TRUE,
                                     align = "right"))
  
  # Determine the data coverage of "obs_8HourMean" for each day:
  DataCoverage <- Data2 %>% group_by(across(c(all_of(GroupedCols), date2))) %>%
    summarize(DataCoverage = sum(!is.na(obs_8HourMean))/n()) %>%
    arrange(desc(DataCoverage))
  
  # Join with coverage data and set "obs_8HourMean" for all days for each station with less than 75% observations to NA
  # (we still need to keep these records for later computing the data coverage for the number of days in the period):
  Data2 <- merge(Data2, DataCoverage, all.x = TRUE) %>%  # Merge "Data2" with "DataCoverage"
    mutate(obs_8HourMean = ifelse(DataCoverage < 0.75, NA, obs_8HourMean))
  
  # Determine the maximum 8-hour mean for both measured and modeled concentrations for days with at least 75% observations:
  Data2 <- Data2 %>% group_by(across(c(all_of(GroupedCols), date2))) %>%
    summarize(obs = max2(obs_8HourMean), mod = max2(mod_8HourMean)) %>%
    mutate(date = date2) %>%
    select(-date2)
  
  return(Data2)
  
} # End of "DailyMaxAvg8h()"

# Function to compute FAIRMODE statistics:
FAIRMODEStat <- function(Data, U_Par, Pol){
  
  # Uncertainty parameters and percentile (quantile) values for the given pollutant:
  U_Par_tmp <- U_Par %>% filter(Pol == !!Pol)
  
  # Drop rows with missing observations:
  Data <- Data %>% drop_na(obs)
  
  ## Number of daily exceedances per station:
  
  if (Pol %in% c("NO2", "O3", "PM10")){ # Only exceedances for these pollutants
    
    Exceedances <- Data %>%
      mutate(date2       = as.Date(date), # Create a day columns
             Exceedances = if_else(obs > Thresholds[ , Pol], 1, 0)) %>% # 1 if an exceedance is observed, 0 otherwise
      group_by(Station, date2) %>%
      summarize(NExceedances = if_else(sum(Exceedances) >= 1, 1, 0)) %>% # 1 if at least one exceedances took place during a day
      group_by(Station) %>%
      summarize(NExceedances = sum(NExceedances)) # Number of dates per station where an exceedance was observed
    
  } else { # Exceedances are not shown for PM2.5 (see FAIRMODE p. 19)
    Exceedances <- data.frame(NExceedances = NA)
  }
  
  StationsToKeep <- unique(Data$Station) # Stations to keep
  
  # Expand the data.frame to include all stations (present for this pollutant) with zero exceedances (necessary for PM2.5):
  Exceedances <- merge(data.frame(Station = StationsToKeep), Exceedances, all.x = TRUE)
  
  # Create temporal statistics:
  Data2 <- Data %>%
    group_by(Station, StationInfo) %>%
    summarize(N          = n(), # Number of measurements for all stations combined
              M_Perc     = quantile(mod, U_Par_tmp$Perc), # Extreme value - percentile (quantile) for modeled concentrations
              O_Perc     = quantile(obs, U_Par_tmp$Perc), # Extreme value - percentile (quantile) for observed concentrations
              Mean_mod   = mean(mod), # Arithmetic average of modeled concentrations for all stations combined
              Mean_obs   = mean(obs), # Arithmetic average of observed concentrations for all stations combined
              BIAS       = Mean_mod - Mean_obs, # Bias
              Sigma_mod  = sqrt(1/N*sum((mod - Mean_mod)^2)), # Standard deviation of modeled concentrations
              Sigma_obs  = sqrt(1/N*sum((obs - Mean_obs)^2)), # Standard deviation of observed concentrations
              # Measurement uncertainty, FAIRMODE Eq. 38:
              RMS_U      = U_Par_tmp$U_RV*sqrt((1 - U_Par_tmp$alpha^2)*(Mean_obs^2 + Sigma_obs^2) + U_Par_tmp$alpha^2*U_Par_tmp$RV^2),
              # Percentile measurement uncertainty, FAIRMODE Eq. 37 with O_i = O_perc:
              U_O_Perc   = U_Par_tmp$U_RV*sqrt((1 - U_Par_tmp$alpha^2)*O_Perc^2 + U_Par_tmp$alpha^2*U_Par_tmp$RV^2),
              # 95th percentile uncertainty:
              U_O        = U_Par_tmp$U_RV*sqrt((1 - U_Par_tmp$alpha^2)/U_Par_tmp$Np*Mean_obs^2 +
                                                 U_Par_tmp$alpha^2*U_Par_tmp$RV^2/U_Par_tmp$Nnp),
              BIAS_Norm  = BIAS/(U_Par_tmp$beta*RMS_U), # Normalized bias
              CRMSE_Norm = sqrt(1/N*sum(((mod - Mean_mod) - (obs - Mean_obs))^2))/(U_Par_tmp$beta*RMS_U), # Normalized CRMSE
              RMSE       = sqrt(1/N*sum((obs - mod)^2)), # RMSE
              MQI        = RMSE/(U_Par_tmp$beta*RMS_U), # Model quality indicator
              MQI_yr     = abs(Mean_obs - Mean_mod)/(U_Par_tmp$beta*U_O), # MQI for year
              R          = cor(mod, obs, method = "pearson"), # Pearson correlation
              # Ratio to determine if CRMSE is dominated by R or by sigma. This expression is from DELTA version 7.0 user's guide and differ slightly
              # from the expression given by Janssen and Thunis (2022):
              Ratio      = abs(Sigma_mod - Sigma_obs)/(sqrt(Sigma_obs*Sigma_mod)*sqrt(2*(1 - R))),
              SigmaDomR  = if_else(Ratio > 1, 1, -1), # Indicator. If "SigmaDomR" > 1: Sigma dominates R. If "SigmaDomR" < 1: R dominates Sigma
              MPI_BIAS   = BIAS/(U_Par_tmp$beta*RMS_U), # MPI for bias, FAIRMODE Eq. 16, without absolute value
              MPI_R      = (1 - R)/(0.5*U_Par_tmp$beta^2*RMS_U^2/(Sigma_obs*Sigma_mod)), # MPI for correlation, Janssen and Thunis (2022) Eq. 17
              MPI_Sigma  = (Sigma_mod - Sigma_obs)/(U_Par_tmp$beta*RMS_U), # MPI for standard deviation, Janssen and Thunis (2022) Eq. 18, without absolute value
              MPI_Perc   = (M_Perc - O_Perc)/(U_Par_tmp$beta*U_O_Perc), # MPI for extreme events (exceedances), Janssen and Thunis (2022) Eq. 25 without absolute value
    ) %>% left_join(Exceedances, by = "Station") %>% # Join with "Exceedances" data.frame
          mutate(Country = str_sub(Station, start = 3, end = 4)) # Add a country column (only makes sense for DEHM data)
  
  NStations <- nrow(Data2) # Number of stations
  
  StartDate <- min(Data$date) # Start date of data
  EndDate   <- max(Data$date) # End date of data
  
  Data2 <- Data2 %>% mutate(CRMSE_Norm = CRMSE_Norm*SigmaDomR) # Determine if a point should be placed on the right or left side of the plot
  
  # Compute MQI90 (the 90th percentile value of MQI) according to a linear interpolation (Janssen and Thunis (2022) Eq. (9)):
  S90   <- as.integer(NStations*0.9)
  dist  <- NStations*0.9 - S90
  
  MQIList <- sort(Data2 %>% pull(MQI)) # Ranked MQI list
  
  MQI90 <- MQIList[S90] + (MQIList[S90+1] - MQIList[S90])*dist # Linear interpolation
  
  NPointsMQI90 <- nrow(Data2 %>% filter(MQI > 1)) # Number of stations with MQI > 1
  
  ## MQI90 for seasonal/yearly statistics:
  
  MQIList <- sort(Data2 %>% pull(MQI_yr)) # Ranked MQI list
  
  Y90 <- MQIList[S90] + (MQIList[S90+1] - MQIList[S90])*dist # Linear interpolation
  
  NPointsY90 <- nrow(Data2 %>% filter(MQI_yr > 1)) # Number of stations with MQI_yr > 1
  
  # Create a nested list with important function output:
  StatRep <- list(Data       = Data2,
                  MQI        = list(StartDate = StartDate, EndDate = EndDate, MQI90 = MQI90, Y90 = Y90, NPoints = NStations,
                                    NPointsMQI90 = NPointsMQI90, NPointsY90 = NPointsY90),
                  Parameters = as.list(U_Par_tmp))
  
  return(StatRep)
  
} # End "FAIRMODEStatistics()"

# Function to create a target plot:
TargetPlot <- function(StatRep, Version, PlotPoint, OutputDir, OutputFile, SavePlot){
  
  if ((PlotPoint == 2) & (StatRep$MQI$NPoints > 60)) stop("Too many points for PlotPoint = 2! Set it equal to 1 instead")
  
  ## Upper-left text box for plot:
  if (StatRep$MQI$NPointsMQI90 == 1) {
    StationString1 <- "station"
  } else {
    StationString1 <- "stations"
  }
  if (StatRep$MQI$NPointsY90 == 1) { # String for yearly data
    StationString2 <- "station"
  } else {
    StationString2 <- "stations"
  }
  
  PlotString <- paste0(StatRep$MQI$NPointsMQI90, " ", StationString1, " with MQI > 1 \n", "MQI90 = ", round(StatRep$MQI$MQI90, 3), "\n",
                       StatRep$MQI$NPointsY90, " ", StationString2, " with MQIYr > 1 \n", "Y90 = ", round(StatRep$MQI$Y90, 3))
  
  ## Lower-left text box for plot:
  lab <- paste0("\u03B1 = ", StatRep$Parameters$alpha, "\n",
                "\u03B2 = ", StatRep$Parameters$beta, "\n",
                "RV = ", StatRep$Parameters$RV, "\n",
                "U(RV) = ", StatRep$Parameters$U_RV)
  
  ## List of stations with MQI > 1:
  StationsMQI1 <- StatRep$Data %>% filter(MQI > 1) %>% pull(Station)
  
  if (length(StationsMQI1) > 26) StationsMQI1 <- StationsMQI1[1:26] # If more than 26 of these stations, only show the first 26
  
  StationsMQI1 <- c("Stations with MQI > 1:", StationsMQI1) # Append a header
  
  StationsMQI1 <- data.frame(Stations = StationsMQI1, x = 2.2, y = seq(2, by = -0.15, length = length(StationsMQI1)))
  
  ## Plot title:
  
  # Choose the time resolution string:
  if (Pol == "NO2"){
    ResString <- "hourly mean [\u03BCgm\u207B\u00B3]" # Header string for target plot
  } else if (Pol == "O3"){
    ResString <- "8h moving average daily maximum [\u03BCgm\u207B\u00B3]" # Change target plot header string
  } else if (Pol %in% c("PM2.5", "PM10")){
    ResString <- "daily mean [\u03BCgm\u207B\u00B3]" # Change target plot header string
  }
  
  PlotTitle <- paste0(Version, " analysis - ", StatRep$MQI$NPoints, " stations\nSurface ", Pol, " ", ResString, " \n",
                      round_date(StatRep$MQI$StartDate, unit = "day"), " to ", round_date(StatRep$MQI$EndDate, unit = "day"))
  
  # Colour, depending on if the model is fit for purpose (green4) or not (red):
  if ((StatRep$MQI$MQI90 < 1) & (StatRep$MQI$Y90 < 1)){
    ColorPassed <- "green4"
  } else {
    ColorPassed <- "red"
  }
  
  # List of shapes for target plot:
  ShapeList <- setNames(0:length(unique(StatRep$Data$StationInfo)), unique(StatRep$Data$StationInfo))
  
  # Choose the point size according to "PlotPoint":
  if (PlotPoint == 1){
    PointSize <- 2.5
  } else if (PlotPoint %in% c(2, 3)){
    PointSize <- 2
  }
  
  # Specify a unique combination of "fill" and "shape" (only used for PlotPoint == 2 and PlotPoint == 3):
  Fill       <- brewer.pal(12, "Paired")
  Shape      <- 21:25
  FillShapes <- expand_grid(Fill, Shape) # Determine all unique combinations
  
  ## Create the base target plot:
  Plot <- StatRep$Data %>% ggplot() +
    # We create filled circles with "geom_polygon()" instead of "geom_circle()", since there are problems with "alpha" in "geom_circle()":
    geom_polygon(data = data.frame(x = cos(seq(0, 2*pi, length.out = 1000)), y = sin(seq(0, 2*pi, length.out = 1000))), # Outer circle
                 aes(x = x, y = y), fill = "forestgreen", color = "black",  linetype = "dashed", alpha = 0.4) +
    geom_polygon(data = data.frame(x = cos(seq(0, 2*pi, length.out = 1000))/2, y = sin(seq(0, 2*pi, length.out = 1000))/2), # Inner circle
                 aes(x = x, y = y), color = "black",  linetype = "dashed", alpha = 0) +
    geom_abline(slope = 1, intercept = 0) +
    geom_abline(slope = -1, intercept = 0) +
    scale_x_continuous(limits = c(-2, 2),
                       name = "CRMSE / \u03B2RMS\u1D64", breaks = c(-2, -1, 0, 1, 2), labels = c("2", "1", "0", "1", "2")) +
    scale_y_continuous(limits = c(-2, 2),
                       name = "Mean Bias / \u03B2RMS\u1D64") +
    annotation_ticks(sides = "tr") +
    ggtitle(PlotTitle) +
    coord_fixed(expand = FALSE, clip = "off")
  
  
  
  ## Add the point layer:
  if (PlotPoint == 1){
    
    Plot <- Plot + geom_point(aes(x = CRMSE_Norm, y = BIAS_Norm, shape = StationInfo), color = "blue", size = PointSize) +
      scale_shape_manual(values = ShapeList)
    
  } else if (PlotPoint == 2){
    
    set.seed(123) # Ensure that the same combination of "FillShapes" is always used
    
    FillShapes <- slice_sample(FillShapes, n = StatRep$MQI$NPoints) # Choose the number of stations for each combinations
    
    Plot <- Plot + geom_point(aes(x = CRMSE_Norm, y = BIAS_Norm, shape = Station, fill = Station), color = "black", size = PointSize) +
      scale_shape_manual(values = FillShapes$Shape) +
      scale_fill_manual(values = FillShapes$Fill)
    
  } else if (PlotPoint == 3){
    
    set.seed(123) # Ensure that the same combination of "FillShapes" is always used
    
    FillShapes <- slice_sample(FillShapes, n = length(unique(StatRep$Data$Country))) # Choose the number of stations for each combinations
    
    Plot <- Plot + geom_point(aes(x = CRMSE_Norm, y = BIAS_Norm, color = Country, fill = Country, shape = Country), size = PointSize) +
      scale_shape_manual(values = FillShapes$Shape) +
      scale_fill_manual(values = FillShapes$Fill)
    
  } # End if statement over point layer
  
  # Add annotations:
  Plot <- Plot + annotate("text", x = -1.8, y = 0, label = "R") +
    annotate("text", x = 1.8, y = 0, label = "SD", size = PointSize + 1) +
    annotate("label", x = -Inf, y = Inf, label = PlotString, hjust = 0, vjust = 1, colour = ColorPassed, size = PointSize + 1, alpha = 0.8) +
    annotate("label", x = -Inf, y = -Inf, label = lab, hjust = 0, vjust = 0, size = PointSize + 1) +
    ThemeAnalysisTarget
  
  # Add a theme:
  if (PlotPoint == 1){
    Plot <- Plot + theme(legend.position = c(1, 1), legend.justification = c("right", "top"))
  } else if (PlotPoint == 2){ # Change the legend:
    Plot <- Plot + theme(legend.position = "bottom", legend.text = element_text(size = 4), legend.key.size = unit(0.1, "mm"),
                         text = element_text(size = 10))
  } else if (PlotPoint == 3){ # Change the legend:
    Plot <- Plot + theme(legend.position = "bottom", legend.text = element_text(size = 8), legend.key.size = unit(0.1, "mm")) +
      guides(col = guide_legend(ncol = 10), fill = guide_legend(ncol = 10), shape = guide_legend(ncol = 10))
  }
  
  # Add stations name where MQI > 1 if "PlotPoint == 1" or "PlotPoint == 3":
  if (PlotPoint %in% c(1, 3)){
    Plot <- Plot + geom_text(data = StationsMQI1, aes(x = x - 0.2, y = y, label = Stations), hjust = -0.05, size = 3)
  } else {
    Plot <- Plot + theme(plot.margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, "mm"),)
  }
  
  print(Plot)
  
  # Save the plot:
  if (SavePlot){
    # Name of output file:
    if (OutputFile == FALSE){
      FileName <- paste0(OutputDir, "TargetPlot_", Version, "_", Pol, ".png")
    } else {
      FileName <- paste0(OutputDir, OutputFile)
    }
    print(paste0("Writing the file: ", FileName))
    ggsave(Plot, filename = FileName, height = 6.5, width = 6.5, unit = "in")
  }
  
} # End "TargetPlot()"

# Function to create a summary report:
SummaryReport <- function(StatRep, Pol, Version, PointSize, OutputDir, OutputFile, SavePlot){
  
  # Upper-right text box in report:
  if (StatRep$MQI$NPointsMQI90 == 1) {
    StationString1 <- "station"
  } else {
    StationString1 <- "stations"
  }
  
  # Ranges for dashed zones for values beyond proposed plot scales:
  Xrange  <- c(seq(2, 2.3, length.out = 30), rep(2.3, 15), seq(2.3, 2, length.out = 30))
  Xrange2 <- c(seq(2, 2.15, length.out = 30), rep(2.15, 15), seq(2.15, 2, length.out = 30))
  Yrange  <- c(rep(-1/25, 30), seq(-1/25, 1/25, length.out = 15), rep(1/25, 30))
  Yrange2 <- c(rep(-1/50, 30), seq(-1/50, 1/50, length.out = 15), rep(1/50, 30))
  
  ### Observation subplots:
  
  ## Observed mean - plot 1:
  
  # Points in the dashed zone:
  DashedZoneR <- StatRep$Data %>% filter(Mean_obs > 100) # Right
  
  Plot1 <- ggplot() +
    # Visualize all data points not in the dashed zones:
    geom_point(data = anti_join(StatRep$Data, DashedZoneR, by = "Station"),
               aes(x = Mean_obs, y = 0), color = "blue", size = PointSize) +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    geom_polygon(data = data.frame(x = 50*Xrange2, y = 50*Yrange2), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    ggtitle("Observations") +
    labs(x = NULL, y = "Observed\nmean") +
    coord_fixed(xlim = c(0, 100), ylim = c(-1 , 1), ratio = 1.5, expand = FALSE, clip = "off") +
    annotate("text", x = 105, y = -3.0, hjust = 0, label = "\u03BCgm\u207B\u00B3") +
    ThemeAnalysisSummary
  
  # Set a point in the right dashed zone if required:
  if (nrow(DashedZoneR) > 0) {
    Plot1 <- Plot1 + geom_point(aes(x = 100 + (50*max(Xrange2) - 100)/2, y = 0), color = "blue", size = PointSize)
  }
  
  ## Observed exceedances - plot 2:
  
  # Create a string for the left side of the exceedance plot:
  if (StatRep$Parameters$Pol %in% c("NO2", "O3", "PM10")){ # Only exceedances for these pollutants
    ExceedanceString <- paste0("Observed\nexceedances\n(>", paste0(Thresholds %>% pull(Pol), ".0"), " \u03BCgm\u207B\u00B3)")
  } else { # For PM2.5, we don't have any threshold for exceedances:
    ExceedanceString <- "No exceedances \nshown for PM2.5"
  }
  
  # Points in the dashed zone:
  DashedZoneR <- StatRep$Data %>% filter(NExceedances > 100) # Right
  
  # First an empty plot is created, but it is later populated for all pollutants except PM2.5:
  Plot2 <- ggplot() +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    geom_polygon(data = data.frame(x = 50*Xrange2, y = 50*Yrange2), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    labs(x = NULL, y = ExceedanceString) +
    coord_fixed(xlim = c(0, 100), ylim = c(-1 , 1), ratio = 1.5, expand = FALSE, clip = "off") +
    annotate("text", x = 105, y = -3.0, hjust = 0, label = "days") +
    ThemeAnalysisSummary
  
  # Populate the plot with points, if not "PM2.5":
  if (StatRep$Parameters$Pol != "PM2.5") {
    
    # Visualize all data points not in the dashed zones:
    Plot2 <- Plot2 + geom_point(data = anti_join(StatRep$Data, DashedZoneR, by = "Station"),
                                aes(x = NExceedances, y = 0), color = "blue", size = PointSize)
    
    # Set a point in the dashed zone if required:
    if (nrow(DashedZoneR) > 0) {
      Plot2 <- Plot2 + geom_point(aes(x = 100 + (50*max(Xrange2) - 100)/2, y = 0), color = "blue", size = PointSize) # Right
    }
    
  }
  
  ### Temporal MPI subplots:
  
  MPIList <- c("MPI_BIAS", "MPI_R", "MPI_Sigma", "MPI_Perc") # List of temporal MPIs
  
  # Create a data.frame with all temporal MPIs and convert the data.frame to a long format:
  MPIDF <- StatRep$Data %>% ungroup() %>%
    select(all_of(MPIList)) %>%
    pivot_longer(cols = all_of(MPIList), names_to = "Stat", values_to = "Value") %>%
    mutate(Value = abs(Value)) # Convert to absolute value to determine MPI90
  
  ## Compute all temporal MPI90:
  S90   <- as.integer(StatRep$MQI$NPoints*0.9)
  dist  <- (StatRep$MQI$NPoints*0.9 - S90)
  
  MPIDF2 <- MPIDF %>% group_by(Stat) %>%
    summarize(MPI90          = sort(Value)[S90] + (sort(Value)[S90+1] - sort(Value)[S90])*dist, # Computing MPI90 from FAIRMODE interpolation
              StationsMQIgt1 = sum(Value > 1), # Number of stations having higher than MPI90
              CircleColor    = ifelse(MPI90 < 1, "green4", "red")) # Determine the circle color
  
  ## MPI for normalized bias - plot 3:
  MPIBox <- MPIDF2 %>% filter(Stat == "MPI_BIAS") # MPI information
  
  # Points in the dashed zones:
  DashedZoneR <- StatRep$Data %>% filter(MPI_BIAS > 2) # Right
  DashedZoneL <- StatRep$Data %>% filter(MPI_BIAS < -2) # Left
  
  Plot3 <- ggplot() +
    geom_rect(aes(xmin = -1/sqrt(2), xmax = 1/sqrt(2), ymin = -Inf, ymax = Inf), fill = "green4") + # The factor "1/sqrt(2)" is found on
    # page 17 in the report "DELTA Version 7.0" from 2022
    geom_rect(aes(xmin = -1, xmax = -1/sqrt(2), ymin = -Inf, ymax = Inf), fill = "orange") +
    geom_rect(aes(xmin = 1/sqrt(2), xmax = 1, ymin = -Inf, ymax = Inf), fill = "orange") +
    # Visualize all data points not in the dashed zones:
    geom_point(data = anti_join(anti_join(StatRep$Data, DashedZoneR, by = "Station"), DashedZoneL, by = "Station"),
               aes(x = MPI_BIAS, y = 0), color = "blue", size = PointSize) +
    annotate("text", x = 2.6, y = 0.15, label = "MPI90") +
    annotate("label", x = 2.6, y = 0, label = round(MPIBox$MPI90, 3), color = MPIBox$CircleColor) + # Colored MPI indicator on the RHS
    scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
    geom_polygon(data = data.frame(x = Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    geom_polygon(data = data.frame(x = -Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, LHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    ggtitle("Performance in time") +
    labs(x = NULL, y = "Bias Norm") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-1/25 , 1/25), ratio = 1.5, expand = FALSE, clip = "off") +
    ThemeAnalysisSummary
  
  # Set a point in the dashed zones if required:
  if (nrow(DashedZoneR) > 0) Plot3 <- Plot3 + geom_point(aes(x = 2 + (max(Xrange) - 2)/2, y = 0), color = "blue", size = PointSize) # Right
  if (nrow(DashedZoneL) > 0) Plot3 <- Plot3 + geom_point(aes(x = -2 + (min(-Xrange) + 2)/2, y = 0), color = "blue", size = PointSize) # Left
  
  ## MPI for normalized correlation - plot 4:
  MPIBox <- MPIDF2 %>% filter(Stat == "MPI_R") # MPI information
  
  # Points in the dashed zone:
  DashedZoneR <- StatRep$Data %>% filter(MPI_R > 2) # Right
  
  Plot4 <- ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = -Inf, ymax = Inf), fill = "green4") +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf), fill = "orange") +
    # Visualize all data points not in the dashed zone:
    geom_point(data = anti_join(StatRep$Data, DashedZoneR, by = "Station"), aes(x = MPI_R, y = 0), color = "blue", size = PointSize) +
    annotate("label", x = 2.3, y = 0, label = round(MPIBox$MPI90, 3), color = MPIBox$CircleColor) + # Colored MPI indicator on the RHS
    scale_x_continuous(breaks = seq(0, 2, 0.25)) +
    geom_polygon(data = data.frame(x = Xrange2, y = Yrange2), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    labs(x = NULL, y = "1-R Norm") +
    coord_fixed(xlim = c(0, 2), ylim = c(-1/50 , 1/50), ratio = 1.5, expand = FALSE, clip = "off") +
    ThemeAnalysisSummary
  
  # Set a point in the dashed zone if required:
  if (nrow(DashedZoneR) > 0) Plot4 <- Plot4 + geom_point(aes(x = 2 + (max(Xrange2) - 2)/2, y = 0), color = "blue", size = PointSize) # Right
  
  ## MPI for normalized standard deviation - plot 5:
  MPIBox <- MPIDF2 %>% filter(Stat == "MPI_Sigma") # MPI information
  
  # Points in the dashed zones:
  DashedZoneR <- StatRep$Data %>% filter(MPI_Sigma > 2) # Right
  DashedZoneL <- StatRep$Data %>% filter(MPI_Sigma < -2) # Left
  
  Plot5 <- ggplot() +
    geom_rect(aes(xmin = -1/sqrt(2), xmax = 1/sqrt(2), ymin = -Inf, ymax = Inf), fill = "green4") +
    geom_rect(aes(xmin = -1, xmax = -1/sqrt(2), ymin = -Inf, ymax = Inf), fill = "orange") +
    geom_rect(aes(xmin = 1/sqrt(2), xmax = 1, ymin = -Inf, ymax = Inf), fill = "orange") +
    # Visualize all data points not in the dashed zones:
    geom_point(data = anti_join(anti_join(StatRep$Data, DashedZoneR, by = "Station"), DashedZoneL, by = "Station"),
               aes(x = MPI_Sigma, y = 0), color = "blue", size = PointSize) +
    annotate("label", x = 2.6, y = 0, label = round(MPIBox$MPI90, 3), color = MPIBox$CircleColor) + # Colored MPI indicator on the RHS
    scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
    geom_polygon(data = data.frame(x = Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    geom_polygon(data = data.frame(x = -Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, LHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    labs(x = NULL, y = "StDev Norm") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-1/25 , 1/25), ratio = 1.5, expand = FALSE, clip = "off") +
    ThemeAnalysisSummary
  
  # Set a point in the dashed zones if required:
  if (nrow(DashedZoneR) > 0) Plot5 <- Plot5 + geom_point(aes(x = 2 + (max(Xrange) - 2)/2, y = 0), color = "blue", size = PointSize) # Right
  if (nrow(DashedZoneL) > 0) Plot5 <- Plot5 + geom_point(aes(x = -2 + (min(-Xrange) + 2)/2, y = 0), color = "blue", size = PointSize) # Left
  
  ## MPI for high percentile - plot 6:
  MPIBox <- MPIDF2 %>% filter(Stat == "MPI_Perc") # MPI information
  
  # Points in the dashed zones:
  DashedZoneR <- StatRep$Data %>% filter(MPI_Perc > 2) # Right
  DashedZoneL <- StatRep$Data %>% filter(MPI_Perc < -2) # Left
  
  Plot6 <- ggplot() +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -Inf, ymax = Inf), fill = "green4") +
    geom_point(data = anti_join(anti_join(StatRep$Data, DashedZoneR, by = "Station"), DashedZoneL, by = "Station"),
               aes(x = MPI_Perc, y = 0), color = "blue", size = PointSize) +
    annotate("label", x = 2.6, y = 0, label = round(MPIBox$MPI90, 3), color = MPIBox$CircleColor) + # Colored MPI indicator on the RHS
    scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
    geom_polygon(data = data.frame(x = Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    geom_polygon(data = data.frame(x = -Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, LHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    labs(x = NULL, y = "Hperc Norm") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-1/25 , 1/25), ratio = 1.5, expand = FALSE, clip = "off") +
    ThemeAnalysisSummary
  
  # Set a point in the dashed zones if required:
  if (nrow(DashedZoneR) > 0) Plot6 <- Plot6 + geom_point(aes(x = 2 + (max(Xrange) - 2)/2, y = 0), color = "blue", size = PointSize) # Right
  if (nrow(DashedZoneL) > 0) Plot6 <- Plot6 + geom_point(aes(x = -2 + (min(-Xrange) + 2)/2, y = 0), color = "blue", size = PointSize) # Left
  
  ### Spatial MPI subplots:
  
  # Spatial statistics:
  DataSpace <- StatRep$Data %>% ungroup() %>%
    summarize(N               = n(),
              Mean_mod_space  = mean(Mean_mod),
              Mean_obs_space  = mean(Mean_obs),
              R               = cor(Mean_mod, Mean_obs, method = "pearson"),
              Sigma_mod_space = sqrt(1/N*sum((Mean_mod - Mean_mod_space)^2)), # Standard deviation of modeled concentrations
              Sigma_obs_space = sqrt(1/N*sum((Mean_obs - Mean_obs_space)^2)), # Standard deviation of observed concentrations
              RMS_U           = sqrt(1/N*sum(U_O^2)), # Measurement uncertainty, FAIRMODE page 14
              MPI_R_space     = (1 - R)/(0.5*StatRep$Parameters$beta^2*RMS_U^2/(Sigma_obs_space*Sigma_mod_space)), # MPI for spatial correlations, FAIRMODE Eq. 19
              MPI_Sigma_space = (Sigma_mod_space - Sigma_obs_space)/(StatRep$Parameters$beta*RMS_U)) # MPI for standard deviation, FAIRMODE Eq. 20, without absolute value
  
  # Convert the data.frame with the spatial MPIs to a long format:
  MPIDF  <- DataSpace %>%
    pivot_longer(cols = c("MPI_R_space", "MPI_Sigma_space"), names_to = "Stat", values_to = "Value") %>%
    mutate(Value = abs(Value)) # Convert to absolute value to determine MPI90
  
  MPIDF2 <- MPIDF %>% group_by(Stat) %>%
    summarize(MPI         = Value,
              CircleColor = ifelse(MPI < 1, "green4", "red"))
  
  ## MPI for normalized correlation - plot 7:
  MPIBox <- MPIDF2 %>% filter(Stat == "MPI_R_space") # MPI information
  
  # Check if the single point should be located in the dashed zone:
  DashedZoneR <- DataSpace %>% filter(MPI_R_space > 2) # Right
  
  Plot7 <- ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = -Inf, ymax = Inf), fill = "green4") +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf), fill = "orange") +
    # Visualize the single data point if it is not in the dashed zone:
    geom_point(data = anti_join(DataSpace, DashedZoneR, by = "N"),
               aes(x = MPI_R_space, y = 0), color = "blue", size = PointSize) +
    annotate("text", x = 2.3, y = 0.075, label = "MPI") +
    annotate("label", x = 2.3, y = 0, label = round(DataSpace$MPI_R_space, 3), color = MPIBox$CircleColor) + # Colored MPI indicator on the RHS
    scale_x_continuous(breaks = seq(0, 2, 0.25)) +
    geom_polygon(data = data.frame(x = Xrange2, y = Yrange2), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    ggtitle("Performance in space") +
    labs(x = NULL, y = "1-R Norm") +
    coord_fixed(xlim = c(0, 2), ylim = c(-1/50 , 1/50), ratio = 1.5, expand = FALSE, clip = "off") +
    ThemeAnalysisSummary
  
  # Set a point in the dashed zone if required:
  if (nrow(DashedZoneR) > 0) Plot7 <- Plot7 + geom_point(aes(x = 2 + (max(Xrange2) - 2)/2, y = 0), color = "blue", size = PointSize) # Right
  
  ## MPI for normalized standard deviation - plot 8:
  MPIBox <- MPIDF2 %>% filter(Stat == "MPI_Sigma_space") # MPI information
  
  # Points in the dashed zones:
  DashedZoneR <- DataSpace %>% filter(MPI_Sigma_space > 2) # Right
  DashedZoneL <- DataSpace %>% filter(MPI_Sigma_space < -2) # Left
  
  Plot8 <- ggplot() +
    geom_rect(aes(xmin = -1/sqrt(2), xmax = 1/sqrt(2), ymin = -Inf, ymax = Inf), fill = "green4") +
    geom_rect(aes(xmin = -1, xmax = -1/sqrt(2), ymin = -Inf, ymax = Inf), fill = "orange") +
    geom_rect(aes(xmin = 1/sqrt(2), xmax = 1, ymin = -Inf, ymax = Inf), fill = "orange") +
    # Visualize the single data point if it is not in the dashed zone:
    geom_point(data = anti_join(anti_join(DataSpace, DashedZoneR, by = "N"), DashedZoneL, by = "N"),
               aes(x = MPI_Sigma_space, y = 0), color = "blue", size = PointSize) +
    annotate("label", x = 2.6, y = 0, label = round(DataSpace$MPI_Sigma_space, 3), color = MPIBox$CircleColor) + # Colored MPI indicator on the RHS
    scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
    geom_polygon(data = data.frame(x = Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, RHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    geom_polygon(data = data.frame(x = -Xrange, y = Yrange), aes(x = x, y = y), # Dashed zone, LHS of plot
                 fill = "white", color = "black",  linetype = "dashed", alpha = 0.4) +
    labs(x = NULL, y = "StDev Norm") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-1/25 , 1/25), ratio = 1.5, expand = FALSE, clip = "off") +
    ThemeAnalysisSummary
  
  # Set a point in the dashed zones if required:
  if (nrow(DashedZoneR) > 0) Plot8 <- Plot8 + geom_point(aes(x = 2 + (max(Xrange) - 2)/2, y = 0), color = "blue", size = PointSize) # Right
  if (nrow(DashedZoneL) > 0) Plot8 <- Plot8 + geom_point(aes(x = -2 + (min(-Xrange) + 2)/2, y = 0), color = "blue", size = PointSize) # Left
  
  # Choose the time resolution string: 
  if (Pol == "NO2"){
    ResString <- "hourly mean [\u03BCgm\u207B\u00B3]" # Header string for target plot
  } else if (Pol == "O3"){
    ResString <- "8h moving average daily maximum [\u03BCgm\u207B\u00B3]" # Change target plot header string
  } else if (Pol %in% c("PM2.5", "PM10")){
    ResString <- "daily mean [\u03BCgm\u207B\u00B3]" # Change target plot header string
  }
  
  # Modified plot title for better justification:
  PlotTitle <- paste0("               ", Version, " analysis - ", StatRep$MQI$NPoints,
                      " stations\n               Surface ", StatRep$Parameters$Pol, " ", ResString, " \n               ",
                      round_date(StatRep$MQI$StartDate, unit = "day"), " to ", round_date(StatRep$MQI$EndDate, unit = "day"))
  
  # Combine all plots with patchwork:
  CombinedPlot <- Plot1 + Plot2 + Plot3 + Plot4 + Plot5 + Plot6 + Plot7 + Plot8 +
    plot_layout(nrow = 8) +
    plot_annotation(title = PlotTitle, theme = theme(plot.title = element_text(hjust = 0.5))) # Add a common plot title
  
  print(CombinedPlot)
  
  # Save the combined plot (using cowplot):
  if (SavePlot){
    # Name of output file:
    if (OutputFile == FALSE){
      FileName <- paste0(OutputDir, "SummaryReport_", Version, "_", Pol, ".png")
    } else {
      FileName <- paste0(OutputDir, OutputFile)
    }
    print(paste0("Writing the file: ", FileName))
    save_plot(CombinedPlot, filename = FileName, nrow = 1.75, base_asp = 2)
  }
  
} # End "SummaryReport()"
