
library(Synth)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(zoo)

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("\014")  #Clearer console

mytime <- Sys.time()

# Create log-file ---------------------
# Specify a file for logging
log_file <- "my_log.txt"

# Write  initial time stamp to log
cat(
  "Log started at",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),  # Format as YYYY-mm-dd HH:MM:SS
  "\n\n",
  file = log_file
)

# Options -----------------------------------------------------------------
 
#Set this to TRUE to skip the code and only update plots
onlyPlots <- FALSE
 
#Set this to FALSE to skip placebos (very time consuming)
runPlacebo <- TRUE
 
# Exclude the share of 65+ aged (correlated with mortality rates)
excl65share <- FALSE
 
# Run analysis using only Northern and Western European countries
onlyEurope <- FALSE
 
# Prepare data -------------------------------------------------------------

## Define variables etc. which will create new folders to store results -----

#Set periods to analyze
prewindows <- c(201627)

# Choose the end of the analyzed period
postwindow <- 202126

#set list of predictors to analyze
mypredictors <- c("none", "ps11-14", "ps11-15", "ps11-16")


#set list of dependents to analyze
dependents <- c("RTotal_weekly100K") #We only look at the total mortality

#Set list of SWE-related country codes to analyze
swe_related_codes <- c("SWE", "SWE_w7", "SWE_w8", "SWE_w9", "SWE_w10")
swe_codes <- c("SWE", "SWE_w7", "SWE_w8", "SWE_w9", "SWE_w10", "SWE_w78", "SWE_w910", "SWE_STMF")

# Store variables for use in other codes
save(prewindows, postwindow, mypredictors, dependents, swe_related_codes, file = "SCM_variables.RData")



## Load and merge data --------------------------------------------
load("Mortality_data.Rdata")
Mortality_data$R0_64_weekly100K <- NULL
Mortality_data$R65p_weekly100K <- NULL

load("WB_data.Rdata")
WB_cols <- setdiff(names(WB_data), c("CountryCode", "Year"))

# Function to fill and interpolate missing values within each country
impute_missing_data <- function(data, column_name) {
  if (all(is.na(data[[column_name]]))) {
    # If all values are missing, return the data as is
    return(data)
  } else {
    data %>%
      arrange(Year) %>%
      mutate(!!sym(column_name) := na.locf(!!sym(column_name), na.rm = FALSE)) %>% # Fill forward
      mutate(!!sym(column_name) := na.locf(!!sym(column_name), fromLast = TRUE)) %>% # Fill backward
      mutate(!!sym(column_name) := na.approx(!!sym(column_name), na.rm = FALSE)) # Interpolate
  }
}

WB_col <- WB_cols[1]

# Apply the function to each country/variable
for(wb in 1:length(WB_cols)){
  WB_col <- WB_cols[wb]
  WB_data <- WB_data %>%
    group_by(CountryCode) %>%
    group_modify(~ impute_missing_data(.x, WB_col)) %>%
    ungroup()
}

#Change Swedish country codes to SWE before merging data from WB on
df <- Mortality_data %>%
  mutate(
    CountryCode_org = CountryCode,  # Preserve original CountryCode
    CountryCode = substr(CountryCode,1,3)
  )

df <- left_join(df, WB_data, by=c("CountryCode", "Year"))

df <- df %>%
  mutate(CountryCode = CountryCode_org) %>%
  select(-CountryCode_org)

# Numeric CountryID
df <- df %>%
  mutate(CountryID = dense_rank(CountryCode))

df <- df %>%
  mutate(
    CountryID = as.numeric(factor(CountryCode)),
    YearWeek = Year * 100 + Week
      )


## Summarize data  ---------------------------------

### Summary of data availability --------------

filtered_data <- df %>%
  filter(YearWeek >= prewindows & YearWeek <= postwindow & substr(CountryCode, 1, 4) != "SWE_")



# Identify countries that have data for both 200727 and 202126
valid_countries <- filtered_data %>%
  filter(YearWeek %in% postwindow) %>%
  group_by(CountryCode) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  pull(CountryCode)

# Filter out countries that do not meet the criterion
filtered_data <- filtered_data %>%
  filter(CountryCode %in% valid_countries)


# Function to check if a country has complete data for a given year
has_complete_data <- function(data) {
  complete_data <- data %>%
    group_by(Year) %>%
    summarize(complete = all(!is.na(RTotal_weekly100K) &
                               !is.na(Hospital_beds) & !is.na(Urban_population) & !is.na(GDPCAP_PPP) &
                               !is.na(ShareAged65p) & !is.na(Migrant_share)))
  complete_data
}

# Check data availability for each country
country_data_availability <- filtered_data %>%
  group_by(CountryCode) %>%
  do(has_complete_data(.))

# Summarize how many countries have complete data for each year
summary_data_availability <- country_data_availability %>%
  group_by(Year) %>%
  summarize(complete_countries = sum(complete))

# Print and save the summary table
print(summary_data_availability)
write.xlsx(summary_data_availability, file = paste0("Data availability.xlsx"))




## Define plot functions -----------------------------------------------------------

create_custom_plot <- function(df, lsw, min_ylim = NULL, max_ylim = NULL, yplace = NULL, ylabel = NULL) {
  # Find the position of 'lsw' within the sorted unique weeks
  all_weeks <- sort(unique(df$Sequence))
  lw_position <- match(lsw, all_weeks)
  
  # Generate sequences backwards and forwards from 'lsw', including 'lsw' itself
  myLabelCount <- ceiling(length(all_weeks) / 12)
  backward_indices <- seq(lw_position, 1, by = -myLabelCount)
  forward_indices <- seq(lw_position, length(all_weeks), by = myLabelCount)
  
  # Combine the sequences and remove duplicates
  combined_indices <- unique(c(backward_indices, forward_indices))
  combined_indices <- sort(combined_indices)  # Ensure it's sorted
  
  # Select the custom breaks and labels using the combined indices
  custom_breaks <- all_weeks[combined_indices]
  custom_labels <- unique(df$Year_w_Week[match(custom_breaks, df$Sequence)])
  
  # Set Y-scale
  if (is.null(max_ylim)) {
    max_outcome <- max(df$Outcome)
    max_ylim <- ceiling(max_outcome)
  }
  
  if (is.null(min_ylim)) {
    min_outcome <- min(df$Outcome)
    min_ylim <- floor(min_outcome)
  }
  
  if (is.null(yplace)) {
    yplace <- 0
  }
  
  if (is.null(ylabel)) {
    ylabel <- "Mortality/100,000"
  }
  
  # Plotting
  MyPlot <- 
    ggplot(df, aes(x = Sequence, y = Outcome, linetype = Type)) +
    geom_line() +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.background = element_rect(fill = "white"),  # Set panel background to white
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels
    scale_x_continuous(name = NULL, breaks = custom_breaks, labels = custom_labels) +
    scale_color_manual(values = rep("black", length(unique(df$Type)))) +  # Ensure one color per Type
    scale_linetype_manual(values = c("solid", "dotted")) +
    labs(title = NULL,
         y = ylabel,
         linetype  = NULL) +  # Removes default legend title
    geom_vline(xintercept = lsw, linetype = "dashed", color = "black", size = 1, show.legend = FALSE) +
    annotate("text", x = lsw - myLabelCount / 4, y = yplace, label = "Lockdown week", size = 4, color = "black", angle = 90, hjust = 0) +
    ylim(min_ylim, max_ylim) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8)  # Add horizontal line at y = 0
  
  return(MyPlot)
}

create_placebo_plot <- function(df, lsw, main, min_ylim = NULL, max_ylim = NULL, yplace = NULL) {
  # Find the position of 'lsw' within the sorted unique weeks
  all_weeks <- sort(unique(df$Sequence))
  lw_position <- match(lsw, all_weeks)
  
  # Generate sequences backwards and forwards from 'lsw', including 'lsw' itself
  myLabelCount <- ceiling(length(all_weeks) / 12)
  backward_indices <- seq(lw_position, 1, by = -myLabelCount)
  forward_indices <- seq(lw_position, length(all_weeks), by = myLabelCount)
  
  # Combine the sequences and remove duplicates
  combined_indices <- unique(c(backward_indices, forward_indices))
  combined_indices <- sort(combined_indices)  # Ensure it's sorted
  
  # Select the custom breaks and labels using the combined indices
  custom_breaks <- all_weeks[combined_indices]
  custom_labels <- unique(df$Year_w_Week[match(custom_breaks, df$Sequence)])
  
  # Set Y-scale
  if (is.null(max_ylim)) {
    max_outcome <- max(df$Outcome)
    max_ylim <- ceiling(max_outcome)
  }
  
  if (is.null(min_ylim)) {
    min_outcome <- min(df$Outcome)
    min_ylim <- floor(min_outcome)
  }
  
  if (is.null(yplace)) {
    yplace <- 0
  }
  
  # Add a group column to differentiate the first line from the rest
  df$LineGroup <- ifelse(grepl("^SWE", df$Type), main, "Placebos")
  
  # Define line sizes
  line_sizes <- c(setNames(0.6, main), "Placebos" = 0.3)
  
  # Plotting
  MyPlot <- 
    ggplot(df, aes(x = Sequence, y = Outcome, color = LineGroup, group = Type)) +
    geom_line(aes(linetype = LineGroup, size = LineGroup)) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.background = element_rect(fill = "white"),  # Set panel background to white
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels
    scale_x_continuous(name = NULL, breaks = custom_breaks, labels = custom_labels) +
    scale_color_manual(values = c(setNames("black", main), "Placebos" = "grey")) +  # Set colors
    scale_linetype_manual(values = c(setNames("solid", main), "Placebos" = "solid")) +  # Ensure all lines are solid
    scale_size_manual(values = line_sizes) +  # Set line sizes
    labs(title = NULL,
         y = "Cumulative Excess Mortality/100,000",
         linetype = NULL,
         color = NULL,
         size = NULL) +  # Removes default legend title
    geom_vline(xintercept = lsw, linetype = "dashed", color = "black", size = 1, show.legend = FALSE) +
    annotate("text", x = lsw - myLabelCount / 4, y = yplace, label = "Lockdown week", size = 4, color = "black", angle = 90, hjust = 0) +
    ylim(min_ylim, max_ylim) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8)  # Add horizontal line at y = 0
  
  MyPlot
  return(MyPlot)
}



# ..............................................................----
# Define scenarios and prepare data ---------------------------------

# Set lockdown week
lw <- 202012 # Changed sinse first version

iteration_info <- list(d = NA, p = NA, pw = NA, i = NA)

# LOOPS -----------------------
##Loop through all SCM scenarios................ --------------------------------------------------------------------


#Choose outcome variable - age group - (and predictor) for SCM 
dependent <- dependents
dependent_folder_name <- sub("_weekly100K", "", dependent)

#Create list of WB_data excl. "ShareAged65p" because of possible singulatiry issues.
WB_predictors <- WB_cols
s65_text <- ""
if(excl65share){
  WB_predictors <- WB_cols[WB_cols != "ShareAged65p"] # Changed since first version
  s65_text <- "-65s"
}


predictor_list <- list()
predictor_list[["none"]] <- NA
predictor_list[["ps11-14"]] <- c(202011, 202012, 202013, 202014)
predictor_list[["ps11-15"]] <- c(202011, 202012, 202013, 202014, 202015)
predictor_list[["ps11-16"]] <- c(202011, 202012, 202013, 202014, 202015, 202016)

# p-loop ... (full, single etc.) ----  
for (p in 1:length(mypredictors)){
  mypredictor <- mypredictors[p]
  
  predictors <- c(WB_predictors)
  pre_spread <- predictor_list[mypredictor]
  
  prewindow <- prewindows
  
  
#### Create folders ----
  #Choose when pre-intervention window starts
  postyear <- floor(postwindow/100)

    # Define the folder names

  agegroup_folder <- paste0(postyear,"-", dependent_folder_name)
  prewindow_folder <- paste0(agegroup_folder, "/", prewindow)
  predictor_folder <- paste0(prewindow_folder, "/", mypredictor)
  results_folder <- paste0(predictor_folder, s65_text)
  
  # Check if the folder exists; if not, create it
  if (!dir.exists(agegroup_folder)) {
    dir.create(agegroup_folder)
  }
  
  if (!dir.exists(results_folder)) {
    dir.create(prewindow_folder)
  }
  
  if (!dir.exists(predictor_folder)) {
    dir.create(predictor_folder)
  }
  
  if (!dir.exists(results_folder)) {
    dir.create(results_folder)
  }

  

#### Filter data ----
  # Filter the dataset to keep relevant countries with a full mortality dataset
  filtered_df <- df %>%
    # Filter based on the prewindow and postwindow
    filter(YearWeek >= prewindow & YearWeek <= postwindow) %>%
    # Group by CountryCode to calculate min and max YearWeek per country
    group_by(CountryCode) %>%
    # Filter out countries that do not cover the full YearWeek range
    filter(min(YearWeek) == prewindow & max(YearWeek) == postwindow) %>%
    # Ungroup to return to the original dataframe structure
    ungroup()
  
  # Filter out UK which had a strategy similar to Sweden's in the first weeks of the pandemic
  filtered_df <- filtered_df %>%  # Changed sinse first version
    filter(!CountryCode %in% c("GBRTENW", "GBR_SCO", "GBR_NIR"))
  print("UK was dropped from the donor pool because of policy similar to SWE.")
  print(paste0(length(unique(filtered_df$CountryCode)), " donor countries remain"))
  
  # Filter out ITA who implemented lockdowns very early
  filtered_df <- filtered_df %>%  # Changed since first version
    filter(!CountryCode %in% c("ITA"))
  print("Italy was dropped from the donor pool because of early lockdown.")
  print(paste0(length(unique(filtered_df$CountryCode)), " donor countries remain"))
  
  
  # Filter out countries from the Southern Hemisphere
  filtered_df <- filtered_df %>%  # Changed sinse first version
    filter(!CountryCode %in% c("AUS", "CHL", "NZL_NP"))
  print("Australia, Chile, and New Zealand were dropped from the donor pool because they are located on the Southern Hemisphere.")
  print(paste0(length(unique(filtered_df$CountryCode)), " donor countries remain"))
  

  
  # Filter to Western and Northern European countries (UN definition)
  if(onlyEurope){
    cc_before <- unique(filtered_df$CountryCode)
    filtered_df <- filtered_df %>%
      filter(!CountryCode %in% c("CAN", "ISR", "KOR", "TWN", "USA"))
    cc_after <- unique(filtered_df$CountryCode)
    cc_removed <- paste(setdiff(cc_before, cc_after), collapse = ", ")
    print(paste0("Non-European countries (", cc_removed, ") was dropped from the donor pool"))
  }
  print(paste0(length(unique(filtered_df$CountryCode)), " donor countries remain"))
    

  # Filter the dataset to keep relevant countries with a full WB dataset pre-intervention
  # Identify countries with NAs before the specified week number
  countries_with_na <- filtered_df %>%
    filter(YearWeek < lw & substr(CountryCode, 1, 3) != "SWE") %>%
    group_by(CountryCode) %>%
    summarise(has_na = any(across(everything(), ~ any(is.na(.))))) %>%
    filter(has_na) %>%
    pull(CountryCode)
  
  # Filter out those countries from the original data frame which do not have a full data set
  filtered_df <- filtered_df %>%
    filter(!CountryCode %in% countries_with_na)
  print(paste0(countries_with_na, " was dropped from the donor pool because of missing WB data."))
  print(paste0(length(unique(filtered_df$CountryCode)), " donor countries remain"))
  
  # Create Sequence
  filtered_df <- filtered_df %>%
    group_by(CountryCode) %>%
    arrange(CountryCode, YearWeek) %>% # Ensure the data is sorted by YearWeek within each country
    mutate(Sequence = row_number()) %>%
    ungroup()
  
  # Create table for use later
  week_to_yearweek <- unique(filtered_df[c("Sequence", "YearWeek")])
  week_to_yearweek <- week_to_yearweek %>%
    mutate(Year_Week = paste(substr(YearWeek, 1, 4), substr(YearWeek, 5, 6), sep = "-"),
           Year_w_Week = paste(substr(YearWeek, 1, 4), substr(YearWeek, 5, 6), sep = "w"))


#### Calculate variables as proxy for pre-lockdown spread ---------
  # Step 1: Calculate the baseline mortality (average RTotal_weekly100K for 202008 to 202010)
  baseline_weeks <- filtered_df[filtered_df$YearWeek %in% c("202008", "202009", "202010"), ]
  baseline_mortality <- aggregate(RTotal_weekly100K ~ CountryCode, data = baseline_weeks, FUN = mean)
  colnames(baseline_mortality)[2] <- "Baseline_mortality"  # Rename the column for clarity
  
  # Step 2: Calculate excess deaths for weeks in 2020
  filtered_df <- filtered_df %>%
    left_join(baseline_mortality, by = "CountryCode") %>%
    mutate(ExcessDeaths = RTotal_weekly100K - Baseline_mortality)

#### Set variables for the SCM --------------------------------------

  #Lockdown sequence week
  lsw <- filtered_df %>% filter(YearWeek == lw) %>% pull(Sequence) %>% max()  # Using 'pull' to directly extract the column values
  
  
  # Special predictors as mean mortality for each season up to 2019/20.
  # For the 2019/20 season we stop at week 202008
  # Define year ranges for special predictors
  start_year <- min(filtered_df$Year)
  end_year <- 2020  # Ending year
  
  # Create an empty list to store the ranges
  yearweek_ranges <- list()
  
  # Loop through each year until end_year
  for (year in start_year:(end_year - 2)) {
    # Define the start and end YearWeeks for the current range
    x <- paste0(year, "27")
    y <- paste0(year + 1, "26")
    
    # Find the corresponding Sequence values for x and y
    x_seq <- filtered_df %>% filter(YearWeek == x) %>% pull(Sequence) %>% max()
    y_seq <- filtered_df %>% filter(YearWeek == y) %>% pull(Sequence) %>% max()
    
    # Add the range to the list
    yearweek_ranges[[paste0(x, ":", y)]] <- x_seq:y_seq
  }
  
  # Handle the last season separately
  final_x <- paste0(end_year-1, "27")
  final_y <- 202008  # Limit for the last season
  
  # Find the corresponding Sequence values for the last year
  final_x_seq <- filtered_df %>% filter(YearWeek == final_x) %>% pull(Sequence) %>% max()
  final_y_seq <- filtered_df %>% filter(YearWeek <= final_y) %>% pull(Sequence) %>% max()
  
  # Add the range for the last year
  yearweek_ranges[[paste0(final_x, ":", final_y)]] <- final_x_seq:final_y_seq
  
  # Create the special.predictors list dynamically
  yearly_mortality_predictors <- lapply(yearweek_ranges, function(range) {
    list("RTotal_weekly100K", range, "mean")
  })
  
  
  # Convert each year-week in pre_spread into the corresponding sequence number
  pre_spread_seq <- if (exists("pre_spread") && length(pre_spread) > 0 && !is.na(pre_spread)) {
    sapply(pre_spread[[1]], function(yw) {
    filtered_df %>%
      filter(YearWeek == yw) %>%
      pull(Sequence) %>%
      max()
    })
  }
  
  
  
  # If pre_spread does not exist or is empty, set excess_deaths_predictors to an empty list
  excess_deaths_predictors <- if (exists("pre_spread") && length(pre_spread) > 0 && !is.na(pre_spread)) {
    lapply(pre_spread_seq, function(x) {
      list("ExcessDeaths", x, "mean")
    })
  } else {
    list()
  }
  
  special_predictors <- c(yearly_mortality_predictors, excess_deaths_predictors)
  
  # Check the result
  special_predictors
  
  
if(!onlyPlots){   
  #number of weeks
  FinalWeek <- max(filtered_df$Sequence)
  
#### Define donor pool and data -----------
  Donordata <- filtered_df %>%
    # Filter out unnecessary Sweden-versions first
    filter(!(CountryCode %in% swe_codes)) %>%
    # Filter based on the prewindow and postwindow
    filter(YearWeek >= prewindow & YearWeek <= postwindow) %>%
    # Group by CountryCode to calculate min and max YearWeek per country
    group_by(CountryCode) %>%
    # Filter out countries that do not cover the full YearWeek range
    filter(min(YearWeek) == prewindow & max(YearWeek) == postwindow) %>%
    # Ungroup to return to the original dataframe structure
    ungroup()
  
  Donordata <- as.data.frame(Donordata)
  

  controlsidentifier <- Donordata %>%
    pull(CountryID) %>%
    unique()
  
  # Setup for the placebo test
  placebo_plot_data_cum_list <- list()
  placebo_plot_data_list <- list()

#### Create placebo test data by looping through all controls ------------------------
if(runPlacebo){
  # Loop over each control unit to perform placebo tests
  for (j in 1:length(controlsidentifier)) {
    treated <- controlsidentifier[j]
    treated_cc <- Donordata %>% 
      filter(CountryID == treated) %>%
      pull(CountryCode) %>%
      unique()

    # Prepare data for the current control unit as if it were the treated unit
    dataprep_placebo <- dataprep(
      foo = Donordata,
      predictors = predictors,
      predictors.op = "mean",
      dependent = dependent,
      unit.variable = "CountryID",
      time.variable = "Sequence",
      unit.names.variable = "CountryCode",
      treatment.identifier = treated,
      controls.identifier = setdiff(controlsidentifier, treated),
      time.predictors.prior = c(1:lsw),
      time.optimize.ssr = c(1:lsw),
      time.plot = c(1:FinalWeek),
      special.predictors = special_predictors
    )
    
    # Run synth for the placebo
    synth_placebo <- synth(data.prep.obj = dataprep_placebo, optimxmethod = "All")
    
    # Calculate the synthetic control for the placebo
    synth_placebo_control <- dataprep_placebo$Y0plot %*% synth_placebo$solution.w
    
#### Create placebo cumulative graphs data  --------------------------------------------------------------
    
    # Graphs data
    plot_data <- data.frame(
      Sequence = 1:FinalWeek,
      Actual = Donordata[[dependent]][Donordata$CountryID == treated],  # Use dynamic column name
      Synthetic = as.numeric(synth_placebo_control)  # Data for synthetic control
    )
    
    # Create data for cumulative effect
    plot_data_cum <- plot_data %>% 
      mutate(
        `Weekly Effect` = ifelse(Sequence <= lsw, 0, Actual - Synthetic),
        `Cumulative Effect` = cumsum(`Weekly Effect`)
      ) %>%
      select(Sequence, `Cumulative Effect`) %>%
      rename_with(~ treated_cc, `Cumulative Effect`)
    
    # Melting the data frame for ggplot2
    plot_data_long_cum <- reshape2::melt(plot_data_cum, id.vars = "Sequence", variable.name = "Type", value.name = "Outcome")
    
    # Store results
    placebo_plot_data_cum_list[[treated_cc]] <- plot_data_long_cum
    placebo_plot_data_list[[treated_cc]] <- plot_data
  }
#### Save placebo_data ----
  save(placebo_plot_data_cum_list, file = paste0(results_folder, "/placebo_plot_data_cum_list.RData"))
  save(placebo_plot_data_list, file = paste0(results_folder, "/placebo_plot_data_list.RData"))
} #end if(runPlacebo)
} #end if(onlyPlots)

#### Create containers for data ------------
  dfResults <- data.frame()
  dfWeights <- data.frame()
  comparison_table_list <- list()
  plot_data_long_list <- list()
  plot_data_list <- list()
  plot_data_cum_list <- list()

  if(onlyPlots){
    # Delete all plots in result_folder
    unlink(paste0(results_folder,"/*.png"))
  }


# Logging
  cat("Starting i-loop.  Time spent", difftime(Sys.time(), mytime, units = "secs"), "\n\n", file = log_file, append = TRUE)
  

## i-loop ... (SWE, SWE_w7 etc.) ----------
for (i in 1:length(swe_related_codes)) {

  
  
  # Update the global iteration info
  iteration_info$p <- p
  iteration_info$i <- i

  # Select the SWE-related country code you want to analyze
  MySce <- swe_related_codes[i]
  
  # Print a custom message showing the stored iteration information
  cat("Current iteration details stored: \n",
      "results folder: ", results_folder, "\n",
      "d = ", iteration_info$d, "-", dependent, "\n",
      "p =", iteration_info$p, "-", mypredictor, "\n",
      "pw =", iteration_info$pw, "-", prewindow, "\n",
      "i =", iteration_info$i, "-", MySce, "\n")
  
if(!onlyPlots){
## Create SCMdata ---------------------------------------------------------
  
  ## Filter the dataset to keep the relevant Sweden
  SWEdata <- filtered_df %>% filter(CountryCode == MySce)
  
  treatmentidentifier <- SWEdata %>%
    pull(CountryID) %>%
    unique()
  
  SCMdata <- bind_rows(Donordata, SWEdata)
  
  SCMdata <- as.data.frame(SCMdata)
  
  #### QA of SCMdata ---------------------------------
  # Check that the data are balanced
  summary <- SCMdata %>%
    group_by(CountryCode) %>%
    summarize(
      Num_Obs = n(),
      .groups = "drop"
    )
  if(min(summary$Num_Obs) != max(summary$Num_Obs)){stop("The data are unbalanced")}
  
  
## Run SCM ---------------------------------------------------------------------
  
  dataprep <-
    dataprep(
      foo = SCMdata,
      predictors = predictors,
      predictors.op = "mean",
      dependent     = dependent,
      unit.variable = "CountryID",
      time.variable = "Sequence",
      unit.names.variable   = "CountryCode",
      treatment.identifier  = treatmentidentifier,
      controls.identifier   = controlsidentifier,
      time.predictors.prior = c(1:lsw),
      time.optimize.ssr     = c(1:lsw),
      time.plot             = c(1:FinalWeek),
      special.predictors    = special_predictors
    )
  
  # run synth
  synth.out <- synth(data.prep.obj = dataprep, optimxmethod="All")
  
  synth.SWE<-(dataprep$Y0plot%*%synth.out$solution.w)
  

#### Compare Actual with Synthetic in the pre-intervention window --------------------------------------------------------------

  # Extract the predictor values for Sweden
  sweden_predictors <- dataprep$X1
  sweden_proxies <- SCMdata %>%
    filter(YearWeek >= 202011 & YearWeek <= 202016 & CountryID %in% treatmentidentifier) %>%
    select(Sequence, ExcessDeaths)

  # Extract the weighted average predictor values for synthetic Sweden
  synthetic_sweden_predictors <- dataprep$X0 %*% synth.out$solution.w
  
  control_proxy_data  <- SCMdata %>%
    filter(YearWeek >= 202011 & YearWeek <= 202016 & CountryID %in% controlsidentifier) %>%
    select(CountryID, Sequence, ExcessDeaths)
  
  synthetic_sweden_proxies <- control_proxy_data %>%
    group_by(Sequence) %>%
    summarize(SyntheticValue = sum(ExcessDeaths * synth.out$solution.w))
  
  # Calculate min and max for each predictor across control units
  min_controls <- apply(dataprep$X0, 1, min)
  max_controls <- apply(dataprep$X0, 1, max)

  # Calculate the mean and standard deviation for each predictor across control units
  mean_controls <- apply(dataprep$X0, 1, mean)
  sd_controls <- apply(dataprep$X0, 1, sd)

  # Calculate the 95% Confidence Interval (CI) for each predictor
  n_controls <- ncol(dataprep$X0)  # Number of control units
  lower_CI <- mean_controls - 1.96 * (sd_controls / sqrt(n_controls))
  upper_CI <- mean_controls + 1.96 * (sd_controls / sqrt(n_controls))

  # Calculate the difference and relative difference between Sweden and Synthetic Sweden
  difference <- sweden_predictors - synthetic_sweden_predictors
  relative_difference <- difference/sweden_predictors

  # Calculate the difference as a share of the SD of control units
  share_of_sd_difference <- difference / sd_controls

  # Create a table to compare Sweden, Synthetic Sweden, and control statistics
  comparison_table <- data.frame(
    Predictor = rownames(dataprep$X1),                         # Names of the predictors
    Sweden = as.vector(sweden_predictors),                     # Values for Sweden
    Synthetic_Sweden = as.vector(synthetic_sweden_predictors), # Values for Synthetic Sweden
    Min_Controls = min_controls,                               # Min value for control units
    Max_Controls = max_controls,                               # Max value for control units
    Mean_Controls = mean_controls,                             # Mean value for control units
    SD_Controls = sd_controls,                                 # Standard deviation of control units
    Lower_CI = lower_CI,                                       # Lower bound of the 95% CI
    Upper_CI = upper_CI,                                       # Upper bound of the 95% CI
    Difference = as.vector(difference),                        # Difference between Sweden and Synthetic Sweden
    Relative_Difference = as.vector(relative_difference),      # Relative difference between Sweden and Synthetic Sweden
    Share_of_SD_Difference = as.vector(share_of_sd_difference) # Difference as a share of control units' SD
  )

  # Combine Sweden and synthetic Sweden proxies
  proxy_comparison <- sweden_proxies %>%
    left_join(synthetic_sweden_proxies, by = "Sequence") %>%
    rename(
      Sweden_Proxies = ExcessDeaths,
      Synthetic_Proxies = SyntheticValue
    )
  
  proxy_rows <- data.frame(
    Predictor = paste0("Proxy_Sequence_", proxy_comparison$Sequence),
    Sweden = proxy_comparison$Sweden_Proxies,
    Synthetic_Sweden = proxy_comparison$Synthetic_Proxies,
    Min_Controls = NA,
    Max_Controls = NA,
    Mean_Controls = NA,
    SD_Controls = NA,
    Lower_CI = NA,
    Upper_CI = NA,
    Difference = NA,
    Relative_Difference = NA,
    Share_of_SD_Difference = NA
  )
  
  comparison_table_w_proxies <- rbind(comparison_table, proxy_rows)
  
  # Store comparison table
  comparison_table_list[[MySce]] <- comparison_table_w_proxies

#### Create plot data --------------------------------------------------------------
  ##### Weekly effect -------------------
  
  # Graphs data
  plot_data_org <- data.frame(
    Sequence = 1:FinalWeek,
    Actual = SCMdata[[dependent]][SCMdata$CountryID == treatmentidentifier],  # Use dynamic column name
    Synthetic = as.numeric(synth.SWE)  # Data for synthetic control
  )
  
  plot_data <- plot_data_org
  
  # Store plot_data
  plot_data_list[[MySce]] <- plot_data
  
  # Melting the data frame for ggplot2
  plot_data_long <- reshape2::melt(plot_data, id.vars = "Sequence", variable.name = "Type", value.name = "Outcome")
  
  # Add more options to the x-axis
  plot_data_long <- merge(plot_data_long, week_to_yearweek, by = "Sequence", all.x = TRUE)

  # Store plot_data_long
  plot_data_long_list[[MySce]] <- plot_data_long
  
  ##### Cumulative effect -------------------------
  
  # Create data for cumulative effect
  plot_data_cum <- plot_data %>%
    # Calculate the Weekly difference
    mutate(Weekly = Actual - Synthetic) %>%
    # Calculate the non-standardized cumulative sum
    mutate(Cumulative_nonstandardized = cumsum(Weekly)) %>%
    # Extract the value of Cumulative_nonstandardized at lsw
    mutate(LSW_Cumulative = ifelse(Sequence == lsw, Cumulative_nonstandardized, NA)) %>%
    # Carry forward and backward the LSW_Cumulative value
    mutate(LSW_Cumulative = zoo::na.locf(zoo::na.locf(LSW_Cumulative, na.rm = FALSE), fromLast = TRUE)) %>%
    # Normalize the Cumulative_nonstandardized by subtracting the value at lsw
    mutate(Cumulative = Cumulative_nonstandardized - LSW_Cumulative) %>%
    # Select relevant columns
    select(Sequence, Cumulative, Weekly)
  
  # Melting the data frame for ggplot2
  plot_data_long_cum <- reshape2::melt(plot_data_cum, id.vars = "Sequence", variable.name = "Type", value.name = "Outcome")
  
  # Add more options to the x-axis
  plot_data_long_cum <- merge(plot_data_long_cum, week_to_yearweek, by = "Sequence", all.x = TRUE)
  
  # Store plot_data_long_cum
  plot_data_cum_list[[MySce]] <- plot_data_long_cum
  
  
  #### Generate output tables ---------------------------------------------------------
  # Create tables with cumulative results for 2020w26 and 2021w26
  # Step 1: Find sequence numbers
  seq202026 <- filtered_df %>% filter(YearWeek == 202026) %>% pull(Sequence) %>% max()  # Using 'pull' to directly extract the column values
  seq202126 <- filtered_df %>% filter(YearWeek == 202126) %>% pull(Sequence) %>% max()  # Using 'pull' to directly extract the column values
  
  # Step 2: Extract mortality rates
    res202026 <- plot_data_cum[seq202026, "Cumulative"]
    res202126 <- plot_data_cum[seq202126, "Cumulative"]
  
    new_row <- data.frame(Dependent = dependent, 
                          Postyear = postyear, 
                          Predictor = mypredictor, 
                          Prewindow = prewindow, 
                          Sweden_Sce = MySce, 
                          Diff_in_202026 = res202026, 
                          Diff_in_202126 = res202126, 
                          stringsAsFactors = FALSE)
    
    dfResults <- rbind(dfResults, new_row)
  
  
  
  # Creates table with weights
  synth.tables <- synth.tab(
    dataprep.res = dataprep,
    synth.res = synth.out
  )
  
  # Rename 'w.weights' to the value stored in MySce, drop 'unit.numbers' and left-join
  if (nrow(dfWeights) == 0) {
    dfWeights <- synth.tables$tab.w %>%
      select(Country := unit.names, !!sym(MySce) := w.weights, -unit.numbers)
  } else {
    # Remove the existing column if it exists
    if (MySce %in% colnames(dfWeights)) {
      dfWeights <- dfWeights %>%
        select(-all_of(MySce))
    }
    
    # Perform the left join to add the new data
    dfWeights <- dfWeights %>%
      left_join(
        synth.tables$tab.w %>%
          select(Country := unit.names, !!sym(MySce) := w.weights),
        by = "Country"
      )
  }

} #end if(!onlyPlots)

  
  
#Loop through all plot creations................ ------------------------------------------

file_info <- paste0("_", dependent_folder_name, "_", start_year, "_", mypredictor)

## Prepare data  -----
  
if(onlyPlots){
  print("Preparing plot data")
  Sys.sleep(1)
  #Load plot_data_long
  load(paste0(results_folder, "/plot_data_long_list" , file_info, ".RData"))
  plot_data_long <- plot_data_long_list[[MySce]]
  
  #Load plot_data_long_cum
  load(paste0(results_folder, "/plot_data_cum_list" , file_info, ".RData"))
  plot_data_long_cum <- plot_data_cum_list[[MySce]] 
  
  #Load placebo_plot_data_cum_list
  if(runPlacebo){
    load(paste0(results_folder, "/placebo_plot_data_cum_list", ".RData"))
  }
}
  
### Fix plot data and set limits ----
  #Fix label for graphs
  mylabel <- sub("SWE_?", "Sweden", MySce)  #Replace SWE and SWE_ with Sweden
  mylabel <- sub("STMF\\b", " (STMF)", mylabel)
  mylabel <- sub("w7\\b", " (w7)", mylabel)
  mylabel <- sub("w8\\b", " (w8)", mylabel)
  mylabel <- sub("w9\\b", " (w9)", mylabel)
  mylabel <- sub("w10\\b", " (w10)", mylabel)
  mylabel <- sub("w78\\b", " (w7+w8)", mylabel)
  mylabel <- sub("w910\\b", " (w9+w10)", mylabel)
  MyNum <- i
  
  # Set file name info
  plot_agegroup <- dependent_folder_name
  plot_predictor <- mypredictor
  plot_prewindow <- substr(prewindow, 1, 4)
  plot_info <- paste(plot_agegroup, plot_prewindow ,plot_predictor, sep="_")
  
  
  plot_data_long <- plot_data_long %>%
    mutate(Type = ifelse(Type == "Synthetic", paste0("Synthetic ", mylabel), paste0("Actual ", mylabel)))
  
  ymin <- floor(min(plot_data_long$Outcome))
  ymax <- ceiling(max(plot_data_long$Outcome))
  
  yp <- plot_data_long %>% 
    filter(Sequence <= lsw & Sequence >= lsw-52) %>%
    select(Outcome) %>%
    max()
  
  yp <- yp+2
  
### Plot with full pre- and post-intervention window ---------
  MyPlot <- create_custom_plot(plot_data_long, lsw, min_ylim = ymin, max_ylim = ymax, yplace = yp)
  MyPlot
  ggsave(paste0(results_folder, "/Plot_", MySce, "_", plot_info, ".png"), plot = MyPlot, width = 5, height = 5, dpi = 300, bg = "white")
  ggsave(paste0(results_folder, "/Wide_", MySce, "_", plot_info, ".png"), plot = MyPlot, width = 10, height = 5, dpi = 300, bg = "white")
  
### Plot focusing on post-intervention window (zoom)------------
  plot_data_long_zoom <- plot_data_long[plot_data_long$YearWeek >= 202001,]
  MyPlotZoom <- create_custom_plot(plot_data_long_zoom, lsw, min_ylim = ymin, max_ylim = ymax, yplace = yp)
  MyPlotZoom
  ggsave(paste0(results_folder, "/Zoom_", MySce, "_", plot_info, ".png"), plot = MyPlotZoom, width = 5, height = 5, dpi = 300, bg = "white")
  
### Cum plot focusing on effect & cumulative effect post-intervention ------------
  yp <- floor(min(plot_data_long_cum$Outcome)) 
  
  MyPlotCum <- create_custom_plot(plot_data_long_cum, lsw, yplace = yp, ylabel = "Excess Mortality/100,000")
  MyPlotCum
  ggsave(paste0(results_folder, "/Cum_", MySce, "_", plot_info, ".png"), plot = MyPlotCum, width = 5, height = 5, dpi = 300, bg = "white")
  
  
### Cum plot focusing on post-intervention window (zoom) ------------
  plot_data_long_cum_zoom <- plot_data_long_cum %>%
    filter(YearWeek >= 202001) %>%
    mutate(Outcome = ifelse(YearWeek <= lw, 0, Outcome))
  
  yp <- floor(min(plot_data_long_cum_zoom$Outcome)) 
  
  MyPlotCumZoom <- create_custom_plot(plot_data_long_cum_zoom, lsw, yplace = yp, ylabel = "Excess Mortality/100,000")
  MyPlotCumZoom
  ggsave(paste0(results_folder, "/CumZoom_", MySce, "_", plot_info, ".png"), plot = MyPlotCumZoom, width = 5, height = 5, dpi = 300, bg = "white")

### Cum plot focusing on post-intervention window from 201927 (zoom) ------------
  plot_data_long_cum_zoom <- plot_data_long_cum %>%
    filter(YearWeek >= 201927)
  # %>%
  #   mutate(Outcome = ifelse(YearWeek <= lw, 0, Outcome))
  # 
  yp <- floor(min(plot_data_long_cum_zoom$Outcome)) 
  
  MyPlotCumZoom <- create_custom_plot(plot_data_long_cum_zoom, lsw, yplace = yp, ylabel = "Excess Mortality/100,000")
  MyPlotCumZoom
  ggsave(paste0(results_folder, "/CumZoom19_", MySce, "_", plot_info, ".png"), plot = MyPlotCumZoom, width = 5, height = 5, dpi = 300, bg = "white")
  
  
### Uniform cum plot focusing on short term post-intervention window (zoom) ------------
  plot_data_long_cum_zoom_short <- plot_data_long_cum %>%
    filter(YearWeek >= 202001 & YearWeek <= 202044) %>%
    mutate(Outcome = ifelse(YearWeek <= lw, 0, Outcome))
  
  yp <- floor(min(plot_data_long_cum_zoom_short$Outcome)) 
  
  MyPlotCumZoom <- create_custom_plot(plot_data_long_cum_zoom_short, lsw, min_ylim = -20, max_ylim = 80, yplace = yp, ylabel = "Excess Mortality/100,000")
  MyPlotCumZoom
  ggsave(paste0(results_folder, "/CumZoomShort_", MySce, "_", plot_info, ".png"), plot = MyPlotCumZoom, width = 5, height = 5, dpi = 300, bg = "white")
  
  
  
### Placebo test plot ------------------------------------------------------

if(runPlacebo){
  # Bind the filtered results
  plot_data_long_placebo <- bind_rows(placebo_plot_data_cum_list)

  # Add WeekNum etc. to the
  plot_data_long_placebo <- merge(plot_data_long_placebo, week_to_yearweek, by = "Sequence", all.x = TRUE)

  #Add the effect from our main scenario
  plot_data_long_main <- plot_data_long_cum %>%
    filter(Type == "Cumulative") %>%
    mutate(Type = MySce)

  # Update plot_data_long_placebo with main first
  plot_data_long_placebotest <- bind_rows(plot_data_long_main, plot_data_long_placebo)

  # Filter to period with an effect (YearWeek >= )
  plot_data_long_placebotest <- plot_data_long_placebotest %>% filter(YearWeek >= 202001)

  MyPlotPlacebo <- create_placebo_plot(plot_data_long_placebotest, lsw, MySce)
  MyPlotPlacebo
  ggsave(paste0(results_folder, "/Placebo_",MySce, "_", plot_info, ".png"), plot = MyPlotPlacebo, width = 5, height = 5, dpi = 300, bg = "white")
} #end if(runPlacebo)

##End loops ----
  # Logging
  cat("Ending i-loop", i, ". Time spent", difftime(Sys.time(), mytime, units = "secs"), "\n\n", file = log_file, append = TRUE)
  
} ###End i-loop ----


  
if(!onlyPlots){
#### Save weights and plot data ----------------------------------------------
  # Set file name info
  file_agegroup <- dependent_folder_name
  file_predictor <- mypredictor
  file_prewindow <- substr(prewindow, 1, 4)
  file_info <- paste(file_agegroup, file_prewindow ,file_predictor, sep="_")
  file_info <- paste0("_", file_info)
  
  write.xlsx(dfResults, file = paste0(results_folder, "/Results" , file_info, ".xlsx"))
  write.xlsx(dfWeights, file = paste0(results_folder, "/Weights" , file_info, ".xlsx"))
  save(comparison_table_list, file = paste0(results_folder, "/comparison_table_list" , file_info, ".RData"))
  save(plot_data_list, file = paste0(results_folder, "/plot_data_list" , file_info, ".RData"))
  save(plot_data_long_list, file = paste0(results_folder, "/plot_data_long_list" , file_info, ".RData"))
  save(plot_data_cum_list, file = paste0(results_folder, "/plot_data_cum_list" , file_info, ".RData"))
  
#### Store comparison tables in an Excel-file for easy access ---------------
  # Create a new workbook
  wb <- createWorkbook()
  
  #Store comparison table2 2 for comparison
  addWorksheet(wb, "Compare Tables")
  
  # Initialize the starting row
  current_row <- 3
  
  # Loop over the comparison_table_list and write each table to the same sheet with 5-row spacing
  for (MySce in names(comparison_table_list)) {
    
    # Write the header (MySce) at the current position in column 3
    writeData(wb, sheet = "Compare Tables", x = MySce, startCol = 3, startRow = current_row)
    
    # Write the table data starting 1 row below the header
    writeData(wb, sheet = "Compare Tables", 
              x = comparison_table_list[[MySce]], 
              startCol = 3, 
              startRow = current_row + 1)
    
    # Update current_row to leave a 5-row gap after the table
    current_row <- current_row + nrow(comparison_table_list[[MySce]]) + 5
  }

  # Save the workbook
  saveWorkbook(wb, paste0(results_folder, "/comparison_tables", file_info, ".xlsx"), overwrite = TRUE)
  
  
}
  save(week_to_yearweek, file = paste0(results_folder, "/week_to_yearweek.RData"))
} ###End of p-loop ----


mytime
Sys.time()-mytime



# Extra graphs showing problems with SCM------------------------------------------------------
print("Extra calculations showing problems with SCM")
MySynth <- NULL
for (k in 1:4){
  #Load plot_data_long
  if(k == 1) {
    load(paste0("2021-RTotal/201627/none/plot_data_long_list_RTotal_2016_none.RData"))
    MySynth <- "Naïve"
  } else if(k == 2){
    load(paste0("2021-RTotal/201627/ps11-14/plot_data_long_list_RTotal_2016_ps11-14.RData"))
    MySynth <- "W11-14"
  } else if(k == 3){
    load(paste0("2021-RTotal/201627/ps11-15/plot_data_long_list_RTotal_2016_ps11-15.RData"))
    MySynth <- "W11-15"
  } else if(k == 4){
    load(paste0("2021-RTotal/201627/ps11-16/plot_data_long_list_RTotal_2016_ps11-16.RData"))
    MySynth <- "W11-16"
  }


  plot_data_long_w7 <- plot_data_long_list[["SWE_w7"]] %>% filter(YearWeek >= 202001 & YearWeek <= 202044)
  plot_data_long_w8 <- plot_data_long_list[["SWE_w8"]] %>% filter(YearWeek >= 202001 & YearWeek <= 202044)
  plot_data_long_w9 <- plot_data_long_list[["SWE_w9"]] %>% filter(YearWeek >= 202001 & YearWeek <= 202044)
  plot_data_long_w10 <- plot_data_long_list[["SWE_w10"]] %>% filter(YearWeek >= 202001 & YearWeek <= 202044)

  df <- bind_rows(plot_data_long_w7 %>% filter(Type == "Synthetic") %>% mutate(Type = "w7"),
                                         plot_data_long_w8 %>% filter(Type == "Synthetic") %>% mutate(Type = "w8"),
                                         plot_data_long_w9 %>% filter(Type == "Synthetic") %>% mutate(Type = "w9"),
                                         plot_data_long_w10 %>% filter(Type == "Synthetic") %>% mutate(Type = "w10"))

  #Lockdown sequence week
  lsw <- df %>% filter(YearWeek == lw) %>% pull(Sequence) %>% max()  # Using 'pull' to directly extract the column values


  min_ylim <- 12
  max_ylim <- 26
  yplace <- NULL
  ylabel <- NULL



  ## Plot --------------------------------------------------------------------


  # create_four_lines_plot <- function(df, lsw, min_ylim = NULL, max_ylim = NULL, yplace = NULL, ylabel = NULL) {
  # Find the position of 'lsw' within the sorted unique weeks
  all_weeks <- sort(unique(df$Sequence))
  lw_position <- match(lsw, all_weeks)

  # Generate sequences backwards and forwards from 'lsw', including 'lsw' itself
  myLabelCount <- ceiling(length(all_weeks) / 12)
  backward_indices <- seq(lw_position, 1, by = -myLabelCount)
  forward_indices <- seq(lw_position, length(all_weeks), by = myLabelCount)

  # Combine the sequences and remove duplicates
  combined_indices <- unique(c(backward_indices, forward_indices))
  combined_indices <- sort(combined_indices)  # Ensure it's sorted

  # Select the custom breaks and labels using the combined indices
  custom_breaks <- all_weeks[combined_indices]
  custom_labels <- unique(df$Year_w_Week[match(custom_breaks, df$Sequence)])

  # Set Y-scale
  if (is.null(max_ylim)) {
    max_outcome <- max(df$Outcome)
    max_ylim <- ceiling(max_outcome)
  }

  if (is.null(min_ylim)) {
    min_outcome <- min(df$Outcome)
    min_ylim <- floor(min_outcome)
  }

  if (is.null(yplace)) {
    yplace <- 0
  }

  if (is.null(ylabel)) {
    ylabel <- "Mortality/100,000"
  }

  my_colors <- rep("black", length(unique(df$Type)))

  # Plotting
  MyPlot <-
    ggplot(df, aes(x = Sequence, y = Outcome, linetype = Type, color = Type)) +
    geom_line() +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.background = element_rect(fill = "white"),  # Set panel background to white
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels
    scale_x_continuous(name = NULL, breaks = custom_breaks, labels = custom_labels) +
    scale_color_manual(values = my_colors) +  # Ensure one color per Type
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
    labs(title = NULL,
         y = ylabel,
         linetype  = NULL,
         color = NULL) +  # Removes default legend title
    geom_vline(xintercept = lsw, linetype = "dashed", color = "black", size = 1, show.legend = FALSE) +
    annotate("text", x = lsw - myLabelCount / 4, y = yplace, label = "Lockdown week", size = 4, color = "black", angle = 90, hjust = 0) +
    ylim(min_ylim, max_ylim) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8)  # Add horizontal line at y = 0

  MyPlot

  ggsave(paste0("2021-RTotal/201627/Synths_", MySynth, ".png"), plot = MyPlot, width = 5, height = 5, dpi = 300, bg = "white")
}


mytime
Sys.time()-mytime


# Write  end time stamp to log
cat(
  "Log ended at",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),  # Format as YYYY-mm-dd HH:MM:SS
  "\n\n",
  file = log_file, append = TRUE
)
