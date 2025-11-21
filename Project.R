library(tidyverse)
#1
#loading data
  setwd("D:/Stage 2_Term 5/Analytics Programming/Report/AP Project")
  engines<-read.csv("Engine.csv")
  automobile<-read.csv("Automobile.csv")
  maintenance<-read.csv("Maintenance.csv")

#replace "?" with NA
  engines[engines == "?"] <- NA #replace "?" with NA in engines
  automobile[automobile == "?"] <- NA #replace "?" with NA in automobile
  maintenance[maintenance == "?"] <- NA #replace "?" with NA in maintenance
  #Code testing
  remain_NA <- sum(engines == "?", na.rm = TRUE)
          +sum(automobile == "?", na.rm = TRUE)
          +sum(maintenance == "?", na.rm = TRUE) #Total number of remaining <NA> after replacement
  #If there is no remaining <NA> after replacement, show that All '?' characters are replaced with NA.
  if(remain_NA == 0) {
    print("All '?' characters are replaced with NA.")
  } else {
    print(paste("There are", remaining_q, "'?' characters still remain!"))
  }
#inspect the data structure
  print(head(engines,10)) # display the top ten rows of the data
  print(head(automobile,10))
  print(head(maintenance,10))
  ls.str(engines) # list and inspect all objects
  ls.str(automobile)
  ls.str(maintenance)

#number of rows were affected in total after replacement
  #Step 1. Calculate rows with NA for each data frame
  na_rows_engines <- sum(rowSums(is.na(engines))) # Finding the number of <NA> in engines
  na_rows_automobile <- sum(rowSums(is.na(automobile))) # Finding the number of <NA> in automobile
  na_rows_maintenance <- sum(rowSums(is.na(maintenance))) # Finding the number of <NA> in maintenance
  
  #Step 2. The total number of rows with NA across all three data frames 
  total_na_rows <- na_rows_engines + na_rows_automobile + na_rows_maintenance
  
  #step 3. Present the result
  cat("engines has", na_rows_engines, "rows with at least one NA.\n")
  cat("automobile has", na_rows_automobile, "rows with at least one NA.\n")
  cat("maintenance has", na_rows_maintenance, "rows with at least one NA.\n")
  cat("Total rows with at least one NA across the three data frames:", total_na_rows,".\n")
  cat("There are",total_na_rows,"were affected in total after replacement.\n")
  
#Does this change alter the data distribution?
  #Step 1: Input data before change
  engines_bef<-read.csv("Engine.csv")
  automobile_bef<-read.csv("Automobile.csv")
  maintenance_bef<-read.csv("Maintenance.csv")
  
  #Step 2: Select 'Horsepower' as a specific column to analyze
  after_value <- engines$Horsepower
  bef_value <- engines_bef$Horsepower
  
  #Step 3: Prepare the "Before" data for comparison
  #Taking the raw column, remove "?", and convert the rest to numbers
  bef_value <- engines_bef$Horsepower
  bef_value <- bef_value[bef_value != "?"] # Remove rows with "?"
  bef_value <- as.numeric(bef_value) # Convert text to number
  
  #Step 4: Prepare the "After" data for comparison
  #Taking the after changed column, remove <NA>, and convert the rest to numbers
  after_value <- engines$Horsepower
  after_value <- after_value[!is.na(after_value)] # Remove rows with <NA>
  after_value <- as.numeric(after_value) # Convert text to number
  
  #Step 5: Compare the Valid Data
  if(identical(after_value, bef_value)) {
    print("1. CONCLUSION: The data distribution remains UNCHANGED after replacing '?' with NA.\n")
      } else {
    print("1. CONCLUSION: The data distribution has CHANGED after replacing '?' with NA.\n")
      }  

#Convert categorical variables BodyStyles, FuelTypes, ErrorCodes to factors
  automobile$BodyStyles <- as.factor(automobile$BodyStyles)
  engines$FuelTypes <- as.factor(engines$FuelTypes)
  maintenance$ErrorCodes <- as.factor(maintenance$ErrorCodes)
  #Code testing
  if(is.factor(automobile$BodyStyles) & is.factor(engines$FuelTypes) & is.factor(maintenance$ErrorCodes)) {
  # the condition to test if all columns are formatted as factors
    print("All variables are converted to Factors.")
  } else {
    print("One or more variables are not factors.")
  }
  #present the result
  print(automobile$BodyStyles)
  print(engines$FuelTypes)
  print(maintenance$ErrorCodes)

#Replace the missing values in column Horsepower with the median horsepower
  #Step 1. Calculate the Median of Horsepower
  median_hp <- median(engines$Horsepower, na.rm = TRUE)

  #Step 2. Replace <NA> with the Median
  engines$Horsepower[is.na(engines$Horsepower)] <- median_hp
  
  #Step 3: Code testing
  remaining_nas <- sum(is.na(engines$Horsepower)) # calculating the 'NA' that did not replace
    if(remaining_nas == 0) {
    print(paste("All missing values are replaced. Median used:", median_hp))
  } else {
    print(paste("There are", remaining_nas, "missing values still remain!"))
  }
  
#Select the appropriate chart type and display: horsepower distribution  
  library(ggplot2)
  engines$Horsepower <- as.numeric(engines$Horsepower)
  ggplot(engines, aes(x = Horsepower)) +
  geom_histogram(binwidth = 10, fill = "#4E84C4", color = "white", alpha = 0.8)
#2
#Analyse Horsepower across Engine Types
  # Step 1: Replace the 'NA' value in EngineTypes to 'Other Types'
  engines$EngineType[is.na(engines$EngineType)]<-"Other Types"
  engines$EngineType<as.factor(engines$EngineType)
  # Step 2: Grouping the data
  Horsepower_EngTypes <- engines %>%  
    group_by(EngineType) %>%
  #step 3: Find the statistic values of horse power for each engine type
    summarise(
      Count = n(), #finding the number of observation (Horsepower) by Engine Types
      Mean_HP = mean(Horsepower, na.rm = TRUE), #finding the average Horsepower by Engine Types
      SD_HP = sd(Horsepower, na.rm = TRUE), #finding the standard deviation of Horsepower by Engine Types
      Min_HP = min(Horsepower, na.rm = TRUE), #finding the minimum value of Horsepower by Engine Types
      Max_HP = max(Horsepower, na.rm = TRUE) #finding the maximum value Horsepower by Engine Types
    )
  #Step 4: Present the result
  print("Statistical Summary of Horsepower by Engine Type")
  print(Horsepower_EngTypes)
  #Step 5: Visualise Horsepower across Engine Types using the histogram
  ggplot(engines, aes(x = Horsepower, fill=EngineType)) +
    geom_histogram(binwidth = 10, color = "white", alpha = 0.8)+
    labs( title = "Distribution of Horse power across Engine Types")
  
#Analyse Horsepower Across Engine Size Groups
  #Step 1: Create the Groups (Bins for EngineSize)
  engines$SizeGroup <- cut(engines$EngineSize, 
                           breaks = c(0, 60, 90, 190, 299, Inf), 
                           labels = c("0-60","61-90", "91-190", "191-299", "300+"),
                           right = TRUE)
  
  #Step 2: Group Horsepower by EngSize
  Horsepower_EngSize <- engines %>%
    group_by(SizeGroup) %>%
  #Step 3: Find the statistic values of horse power for each engine size
    summarise(
      Count = n(),
      Mean_HP = mean(Horsepower, na.rm = TRUE),
      SD_HP = sd(Horsepower, na.rm = TRUE),
      Min_HP = min(Horsepower, na.rm = TRUE),
      Max_HP = max(Horsepower, na.rm = TRUE)
    )
  #Step 4: Present the the calculated statistic
  print("Statistical Summary of Horsepower by Engine Size")
  print(Horsepower_EngSize)
  #Step 5: Visualise Horsepower across Engine Size using the histogram
  ggplot(engines, aes(x = Horsepower, fill=SizeGroup)) +
    geom_histogram(binwidth=10, color = "white", alpha = 0.7)+
    labs( title = "Distribution of Horse power across Engine Size")
#3
#Do Diesel Cars Have Higher City Mpg?
  #Step 1: Merge the datasets
  merged_data <- merge(automobile, engines, by = "EngineModel")
  merged_data$CityMpg <- as.numeric(as.character(merged_data$CityMpg)) #Convert CityMpg to numeric
  merged_data$FuelTypes <- as.factor(merged_data$FuelTypes) #Convert FuelTypes to factor
  #Step 2: Group CityMpg by Fuel Types and Descriptive Statistics
  mpg_fuel <- merged_data %>%
    group_by(FuelTypes) %>%
    summarise(
      Count = n(),
      Average_CityMpg = mean(CityMpg, na.rm = TRUE),
    )
  print(mpg_fuel)
  CityMpg_diesel<- mpg_fuel$Average_CityMpg[mpg_fuel$FuelTypes == "diesel"]
  CityMpg_gas<- mpg_fuel$Average_CityMpg[mpg_fuel$FuelTypes == "gas"]
  cat("The average CityMpg of diesel cars is",CityMpg_diesel,"\n")
  cat("The average CityMpg of gasoline cars is",CityMpg_gas,"\n")
  if(CityMpg_diesel>CityMpg_gas){
    print("Conclusion: The diesel cars have HIGHER average CityMpg the than gasoline cars")
    }else{
    print("Conclusion: The diesel cars have LOWER average CityMpg the than gasoline cars")
  }
# How does DriveWheels affect fuel efficiency?
  drive_stats <- automobile %>%
    group_by(DriveWheels) %>%
    summarise(
      Count = n(),
      Avg_City_Mpg = mean(CityMpg, na.rm = TRUE),
      Avg_Hwy_Mpg = mean(HighwayMpg, na.rm = TRUE),
      )
  print("Fuel Efficiency by Drive Wheels")
  print(drive_stats)
#Filter out those engines in the dataset that have trouble or are suspected of having trouble
  #Step 1. Data Preparation & Filtering
  safe <- "No error"
  troubled_maintenance <- maintenance %>%
    filter(Troubles != safe & !is.na(Troubles))
  #Step 2. Linking The data frames (The Chain)
  #a. Link Cars to the Troubled Maintenance records
  # right_join keeps all rows from 'troubled_maintenance' and attaches car info.
  troubled_cars<- automobile %>%
    right_join(troubled_maintenance, by = "PlateNumber")
  #b. Link Engines to those Troubled Cars
  #right_join keeps all rows from 'troubled_cars' and attaches engine info.
  target_engines_merged <- engines %>%
    right_join(troubled_cars, by = "EngineModel")
  #c. Ensure Uniqueness
  # Joins can create duplicates if a car has multiple troubles. 
  # Use distinct() to get a unique list of engines.
    target_engines <- target_engines_merged %>%
    distinct(EngineModel, .keep_all = TRUE)
  #Step 3. FINAL OUTPUT  
    cat("Total Troubled/Suspected Engines Identified:", nrow(target_engines),".\n")
    print(head(target_engines,10))

#what are the top 5 most common troubles related to the engines
  #Step 1. Data preparation
  #The requirement states: ErrorCodes == 1 means "Engine Fails".
  #We only want to analyze these specific rows.  
  engine_troubles_data <- maintenance %>%
  filter(as.character(ErrorCodes) == "1")
  #Step 2. Calculate frequency (The Top 5)
  top_5_engine_issues <- engine_troubles_data %>%
  count(Troubles, sort = TRUE)%>%
  head(5)
  #Step 3. Present the result
  print("Top 5 Troubles")
  print(top_5_engine_issues)

#Do the troubles differ between engine types?
  #Step 1. Data Preparation
  safe_value <- "No error"
  trouble_analysis_data <- maintenance %>%
    filter(Troubles != safe_value & !is.na(Troubles)) %>%
    inner_join(automobile, by = "PlateNumber") %>%
    inner_join(engines, by = "EngineModel")
  
  #Step 2. Create the summary
   summary_table <- trouble_analysis_data %>%
    group_by(EngineType) %>%
    summarise(number_troubles = n_distinct(Troubles))
  
  #Step 3. Present the result
  print(summary_table)
  
  #Step 4. Conclusion
  if (length(unique(summary_table$number_troubles)) == 1) {
    print("The troubles are the same between engine types")
  } else {
    print("The troubles are different between engine types")
  }

#4
#Error type (ErrorCodes) occurs most frequently
 #Step 1. Calculate Frequency
 #We count how many times each code appears.
  error_counts <- maintenance %>%
  count(ErrorCodes, sort = TRUE)  # Sorts highest to lowest automatically
  print("Error Code Frequency Table")
  print(error_counts)
  #Step 2. Conclusion
  # Extract the winner (the first row, since we sorted it)
  top_code <- as.character(error_counts$ErrorCodes[1])
  top_count <- error_counts$n[1]
  cat("The most frequent Error Type is Code:", top_code,".")
  cat("It appeared", top_count)
  if(top_code == "0") {
    Cat("(Meaning: 'No Error' is the most common state).")
  } else if (top_code == "1") {
    cat("(Meaning: 'Engine Failure' is the most common issue).")
  } else if (top_code == "-1") {
    cat("(Meaning: 'Other Component Failure' is the most common issue).")
  }
  
#Factors that might influence the maintenance methods
  # --- TABLE 1: ERROR CODES (Counts) ---
  print("--- Counts: Error Codes vs Methods ---")
  table1 <- table(trouble_analysis_data$ErrorCodes, trouble_analysis_data$Methods)
  print(table1)
  
  # --- TABLE 2: ENGINE TYPE (Counts) ---
  print("--- Counts: Engine Type vs Methods ---")
  table2 <- table(trouble_analysis_data$EngineType, trouble_analysis_data$Methods)
  print(table2)