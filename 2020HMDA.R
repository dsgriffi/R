#Set working directory and assign to the Working Directory object

setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/HMDA/Data")
WorkingDirectory <- setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/HMDA/Data")

#Save workspace
save.image("2020HMDA.RData")

#Save history
savehistory("2020HMDA.Rhistory")

#Load workspace file
load("2020HMDA.RData")

#Load history
loadhistory("2020HMDA.Rhistory")

#install necessary packages
install.packages("data.table")
install.packages("future")
install.packages("arrow") #this allows manipulation of massive data
install.packages("plyr")
install.packages("forcats")


#load necessary libraries
library(arrow)
library(data.table)
library(tidyverse)
library(janitor)
library(ggthemes)
library(rJava)
library(lubridate)
library(openxlsx)
library(purrr)
library(readr)
library(plyr)
library(dplyr)
library(forcats)

#Import Massive Data File, but dont use this one for this example

hmda2020 <- read_csv("2020HMDA.csv")


#IMPORT SPECIFIC COLUMNS AND SPECIFY TYPES--saves space when reading huge csv file
hmda <- fread("2020HMDA.csv", select = c("activity_year", "lei", "derived_msa_md", "state_code", 
              "county_code", "census_tract", "derived_loan_product_type",
              "derived_dwelling_category", "action_taken", "loan_purpose",
              "loan_amount", "interest_rate", 
              "rate_spread", "hoepa_status", "origination_charges", 
              "loan_term", "occupancy_type", "income",
              "applicant_ethnicity_1", "applicant_race_1", "denial_reason_1", 
              "tract_population", "tract_minority_population_percent", 
              "ffiec_msa_md_median_family_income", "tract_to_msa_income_percentage",
              "tract_owner_occupied_units", "tract_one_to_four_family_homes"))


#View column data types
spec(hmda)


#check the number of cases for a state to see if it matches raw data download
nrow(subset(hmda, state_code == "AK" & action_taken == "3"))

#Filter file to only show loan originations and loan denials

hmda2020 <- hmda %>%
  select("activity_year", "lei", "derived_msa_md", "state_code", 
         "county_code", "census_tract", "derived_loan_product_type",
         "derived_dwelling_category", "action_taken", "loan_purpose",
         "loan_amount", "interest_rate", 
         "rate_spread", "hoepa_status", "origination_charges", 
         "loan_term", "occupancy_type", "income",
         "applicant_ethnicity_1", "applicant_race_1", "denial_reason_1", 
         "tract_population", "tract_minority_population_percent", 
         "ffiec_msa_md_median_family_income", "tract_to_msa_income_percentage",
         "tract_owner_occupied_units", "tract_one_to_four_family_homes") %>%
  filter(action_taken == "1" | action_taken == "3")

#check the number of cases for a state to see if it matches raw data download
nrow(subset(hmda2020, state_code == "AK" & action_taken == "3"))

#remove original to reduce storage space being used
rm(hmda)

#View column data types
sapply(hmda2020, class)


#Convert data types: figure out how to do this all at once, for now, complete each row separately

hmda2020$interest_rate <- as.numeric(hmda2020$interest_rate) 
hmda2020$rate_spread <- as.numeric(hmda2020$rate_spread)
hmda2020$origination_charges <- as.numeric(hmda2020$origination_charges)
hmda2020$loan_term <- as.numeric(hmda2020$loan_term)
hmda2020$action_taken <- as.factor(hmda2020$action_taken) 
hmda2020$loan_purpose <- as.factor(hmda2020$loan_purpose)
hmda2020$applicant_ethnicity_1 <- as.factor(hmda2020$applicant_ethnicity_1)
hmda2020$applicant_race_1 <- as.factor(hmda2020$applicant_race_1)
hmda2020$denial_reason_1 <- as.factor(hmda2020$denial_reason_1)


#Replace negative numbers in income column
hmda2020$income[hmda2020$income <0] <- 0

#income column needs to be multiplied by 1,000 because it is currently listed in hundreds
hmda2020 <- hmda2020 %>%
  mutate(IncomeFinal = income * 1000)
#view result
hmda2020$IncomeFinal[1:3]



#Rename factor variables
hmda2020$applicant_race_1 <- fct_recode(hmda2020$applicant_race_1, White = "5") 
hmda2020$applicant_race_1 <- fct_recode(hmda2020$applicant_race_1, Asian = "2", Asian = "21", Asian = "22", Asian = "23", 
                                          Asian = "24", Asian = "25", Asian = "26", Asian = "27")
hmda2020$applicant_race_1 <- fct_recode(hmda2020$applicant_race_1, Black = "3")
hmda2020$applicant_race_1 <- fct_recode(hmda2020$applicant_race_1, NativeHawaiianorPacificIslander = "4", NativeHawaiianorPacificIslander = "41", NativeHawaiianorPacificIslander = "42", 
                                          NativeHawaiianorPacificIslander = "43", NativeHawaiianorPacificIslander = "44")                                         
hmda2020$applicant_race_1 <- fct_recode(hmda2020$applicant_race_1, "NA" = "6")
hmda2020$applicant_race_1 <- fct_recode(hmda2020$applicant_race_1, "NA" = "7")                                       

hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, HispanicLatino = "1") 
hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, HispanicLatino = "11")                                               
hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, HispanicLatino = "12")
hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, HispanicLatino = "13")
hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, HispanicLatino = "14")
hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, NotHispanic = "2") 
hmda2020$applicant_ethnicity_1 <- fct_recode(hmda2020$applicant_ethnicity_1, "NA" = "3", "NA" = "4")

#preview data
hmda2020$applicant_ethnicity_1[1:10]
                                          
#export filtered data to csv: saving this as I go along so that next time I open R, I don't have
#to start from the beginning to get the clean file. I can start right from the HMDA2020trunc1.csv

write.csv(hmda2020, "HMDA2020trunc.csv")                                         


#Import filtered hmda data to continue manipulating instead of starting from beginning:
hmda2020 <- read_csv("HMDA2020trunc.csv")


#View structure of data
str(hmda2020)

#remove NA values so that you can run calculations OR think through how you should handle missing data for each variable
#INSERT COUNTY NAMES
#look up how to merge in information from other table? or how to replace values





#-----------------------------------------OTHER DATA CLEANING TIPS:

hmdafilter2 <- gsub(",", "", hmdafilter2) #this is an example of the code you can run before
#converting to numeric if you want non-numeric values to be converted to numeric, but in this case,
#its NAs so I want to leave them and ignore the warning message




#Import .csv file to global environment
hmda2020 <- read.csv("2020HMDA.csv", header = TRUE, sep = ",")  #this is how to do it, but my file is too large so I have to do option 2 below

#Used fread function from data.table package to read in extremely large files
hmda2020 <- fread("2020HMDA.csv", header = TRUE, sep = ",")



sapply(hmda2020, class)
  

#view first few rows of data
hmda2020[1:3]

#Selecting the columns we want to keep
vHMDA2020 <- hmda2020 %>%
  select("activity_year", "lei", "derived_msa_md", "state_code", 
         "county_code", "census_tract", "derived_loan_product_type",
         "derived_dwelling_category", "derived_ethnicity",
         "derived_race", "action_taken", "loan_purpose", "lien_status",
         "loan_amount", "interest_rate", 
         "rate_spread", "hoepa_status", "origination_charges", 
         "loan_term", "occupancy_type", "income", "debt_to_income_ratio",
         "applicant_ethnicity_1", "applicant_race_1", "denial_reason_1", 
         "tract_population", "tract_minority_population_percent", 
         "ffiec_msa_md_median_family_income", "tract_to_msa_income_percentage",
         "tract_owner_occupied_units", "tract_one_to_four_family_homes")

#Remove rows where state code is blank
vHMDA2020[vHMDA2020 == 0] <- NA

vHMDA2020 %>%                                      
  drop_na(state_code)


vHMDA2020[1:3]

#view data types to make sure they're correct
sapply(vHMDA2020, class)
#specify columns to change
change_columns <- c("activity_year", "interest_rate", "origination_charges")


view(vHMDA2020)

#More ways of importing data:
#specify columns  data types with the import to avoid parsing errors, but dont use this for this example
hmda <- read_csv("2020HMDA.csv", col_types =cols(.default = "?", interest_rate = "i", 
                                                 rate_spread = "i", origination_charges = "i", 
                                                 loan_term = "i", action_taken = "i", loan_purpose = "i", 
                                                 applicant_ethnicity_1 = "i", applicant_race_1 = "i", 
                                                 denial_reason_1 = "i"))

#more data cleaning tips
#Remove unwanted columns: selecting columns I want to keep
hmdafilter <- hmda2020 %>%
  select("activity_year", "lei", "derived_msa_md", "state_code", 
         "county_code", "census_tract", "derived_loan_product_type",
         "derived_dwelling_category", "action_taken", "loan_purpose",
         "loan_amount", "interest_rate", 
         "rate_spread", "hoepa_status", "origination_charges", 
         "loan_term", "occupancy_type", "income",
         "applicant_ethnicity_1", "applicant_race_1", "denial_reason_1", 
         "tract_population", "tract_minority_population_percent", 
         "ffiec_msa_md_median_family_income", "tract_to_msa_income_percentage",
         "tract_owner_occupied_units", "tract_one_to_four_family_homes")


