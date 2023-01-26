#Set working directory and assign to the Working Directory object

setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Data (for non-API source data)")
WorkingDirectory <- setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Data (for non-API source data)")

#Save workspace
save.image("ACS2020Data.RData")

#Save history
savehistory("ACS2020Data.Rhistory")

#Load workspace file
load("ACS2020Data.RData")
#Load history
loadhistory("ACS2020Data.Rhistory")

#Load tidycensus library to interact with Census API
library(tidycensus)

census_api_key("b60c9ca668ed5dfba8f7458af3863bca4ace5705", install = TRUE)

census_api_key(Sys.getenv("CENSUS_API_KEY"))   #RUN THIS EVERY TIME YOU'RE USING TIDYCENSUS

#key points -- use get_acs() for American Community Survey;
#use get_decennial() for 2010 or 2020 decennial census results
#note-- the syntax for the various geography levels, such as "state" to get
#state level or "us" to get national data

install.packages("xlsx")
install.packages("rJava")
install.packages("palmerpenguins")


#load necessary libraries

library(tidyverse)
library(janitor)
library(rmarkdown)
library(xlsx)
library(ggthemes)
library(knitr)
library(plotly)
library(purrr)
library(dplyr)                                                  #package useful for data manipulation
library(rio)
library(rJava)
library(lubridate)
library(openxlsx)

# ----------------------------- Getting ACS Data ---------------------------
get_acs()
get_decennial()
get_estimates()
get_pums()

#How to get a list of variables (knowing which ones to pull):
var2020 <- load_variables(2020, "acs5") #search table for variable names
var2016 <- load_variables(2016, "acs5")

#Resource to reference to find variables or tables you need: https://censusreporter.org/



#I'm getting ACS 5YR State and County level information. Since I didn't specify a state,it will give all states plus DC&PR
#I can limit this to a particular state by adding state="stateabbrev" after the survey line
#I can add more variables by going back into the code and adding more to the list

#---------------Get data multiple years at the same time---------------------
#I'm using the map_dfr() function from the purrr package to write
#a loop. It will loop through the years that we've told it to pull

#Assigning variables to a list of values

autopopulator_variables <- c(TotalPop = "B01003_001",
                             White = "B03002_003",
                             Black = "B03002_004",
                             AmIndianAlNative = "B03002_005",
                             Asian = "B03002_006",
                             NatHawPacisl = "B03002_007",
                             HispLatino = "B03002_012",
                             NumPop16over = "B23001_001",
                             Num16overinLF = "B23025_002",
                             Num16overinLFemp = "B23025_004",
                             Num16overinLFunemp = "B23025_005",
                             TotalHouseholds = "B11001_001",
                             MedHouseholdIncome = "B19013_001",
                             aminalnatincome = "B19013C_001",
                             asianincome = "B19013D_001",
                             blackincome = "B19013B_001",
                             hispanicincome = "B19113I_001",
                             nathawincome = "B19013E_001",
                             whitealoneincome = "B19113H_001",
                             MedFamilyIncome = "B19119_001",
                             PctBelowPov = "B17001_002",
                             Pop25over = "B15003_001",
                             FemPop25overHSequiv = "B15002_028",
                             FemPop25overBaBs = "B15002_032",
                             MalePop25overHSequiv = "B15002_011",
                             MalePop25overBaBs = "B15002_015",
                             HSdiploma = "B15003_017",
                             GED = "B15003_018",
                             Somecollege1 = "B15003_019",
                             Somecollege2 = "B15003_020",
                             Associates = "B15003_021",
                             Bachelors = "B15003_022",
                             Masters = "B15003_023",
                             ProfSchool = "B15003_024",
                             Doctorate = "B15003_025",
                             TotalHousingUnits = "B25001_001",
                             OccupiedUnits = "B25002_002",
                             OwneroccUnits = "B25003_002",
                             TotalAmInAlNatHH = "B25003C_001",
                             AmInAlNatOwnerOcc = "B25003C_002",
                             TotalAsianHH = "B25003D_001",
                             AsianOwnerOcc = "B25003D_002",
                             TotalBlackHH = "B25003B_001",
                             BlackOwnerOcc = "B25003B_002", 
                             TotalHispHH = "B25003I_001", 
                             HispOwnerOcc = "B25003I_002", 
                             TotalNatHawHH = "B25003E_001",
                             NatHawOwnerOcc = "B25003E_002",
                             TotalWhiteHH = "B25003H_001",
                             WhiteOwnerOcc = "B25003H_002",
                             RenteroccUnits = "B25008_003",
                             AmInAlNatRenterOcc = "B25003C_003",
                             AsianRenterOcc = "B25003D_003",
                             BlackRenterOcc = "B25003B_003",
                             WhiteRenterOcc = "B25003H_003",
                             NatHawRenterOcc = "B25003E_003")


#Creating a list of the years that I want to pull 
#The API only goes back to 2012
years <- lst(2015, 2020)       #note this is the letter l and not the number 1!

#Next is the loop code wrapped around the standard TidyCensus syntax
#Note that we're getting county level from acs 5 year and added clean_names()
#function from janitor package

County2020and2016variables <- map_dfr(
  years,
  ~get_acs(
    geography = "county",
    variables = autopopulator_variables,
    output= "wide",                 #so variables aren't in one column
    year = .x,
    survey = "acs5"
  ),
  .id = "year")



State2020and2016variables <- map_dfr(
  years,
  ~get_acs(
    geography = "state",
    variables = autopopulator_variables,
    output= "wide",                 #so variables aren't in one column
    year = .x,
    survey = "acs5"
  ),
  .id = "year")



#Removing unwanted columns--specifying which columns I want to keep
FinalCounty2020and2016 <- County2020and2016variables[ , names(County2020and2016variables) %in% c("year","GEOID", "NAME", 
                                                                                                 "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                                                                                 "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                                                                                 "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                                                                                 "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                                                                                 "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                                                                                 "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                                                                                 "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", 
                                                                                                 "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                                                                                 "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


FinalState2020and2016 <- State2020and2016variables[ , names(State2020and2016variables) %in% c("year","GEOID", "NAME", 
                                                                                                 "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                                                                                 "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                                                                                 "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                                                                                 "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                                                                                 "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                                                                                 "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                                                                                 "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", 
                                                                                                 "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                                                                                 "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]



#---------------------------Exporting data frame to excel-----------------
#store directory path
my_path <- "C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Output"

 

#option 2 for exporting or appending more than one sheet

write.xlsx2(FinalCounty2020and2016, paste0(my_path, "Final2020ACSdata.xlsx"), sheetName = "county") 
write.xlsx2(FinalState2020and2016, paste0(my_path, "Final2020ACSdata_state.xlsx"), sheetName = "state", append = TRUE)

