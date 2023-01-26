#Set working directory and assign to the Working Directory object

setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Data (for non-API source data)")
WorkingDirectory <- setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Data (for non-API source data)")

#Save workspace
save.image("ACS2020Tracts.RData")

#Save history
savehistory("ACS2020Tracts.Rhistory")

#Load workspace file
load("ACS2020Tracts.RData")
#Load history
loadhistory("ACS2020Tracts.Rhistory")

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
var2015 <- load_variables(2015, "acs5")

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
                             hispanicincome = "B19013I_001",
                             nathawincome = "B19013E_001",
                             whitealoneincome = "B19013H_001",
                             MedFamilyIncome = "B19119_001",
                             PctBelowPov = "B17001_002",
                             BelowPovTotalpop = "B17001_001",
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

alabama<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "AL",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


alaska<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "AK",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


arizona<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "AZ",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


arkansas<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "AR",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


california<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "CA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


colorado<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "CO",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


connecticut<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "CT",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


delaware<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "DE",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")

florida<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "FL",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


georgia<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "GA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


hawaii<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "HI",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


idaho<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "ID",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


illinois<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "IL",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


indiana<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "IN",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


iowa<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "IA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


kansas<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "KS",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


kentucky<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "KY",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


louisiana<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "LA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


maine<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "ME",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


maryland<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MD",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")



massachusetts<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


michigan<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MI",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


minnesota<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MN",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


mississippi<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MS",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


missouri<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MO",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


montana<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "MT",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


nebraska<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NE",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


nevada<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NV",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


newhampshire<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NH",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


newjersey<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NJ",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")

newmexico<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NM",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


newyork<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NY",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


northcarolina<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "NC",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")

northdakota<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "ND",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


ohio<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "OH",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


oklahoma<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "OK",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


oregon<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "OR",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


pennsylvania<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "PA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


rhodeisland<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "RI",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


southcarolina<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "SC",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


southdakota<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "SD",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


tennessee<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "TN",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


texas<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "TX",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


utah<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "UT",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


vermont<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "VT",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


virginia<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "VA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


washington<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "WA",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


westvirginia<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "WV",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


wisconsin<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "WI",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")


wyoming<- map_dfr(                      
  years,
  ~get_acs(
    geography = "tract",
    state = "WY",
    variables = autopopulator_variables,
    output= "wide",                 
    year = .x,
    survey = "acs5"
  ),
  .id = "year")



#Removing unwanted columns--specifying which columns I want to keep
alabama <- alabama[ , names(alabama) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


alaska <- alaska[ , names(alaska) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

arizona <- arizona[ , names(arizona) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

arkansas <- arkansas[ , names(arkansas) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

california <- california[ , names(california) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

colorado <- colorado[ , names(colorado) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

connecticut <- connecticut[ , names(connecticut) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


delaware <- delaware[ , names(delaware) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

florida <- florida[ , names(florida) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

georgia <- georgia[ , names(georgia) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

hawaii <- hawaii[ , names(hawaii) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

idaho <- idaho[ , names(idaho) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

illinois <- illinois[ , names(illinois) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

indiana <- indiana[ , names(indiana) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

iowa <- iowa[ , names(iowa) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

kansas <- kansas[ , names(kansas) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

kentucky <- kentucky[ , names(kentucky) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

louisiana <- louisiana[ , names(louisiana) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

maine <- maine[ , names(maine) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


maryland <- maryland[ , names(maryland) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                            "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                            "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                            "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                            "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                            "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                            "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                            "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                            "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


massachusetts <- massachusetts[ , names(massachusetts) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

michigan <- michigan[ , names(michigan) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


minnesota <- minnesota[ , names(minnesota) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

mississippi <- mississippi[ , names(mississippi) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

missouri <- missouri[ , names(missouri) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

montana <- montana[ , names(montana) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

nebraska <- nebraska[ , names(nebraska) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

nevada <- nevada[ , names(nevada) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

newhampshire <- newhampshire[ , names(newhampshire) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

newjersey <- newjersey[ , names(newjersey) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

newmexico <- newmexico[ , names(newmexico) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

newyork <- newyork[ , names(newyork) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

northcarolina <- northcarolina[ , names(northcarolina) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

northdakota <- northdakota[ , names(northdakota) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

ohio <- ohio[ , names(ohio) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

oklahoma <- oklahoma[ , names(oklahoma) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

oregon <- oregon[ , names(oregon) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

pennsylvania <- pennsylvania[ , names(pennsylvania) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

rhodeisland <- rhodeisland[ , names(rhodeisland) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

southcarolina <- southcarolina[ , names(southcarolina) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

southdakota <- southdakota[ , names(southdakota) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

tennessee <- tennessee[ , names(tennessee) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

texas <- texas[ , names(texas) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

utah <- utah[ , names(utah) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

vermont <- vermont[ , names(vermont) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

virginia <- virginia[ , names(virginia) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

washington <- washington[ , names(washington) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

westvirginia <- westvirginia[ , names(westvirginia) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

wisconsin <- wisconsin[ , names(wisconsin) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]

wyoming <- wyoming[ , names(wyoming) %in% c("year","GEOID", "NAME", "TotalPopE", "WhiteE","BlackE", "AmIndianAlNativeE", "AsianE", "NatHawPacislE", "HispLatinoE", 
                                               "NumPop16overE","Num16overinLFE", "Num16overinLFempE", "Num16overinLFunempE", "TotalHouseholdsE",
                                               "MedHouseholdIncomeE", "MedFamilyIncomeE", "aminalnatincomeE","asianincomeE", "blackincomeE", 
                                               "hispanicincomeE", "nathawincomeE", "whitealoneincomeE", "TotalAmInAlNatHHE", "AmInAlNatOwnerOccE", 
                                               "TotalAsianHHE", "AsianOwnerOccE", "TotalBlackHHE", "BlackOwnerOccE", "TotalHispHHE", "HispOwnerOccE", 
                                               "TotalNatHawHHE", "NatHawOwnerOccE", "TotalWhiteHHE", "WhiteOwnerOccE", "AmInAlNatRenterOccE", "AsianRenterOccE",
                                               "BlackRenterOccE", "WhiteRenterOccE", "NatHawRenterOccE", "PctBelowPovE", "BelowPovTotalpopE",
                                               "Pop25overE", "HSdiplomaE", "GEDE", "Somecollege1E", "Somecollege2E", "AssociatesE", "BachelorsE", "MastersE", 
                                               "ProfSchoolE", "DoctorateE", "TotalHousingUnitsE", "OccupiedUnitsE", "OwneroccUnitsE", "RenteroccUnitsE")]


#-------------------Merge all tables into one-----------------------------------

CensusTracts2020and2015 <- rbind(alabama, alaska, arizona, arkansas, california, colorado, connecticut, delaware,
                                 florida, georgia, hawaii, idaho, illinois, indiana, iowa, kansas, kentucky,
                                 louisiana, maine, maryland, massachusetts, michigan, minnesota, mississippi,
                                 missouri, montana, nebraska, nevada, newhampshire, newjersey, newmexico, newyork,
                                 northcarolina, northdakota, ohio, oklahoma, oregon, pennsylvania, rhodeisland,
                                 southcarolina, southdakota, tennessee, texas, utah, vermont, virginia, washington,
                                 westvirginia, wisconsin, wyoming)


#now free up memory space by removing all of the indivudal tables
rm(arizona, arkansas, california, colorado, connecticut, delaware,
   florida, georgia, hawaii, idaho, illinois, indiana, iowa, kansas, kentucky,
   louisiana, maine, maryland, massachusetts, michigan, minnesota, mississippi,
   missouri, montana, nebraska, nevada, newhampshire, newjersey, newmexico, newyork,
   northcarolina, northdakota, ohio, oklahoma, oregon, pennsylvania, rhodeisland,
   southcarolina, southdakota, tennessee, texas, utah, vermont, virginia, washington,
   westvirginia, wisconsin, wyoming)



#---------------------------Exporting data frame to excel-----------------
#store directory path
my_path <- "C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Output"

#had to export as csv because of file size and/or construction
write.csv(CensusTracts2020and2015, paste0(my_path, "2020ACStracts.csv"))
          


