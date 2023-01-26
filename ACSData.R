#Set working directory and assign to the Working Directory object

setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Data (for non-API source data)")
WorkingDirectory <- setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Data (for non-API source data)")

#Save workspace
save.image("ACSData.RData")

#Save history
savehistory("ACSData.Rhistory")

#Load workspace file
load("ACSData.RData")
#Load history
loadhistory("ACSData.Rhistory")

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
var2019 <- load_variables(2019, "acs5") #search table for variable names

#Resource to reference to find variables or tables you need: https://censusreporter.org/



#I'm getting ACS 5YR County level median household income from table B21004
#because I didn't specify a state,it will give all states plus DC&PR
#I can limit this to a particular state by adding state="stateabbrev"
#after the survey line
#I can add more variables by going back into the code and adding more to the
#list


#------------------------------How to pull data from ONE year at a time----------------
countyincome_2019 <- get_acs(geography = "county",
                             year = 2019,
                             survey = "acs5",
                             variables = c(medianincome = "B19013_001",
                                           blackincome = "B19013B_001",
                                           asianincome = "B19013D_001",
                                           hispanicincome = "B19113I_001",
                                           whitealoneincome = "B19113H_001"),
                             output="wide")%>%
  clean_names()

v2019Variables <- get_acs(geography = "county",
                                       year = 2019,
                                       survey = "acs5",
                                       variables = c(TotalPop = "B01003_001",
                                                     White = "B02001_002",
                                                     Black = "B02001_003",
                                                     AmIndianAlNative = "B02001_004",
                                                     Asian = "B02001_005",
                                                     NatHawPacisl = "B02001_006",
                                                     HispLatino = "B01001I_001",
                                                     NumPop16over = "B23001_001",
                                                     Num16overinLF = "B23025_002",
                                                     Num16overinLFemp = "B23025_004",
                                                     Num16overinLFunemp = "B23025_005",
                                                     TotalHouseholds = "B11012_001",
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
                                                     NatHawRenterOcc = "B25003E_003"),
                                       output="wide")%>%
  clean_names()


v2019StateVariables <- get_acs(geography = "state",
                                year = 2019,
                                survey = "acs5",
                                variables = c(TotalPop = "B01003_001",
                                              White = "B02001_002",
                                              Black = "B02001_003",
                                              AmIndianAlNative = "B02001_004",
                                              Asian = "B02001_005",
                                              NatHawPacisl = "B02001_006",
                                              HispLatino = "B01001I_001",
                                              NumPop16over = "B23001_001",
                                              Num16overinLF = "B23025_002",
                                              Num16overinLFemp = "B23025_004",
                                              Num16overinLFunemp = "B23025_005",
                                              TotalHouseholds = "B11012_001",
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
                                              NatHawRenterOcc = "B25003E_003"),
                                output="wide")%>%
  clean_names()

v2019Cityvariables <- get_acs(geography = "place",
                               year = 2019,
                               survey = "acs5",
                               variables = c(TotalPop = "B01003_001",
                                             White = "B02001_002",
                                             Black = "B02001_003",
                                             AmIndianAlNative = "B02001_004",
                                             Asian = "B02001_005",
                                             NatHawPacisl = "B02001_006",
                                             HispLatino = "B01001I_001",
                                             NumPop16over = "B23001_001",
                                             Num16overinLF = "B23025_002",
                                             Num16overinLFemp = "B23025_004",
                                             Num16overinLFunemp = "B23025_005",
                                             TotalHouseholds = "B11012_001",
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
                                             TotalHousingUnits = "B25001_001",
                                             OccupiedUnits = "B25002_002",
                                             OwneroccUnits = "B25003_002",
                                             TotalAmInAlNatHH = "B25003C_001",
                                             AmInAlNatOwnerOcc = "B25003C_002",
                                             TotalAsianHH = "B25003D_001",
                                             AsianOwnerOcc = "B25003D_002",
                                             TotalBlackHH = "B25003B_001",
                                             BlackOwnerOcc = "B25003B_002",
                                             TotalNatHawHH = "B25003E_001",
                                             NatHawOwnerOcc = "B25003E_002",
                                             TotalWhiteHH = "B25003H_001",
                                             WhiteOwnerOcc = "B25003H_002",
                                             RenteroccUnits = "B25008_003",
                                             AmInAlNatRenterOcc = "B25003C_003",
                                             AsianRenterOcc = "B25003D_003",
                                             BlackRenterOcc = "B25003B_003",
                                             WhiteRenterOcc = "B25003H_003",
                                             NatHawRenterOcc = "B25003E_003"),
                               output="wide")%>%
  clean_names()
#---------------------Keep/Remove Columns by name---------------------------
#in this method, I'm specifying which columns I want to keep
Final2019Variables <- v2019Variables[ , names(v2019Variables) %in% c("geoid", "name", "total_pop_e", "white_e",
        "black_e", "am_indian_al_native_e", "asian_e", "nat_haw_pacisl_e", "hisp_latino_e", 
        "num_pop16over_e","num16overin_lfe", "num16overin_l_femp_e", "num16overin_l_funemp_e", "total_households_e",
        "med_household_income_e", "med_family_income_e", "aminalnatincome_e","asianincome_e", "blackincome_e", 
        "hispanicincome_e", "nathawincome_e", "whitealoneincome_e", "total_am_in_al_nat_hhe", "am_in_al_nat_owner_occ_e", 
        "total_asian_hhe", "asian_owner_occ_e", "total_black_hhe", "black_owner_occ_e", "total_nat_haw_hhe",
        "nat_haw_owner_occ_e", "total_white_hhe", "white_owner_occ_e", "am_in_al_nat_renter_occ_e", "asian_renter_occ_e",
        "black_renter_occ_e", "white_renter_occ_e", "nat_haw_renter_occ_e", "pct_below_pov_e", 
        "pop25over_e", "fem_pop25over_h_sequiv_e", "fem_pop25over_ba_bs_e", "male_pop25over_h_sequiv_e", "male_pop25over_ba_bs_e", 
        "total_housing_units_e", "occupied_units_e", "ownerocc_units_e", "renterocc_units_e")]

State2019Variables <- v2019StateVariables[ , names(v2019Variables) %in% c("geoid", "name", "total_pop_e", "white_e",
        "black_e", "am_indian_al_native_e", "asian_e", "nat_haw_pacisl_e", "hisp_latino_e", 
        "num_pop16over_e","num16overin_lfe", "num16overin_l_femp_e", "num16overin_l_funemp_e", "total_households_e",
        "med_household_income_e", "med_family_income_e", "aminalnatincome_e","asianincome_e", "blackincome_e", 
        "hispanicincome_e", "nathawincome_e", "whitealoneincome_e", "total_am_in_al_nat_hhe", "am_in_al_nat_owner_occ_e", 
        "total_asian_hhe", "asian_owner_occ_e", "total_black_hhe", "black_owner_occ_e", "total_nat_haw_hhe",
        "nat_haw_owner_occ_e", "total_white_hhe", "white_owner_occ_e", "am_in_al_nat_renter_occ_e", "asian_renter_occ_e",
        "black_renter_occ_e", "white_renter_occ_e", "nat_haw_renter_occ_e", "pct_below_pov_e", 
        "pop25over_e", "fem_pop25over_h_sequiv_e", "fem_pop25over_ba_bs_e", "male_pop25over_h_sequiv_e", "male_pop25over_ba_bs_e", 
        "total_housing_units_e", "occupied_units_e", "ownerocc_units_e", "renterocc_units_e")]

City2019variables <- v2019Cityvariables[ , names(v2019Variables) %in% c("geoid", "name", "total_pop_e", "white_e",
        "black_e", "am_indian_al_native_e", "asian_e", "nat_haw_pacisl_e", "hisp_latino_e", 
        "num_pop16over_e","num16overin_lfe", "num16overin_l_femp_e", "num16overin_l_funemp_e", "total_households_e",
        "med_household_income_e", "med_family_income_e", "aminalnatincome_e","asianincome_e", "blackincome_e", 
        "hispanicincome_e", "nathawincome_e", "whitealoneincome_e", "total_am_in_al_nat_hhe", "am_in_al_nat_owner_occ_e", 
        "total_asian_hhe", "asian_owner_occ_e", "total_black_hhe", "black_owner_occ_e", "total_nat_haw_hhe",
        "nat_haw_owner_occ_e", "total_white_hhe", "white_owner_occ_e", "am_in_al_nat_renter_occ_e", "asian_renter_occ_e",
        "black_renter_occ_e", "white_renter_occ_e", "nat_haw_renter_occ_e", "pct_below_pov_e", 
        "pop25over_e", "fem_pop25over_h_sequiv_e", "fem_pop25over_ba_bs_e", "male_pop25over_h_sequiv_e", "male_pop25over_ba_bs_e", 
        "total_housing_units_e", "occupied_units_e", "ownerocc_units_e", "renterocc_units_e")]

#---------------Get multiple data years at the same time---------------------

#I'm using the map_dfr() function from the purrr package to write
#a loop. It will loop through the years that we've told it to pull


#Assigning variables to a list of values

autopopulator_variables <- c(TotalPop = "B01003_001",
                              White = "B02001_002",
                              Black = "B02001_003",
                              AmIndianAlNative = "B02001_004",
                              Asian = "B02001_005",
                              NatHawPacisl = "B02001_006",
                              HispLatino = "B01001I_001",
                              NumPop16over = "B23001_001",
                              Num16overinLF = "B23025_002",
                              Num16overinLFemp = "B23025_004",
                              Num16overinLFunemp = "B23025_005",
                              TotalHouseholds = "B11012_001",
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
years <- lst(2014, 2019)       #note this is the letter l and not the number 1!

#Next is the loop code wrapped around the standard TidyCensus syntax
#Note that we're getting county level from acs 5 year and added clean_names()
#function from janitor package

County2019and2014variables <- map_dfr(
  years,
  ~get_acs(
    geography = "county",
    variables = autopopulator_variables,
    output= "wide",                 #so variables aren't in one column
    year = .x,
    survey = "acs5"
  ),
  .id = "year")

State2019and2014variables <- map_dfr(
  years,
  ~get_acs(
    geography = "state",
    variables = autopopulator_variables,
    output= "wide",                 #so variables aren't in one column
    year = .x,
    survey = "acs5"
  ),
  .id = "year")

City2019and2014variables <- map_dfr(
  years,
  ~get_acs(
    geography = "place",
    variables = autopopulator_variables,
    output= "wide",                 #so variables aren't in one column
    year = .x,
    survey = "acs5"
  ),
  .id = "year")

#Removing unwanted columns--in this method I'm specifying what I want removed

FinalCounty2019and2014vars <- within(County2019and2014variables, rm(TotalPopM, WhiteM, BlackM, AmIndianAlNativeM, "AsianM", "NatHawPacislM", "HispLatinoM", "NumPop16overM",
Num16overinLFM, Num16overinLFempM, Num16overinLFunempM, TotalHouseholdsM, MedHouseholdIncomeM,
aminalnatincomeM, asianincomeM, blackincomeM, hispanicincomeM, nathawincomeM, whitealoneincomeM,
MedFamilyIncomeM, PctBelowPovM, Pop25overM, FemPop25overHSequivM,FemPop25overBaBsM, MalePop25overHSequivM,
MalePop25overBaBsM, HSdiplomaM, GEDM, Somecollege1M, Somecollege2M, AssociatesM, BachelorsM, MastersM, 
ProfSchoolM, DoctorateM, TotalHousingUnitsM, OccupiedUnitsM, OwneroccUnitsM, TotalAmInAlNatHHM,
AmInAlNatOwnerOccM, TotalAsianHHM, AsianOwnerOccM, TotalBlackHHM, BlackOwnerOccM, TotalHispHHM, HispOwnerOccM, TotalNatHawHHM,
NatHawOwnerOccM, TotalWhiteHHM, WhiteOwnerOccM, RenteroccUnitsM, AmInAlNatRenterOccM, AsianRenterOccM,
BlackRenterOccM, WhiteRenterOccM, NatHawRenterOccM))

FinalState2019and2014vars <- within(State2019and2014variables, rm(TotalPopM, WhiteM, BlackM, AmIndianAlNativeM, AsianM, NatHawPacislM, HispLatinoM, NumPop16overM,
Num16overinLFM, Num16overinLFempM, Num16overinLFunempM, TotalHouseholdsM, MedHouseholdIncomeM,
aminalnatincomeM, asianincomeM, blackincomeM, hispanicincomeM, nathawincomeM, whitealoneincomeM,
MedFamilyIncomeM, PctBelowPovM, Pop25overM, FemPop25overHSequivM,FemPop25overBaBsM, MalePop25overHSequivM,
MalePop25overBaBsM, HSdiplomaM, GEDM, Somecollege1M, Somecollege2M, AssociatesM, BachelorsM, MastersM, 
ProfSchoolM, DoctorateM, TotalHousingUnitsM, OccupiedUnitsM, OwneroccUnitsM, TotalAmInAlNatHHM,
AmInAlNatOwnerOccM, TotalAsianHHM, AsianOwnerOccM, TotalBlackHHM, BlackOwnerOccM, TotalHispHHM, HispOwnerOccM, TotalNatHawHHM,
NatHawOwnerOccM, TotalWhiteHHM, WhiteOwnerOccM, RenteroccUnitsM, AmInAlNatRenterOccM, AsianRenterOccM,
BlackRenterOccM, WhiteRenterOccM, NatHawRenterOccM))

FinalCity2019and2014vars <- within(City2019and2014variables, rm(TotalPopM, WhiteM, BlackM, AmIndianAlNativeM, "AsianM", "NatHawPacislM", "HispLatinoM", "NumPop16overM",
Num16overinLFM, Num16overinLFempM, Num16overinLFunempM, TotalHouseholdsM, MedHouseholdIncomeM,
aminalnatincomeM, asianincomeM, blackincomeM, hispanicincomeM, nathawincomeM, whitealoneincomeM,
MedFamilyIncomeM, PctBelowPovM, Pop25overM, FemPop25overHSequivM,FemPop25overBaBsM, MalePop25overHSequivM,
MalePop25overBaBsM, TotalHousingUnitsM,OccupiedUnitsM, OwneroccUnitsM, TotalAmInAlNatHHM,
AmInAlNatOwnerOccM, TotalAsianHHM, AsianOwnerOccM, TotalBlackHHM, BlackOwnerOccM, TotalNatHawHHM,
NatHawOwnerOccM, TotalWhiteHHM, WhiteOwnerOccM, RenteroccUnitsM, AmInAlNatRenterOccM, AsianRenterOccM,
BlackRenterOccM, WhiteRenterOccM, NatHawRenterOccM))

#------------------------------ Creating calculated column
attach(FinalCounty2019and2014vars)
rm(State2019and2014variables)

FinalCounty2019acs <- FinalCounty2019and2014vars %>%
  select(GEOID, year, NAME, TotalPopE, WhiteE, BlackE, AmIndianAlNativeE, AsianE, NatHawPacislE, HispLatinoE, NumPop16overE,
         Num16overinLFE, Num16overinLFempE, Num16overinLFunempE, TotalHouseholdsE, MedHouseholdIncomeE,
         aminalnatincomeE, asianincomeE, blackincomeE, hispanicincomeE, nathawincomeE, whitealoneincomeE,
         MedFamilyIncomeE, PctBelowPovE, Pop25overE, FemPop25overHSequivE, FemPop25overBaBsE, MalePop25overHSequivE,
         MalePop25overBaBsE, HSdiplomaE, GEDE, Somecollege1E, Somecollege2E, AssociatesE, BachelorsE, MastersE, ProfSchoolE, DoctorateE,
         TotalHousingUnitsE, OccupiedUnitsE, OwneroccUnitsE, TotalAmInAlNatHHE,
         AmInAlNatOwnerOccE, TotalAsianHHE, AsianOwnerOccE, TotalBlackHHE, BlackOwnerOccE, TotalHispHHE, HispOwnerOccE, TotalNatHawHHE,
         NatHawOwnerOccE, TotalWhiteHHE, WhiteOwnerOccE, RenteroccUnitsE, AmInAlNatRenterOccE, AsianRenterOccE,
         BlackRenterOccE, WhiteRenterOccE, NatHawRenterOccE) %>%
  group_by(NAME)%>%
  mutate(HSorhigher = sum(HSdiplomaE,GEDE,Somecollege1E,Somecollege2E,AssociatesE,BachelorsE,MastersE,ProfSchoolE,DoctorateE))%>%
  mutate(BaBsorhigher = sum(BachelorsE,MastersE,ProfSchoolE,DoctorateE))

FinalState2019acs <- FinalState2019and2014vars %>%
  select(GEOID, year, NAME, TotalPopE, WhiteE, BlackE, AmIndianAlNativeE, AsianE, NatHawPacislE, HispLatinoE, NumPop16overE,
         Num16overinLFE, Num16overinLFempE, Num16overinLFunempE, TotalHouseholdsE, MedHouseholdIncomeE,
         aminalnatincomeE, asianincomeE, blackincomeE, hispanicincomeE, nathawincomeE, whitealoneincomeE,
         MedFamilyIncomeE, PctBelowPovE, Pop25overE, FemPop25overHSequivE, FemPop25overBaBsE, MalePop25overHSequivE,
         MalePop25overBaBsE, HSdiplomaE, GEDE, Somecollege1E, Somecollege2E, AssociatesE, BachelorsE, MastersE, ProfSchoolE, DoctorateE,
         TotalHousingUnitsE, OccupiedUnitsE, OwneroccUnitsE, TotalAmInAlNatHHE,
         AmInAlNatOwnerOccE, TotalAsianHHE, AsianOwnerOccE, TotalBlackHHE, BlackOwnerOccE, TotalHispHHE, HispOwnerOccE, TotalNatHawHHE,
         NatHawOwnerOccE, TotalWhiteHHE, WhiteOwnerOccE, RenteroccUnitsE, AmInAlNatRenterOccE, AsianRenterOccE,
         BlackRenterOccE, WhiteRenterOccE, NatHawRenterOccE) %>%
  group_by(NAME)%>%
  mutate(HSorhigher = sum(HSdiplomaE,GEDE,Somecollege1E,Somecollege2E,AssociatesE,BachelorsE,MastersE,ProfSchoolE,DoctorateE))%>%
  mutate(BaBsorhigher = sum(BachelorsE,MastersE,ProfSchoolE,DoctorateE))


#---------------------------Exporting data frame to excel-----------------
#store directory path
my_path <- "C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Output"


#write xlsx file
write.xlsx2(v2019and2014autopopulator1, paste0(my_path, "Final2019ACSdata_Jan103.xlsx"), sheetName = "allvariables") 

#option 2 for exporting or appending more than one sheet
data_names <- c("FinalCounty2019and2014vars", "FinalState2019and2014vars")       #vector of data names

write.xlsx2(FinalCounty2019and2014vars, paste0(my_path, "Final2019ACSdata_Feb3.xlsx"), sheetName = "county") 
write.xlsx2(FinalState2019and2014vars, paste0(my_path, "Final2019ACSdata_Feb3.xlsx"), sheetName = "state", append = TRUE)



#Assigning variables to a list of values from hispanic or latino origin by race
my_variables <- c(totalpop = "B03002_001",
                  nh_white ="B03002_003",
                  nh_black ="B03002_004",
                  nh_asian ="B03002_006",
                  hispanic ="B03002_012")
#---------------------Keep/Remove Columns by name---------------------------
income_2019 <- countyincome_2019[ , names(countyincome_2019) %in% c("geoid", "name", 
               "medianincome_e", "blackincome_e", "asianincome_e", "hispanicincome_e",
               "whitealoneincome_e")]
race_2019 <- county_diversity[ , names(county_diversity) %in% c("year", "GEOID", "NAME", "totalpopE", "nh_whiteE",
                                       "nh_blackE", "nh_asianE", "hispanicE")]


#---------------------------Exporting data frame to excel------------------------
#store directory path
my_path <- "C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/ACS/Output"

#write xlsx file, option 1
write.xlsx(Final2019Variables, paste0(my_path, "Final2019ACSdata_Jan11.xlsx"), sheetName = "allvariables") 


#option 1
#write xlsx file
write.xlsx(race_2019, paste0(my_path, "ACSdata.xlsx"), sheetName = "race")
#append to second sheet
write.xlsx(income_2019, paste0(my_path, "ACSdata.xlsx"), 
            sheetName = "income", append = TRUE)


#Option 2
data_names <- c("race_2019", "income_2019")                        #vector of data names
#write excel file and append other data frames
write.xlsx(get(data_names[1]), paste0(my_path, "ACSdata.xlsx"), sheetName = data_names[1])

for(i in 2:length(data_names)) {write.xlsx(get(data_names[i]), paste0(my_path, "ACSdata.xlsx"), sheetName = data_names[i],append = TRUE)}



#Clear workspace
rm(list = ls())


#-----------------------------PRACTICING STATISTICS with ACS 2019 VARIABLES----------------------------------
#Here we see how to manipulate data: split column by delimiter,replace 0s with NA,how to get a quick statistical
#summary, how to attach data so you don't have to keep typing the dataframe name

summary(State2019Variables)          #this gives a statistical summary(min,median,mean,max.etc) of each variable
mean(State2019Variables$total_pop_e) #dollar sign indicates to only give info for specific variable

attach(State2019Variables)  #this allows you to refer to df without typing it everytime
median(med_household_income_e)
hist(log(total_pop_e))           #inserting log properly adjusts the histogram
boxplot(med_household_income_e ~ name)   #~ disaggregates the data by whatever variable you choose
plot(med_household_income_e ~ log(pop25over_e))  #scatter plot to see relationship between 2 numeric variables


#Splitting column by delimiter!
City2019variables2 <- separate(City2019variables, col = name, into = c("City", "State"), sep = ",") 

#Replace missing values with NA
City2019variables2[City2019variables2 == 0] <- NA


#Creating a new calculated variable, not including NAs (still figure out how to insert into table)
City2019variables2 %>%                        
  select(total_pop_e, black_nhe)%>%
  mutate(BlackPer = black_nhe/total_pop_e, na.rm = TRUE)%>%
  summarize(AverageBlackPer = mean(BlackPer, na.rm= TRUE)*100)

#Creating a calculated variable by selecting and filtering data
attach(City2019variables2)
City2019variables2 %>%                       
  select(State, total_pop_e, black_nhe) %>%
  filter(State == "Alabama" | State == "New York") %>%
  group_by(State) %>%
  summarize(Average_BlackPer = mean(black_nhe/total_pop_e))
