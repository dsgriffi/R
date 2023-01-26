#Set working directory and assign to the Working Directory object

setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/Stats")
WorkingDirectory <- setwd("C:/Users/DomoniqueGriffin/OneDrive - Fund Consulting/Documents/R/Stats")

#Save workspace
save.image("PracticeStats.RData")

#Save history
savehistory("PracticeStats.Rhistory")

#Load workspace file
load("PracticeStats.RData")
#Load history
loadhistory("PracticeStats.Rhistory")

#Load tidycensus library to interact with Census API
library(tidycensus)

census_api_key("b60c9ca668ed5dfba8f7458af3863bca4ace5705", install = TRUE)

census_api_key(Sys.getenv("CENSUS_API_KEY"))   #RUN THIS EVERY TIME YOU'RE USING TIDYCENSUS

#install needed libraries


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
library(ggplot2)

# ----------------------------- Getting ACS Data ---------------------------
v2019Cityvariables <- get_acs(geography = "place",
                              year = 2019,
                              survey = "acs5",
                              variables = c(TotalPop19 = "B01003_001",
                                            White19 = "B02001_002",
                                            Black19 = "B02001_003",
                                            AmIndianAlNative19 = "B02001_004",
                                            Asian19 = "B02001_005",
                                            NatHawPacisl19 = "B02001_006",
                                            HispLatino19 = "B01001I_001",
                                            NumPop16over19 = "B23001_001",
                                            Num16overinLF19 = "B23025_002",
                                            Num16overinLFemp19 = "B23025_004",
                                            Num16overinLFunemp19 = "B23025_005",
                                            TotalHouseholds19 = "B11012_001",
                                            MedHouseholdIncome19 = "B19013_001",
                                            aminalnatincome19 = "B19013C_001",
                                            asianincome19 = "B19013D_001",
                                            blackincome19 = "B19013B_001",
                                            hispanicincome19 = "B19113I_001",
                                            nathawincome19 = "B19013E_001",
                                            whitealoneincome19 = "B19113H_001",
                                            MedFamilyIncome19 = "B19119_001",
                                            PctBelowPov19 = "B17001_002",
                                            Pop25over19 = "B15003_001",
                                            FemPop25overHSequiv19 = "B15002_028",
                                            FemPop25overBaBs19 = "B15002_032",
                                            MalePop25overHSequiv19 = "B15002_011",
                                            MalePop25overBaBs19 = "B15002_015",
                                            TotalHousingUnits19 = "B25001_001",
                                            OccupiedUnits19 = "B25002_002",
                                            OwneroccUnits19 = "B25003_002",
                                            TotalAmInAlNatHH19 = "B25003C_001",
                                            AmInAlNatOwnerOcc19 = "B25003C_002",
                                            TotalAsianHH19 = "B25003D_001",
                                            AsianOwnerOcc19 = "B25003D_002",
                                            TotalBlackHH19 = "B25003B_001",
                                            BlackOwnerOcc19 = "B25003B_002",
                                            TotalNatHawHH19 = "B25003E_001",
                                            NatHawOwnerOcc19 = "B25003E_002",
                                            TotalWhiteHH19 = "B25003H_001",
                                            WhiteOwnerOcc19 = "B25003H_002",
                                            RenteroccUnits19 = "B25008_003",
                                            AmInAlNatRenterOcc19 = "B25003C_003",
                                            AsianRenterOcc19 = "B25003D_003",
                                            BlackRenterOcc19 = "B25003B_003",
                                            WhiteRenterOcc19 = "B25003H_003",
                                            NatHawRenterOcc19 = "B25003E_003"),
                              output="wide")%>%
  clean_names()


#---------------------Keep/Remove Columns by name---------------------------
#in this method, I'm specifying which columns I want to keep
City2019variables <- v2019Cityvariables[ , names(v2019Cityvariables) %in% c("geoid", "name", "total_pop19e", "white19e",
  "black19e", "am_indian_al_native_19e", "asian19e", "nat_haw_pacisl19e", "hisp_latino19e", 
  "num_pop16over19e","num16overin_lf19e", "num16overin_l_femp19e", "num16overin_l_funemp19e", "total_households19e",
  "med_household_income19e", "med_family_income19e", "aminalnatincome19e","asianincome19e", "blackincome19e", 
  "hispanicincome19e", "nathawincome19e", "whitealoneincome19e")]


#---------------------------------DATA CLEANING,FILTERING, AND CLASSIFICATION------------------------------------
#good video on filtering and mutating data:https://www.youtube.com/watch?v=sUAMiAIUhcI

#Viewing data to get a sense of what you're working with
sapply(City2019variables, class)                           #get classification of all columns
City2019variables %>%                                      #filter down columns to see what data looks like for variables of interest and if there are NA values 
  select(State, total_pop19e, black19e) %>%
  View()

#Examples of filtering that I didn't use in this case
City2019variables %>%                                      #filter down to only rows that have missing data (remove ! if you only want to see complete cases)
  select(State, total_pop19e, black19e) %>%
  filter(!complete.cases(.)) %>%
  View()

City2019variables %>%                                      #filter out all rows with missing data --this is not a good idea
  select(State, total_pop19e, black19e) %>%
  na.omit() %>%
  View()

#BELOW ARE THE FILTERING METHODS THAT I DID USE FOR THIS CASE
#REPLACE MISSING VALUES WITH NA BEFORE RUNNING STATS CALCULATIONS
City2019variables[City2019variables == 0] <- NA

#FILTER OUT ROWS W/ MISSING DATA FROM SPECIFIC VARIABLES/COLUMNS
City2019variables %>%                                      
  drop_na(black19e, total_pop19e, State) %>%
  filter(complete.cases(.)) %>%
  View()

#SPLIT COLUMN BY DELIMITER!
City2019variables <- separate(City2019variables, col = name, into = c("City", "State"), sep = ",")

#TRIM LEADING AND TRAILING SPACES: USE trimws function
City2019variables<-data.frame(lapply(City2019variables, trimws))                    #trim from entire dataframe



#create new dataframe from filter results --this new dataframe exludes cities with missing totalpop and blackpop data
cleanedBlackpop <- City2019variables %>%                  
  select(City, State, total_pop19e, black19e) %>%
  drop_na(City, State, black19e, total_pop19e) %>%
  filter(complete.cases(.))

glimpse(cleanedBlackpop)               #This gives you a preview of your data

#-----------------------------PRACTICING STATISTICS with ACS 2019 VARIABLES----------------------------------
#Here we see how to manipulate data: split column by delimiter,replace 0s with NA,how to get a quick statistical
#summary, how to attach data so you don't have to keep typing the dataframe name

attach(City2019variables)  #this allows you to refer to df without typing it every time
median(med_household_income19e)
hist(log(total_pop19e))           #inserting log properly adjusts the histogram
boxplot(med_household_income19e ~ name)   #~ disaggregates the data by whatever variable you choose
plot(med_household_income_e ~ log(pop25over_e))  #scatter plot to see relationship between 2 numeric variables

#GET QUICK STATISTICAL SUMMARY OF DATAFRAME (min,median,mean,max.etc) of each variable
summary(City2019variables) 
#GET A CALCULATION FOR A SPECIFIC VARIABLE BY USING DOLLAR SIGN
mean(City2019variables$total_pop19e)


#removing v2019Cityvariables from global environment so that stats calculations can work [duplicate variable names were causing it not to work]
rm(v2019Cityvariables)


#Creating a new calculated variable, not including NAs (still figure out how to insert into table)
City2019variables %>%                        
  select(State, total_pop19e, black19e)%>%
  mutate(BlackPer = black19e/total_pop19e, na.rm = TRUE)%>%
  summarize(AverageBlackPer = mean(BlackPer, na.rm= TRUE)*100)


#---------------------------START OF STATS WORK PREP FOR RUNNING T-TEST AND LINEAR REGRESSION-----------------
#checking data types before moving forward with calculations
attach(cleanedBlackpop)

sapply(cleanedBlackpop, class) #checking variable classifications after all of my manipulation. I see that R 
#changed my pop variables to character instead of number so that needs to be changed back before moving on to stats
#calculations

cleanedBlackpop$total_pop19e <- as.numeric(as.character(cleanedBlackpop$total_pop19e))   #convert variable to numeric
cleanedBlackpop$black19e <- as.numeric(as.character(cleanedBlackpop$black19e))
sapply(cleanedBlackpop, class)

#STEP1
#Creating a calculated variable by selecting and mutating data --
cleanedBlackpop <- cleanedBlackpop %>%                       
  select(City, State, total_pop19e, black19e) %>%
  mutate(Blackper = black19e/total_pop19e)

#STEP2  
#filtering and summarizing data by group (state)
cleanedBlackpop %>%                       
  select(City, State, total_pop19e, black19e, Blackper) %>%
  filter(State == "Alabama" | State == "New York") %>%
  group_by(State) %>%
  summarise(AverageBlackPop = mean(Blackper)*100)
#when reporting these stats you would need to be clear that there were a lot of missing city data
#that impacts the data (cities with total pop and black pop values missing were NOT included in these calculations)

#STEP3: testing statistical significance of difference between black pop in different states
        #using a t-test because we're only comparing the means of two independent variables
        #if p < 0.05 then we reject the null hypothesis

df1 <- cleanedBlackpop %>%                       #only using the 2 variables of interest: state and blackper
  select(State, Blackper) %>%
  filter(State == "Alabama" | State == "New York")


t.test(data = df1, Blackper ~ State)

#Examples of Visualizing data








#---------------------------Exporting data frame to excel-----------------
