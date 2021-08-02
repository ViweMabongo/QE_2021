#Author: Viwe Mabongo
#Date: 02 August 2021
#Task: SDGs Assignment

library(tidyverse)
library(vegan)
library(missMDA) 
library(ggcorrplot)
library(cluster)
library(factoextra)
library(ggpubr)


# Section A ---------------------------------------------------------------


# Question 1 --------------------------------------------------------------

# 1 -----------------------------------------------------------------------
#The Sustainable Development Goals (SDGs) were born at the United Nations Conference on 
#Sustainable Development in Rio de Janeiro in 2012. The objective was to produce a set of 
#universal goals that meet the urgent environmental, 
#political and economic challenges facing our world.

#On this analysis we will focus on SDG 3, which is "Good Health and Well-being" and it was established
# by United Nations in 2015.

SDG1.a <- read_csv("WHO_SDG1.a_domestic_health_expenditure.csv") %>% 
 filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG1.a")
# In this section we are producing a code for SDG1.a which is " Domestic Health Expenditure",
# a filter function is used to retain only the data for the year 2016 that will be used for
#our analysis. Then the select function is used to select and retain variable columns that will be 
#suitable for the analysis, and the the mutate function is used to create another column which will
# be a column defining this sub-SDG which is SDG1a .

# 2 -----------------------------------------------------------------------
 SDG3.1_1 <- read_csv("WHO_SDG3.1_maternal_mort.csv") %>% 
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_1")
# In this section we are producing a code for SDG3.1_1(maternal mortality),
# the filter function is used to retain the only row data for the year 2016,
# then on our indicator we are only retaining data which specifies rate per
# 100000 live births. The select function was used to select and retain variable columns
# that will be suitable for our analysis. The mutate function will be used to create a new column
 #which will be the column defining this sub-SDG which is SDG3.1_2. 

# 3 -----------------------------------------------------------------------
 SDG3.1_2 <- read_csv("WHO_SDG3.1_skilled_births.csv") %>% 
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")
 
 # In thus section we are producing a code for SDG3.1_2 which is  Births attended by skilled
 #health personnel, the filter function was used to only row data for the year 2016, then
 # the select function was used to select and retain variable columns that will be suitable for our
 #analysis, with Indicator representing the health indicator that we are monitoring, then ParentLocation
 # is continent or region, location representing various countries or country names and lastly 
 # FactValueNumber is representing the actual numbers on which we scale the progress of the country 
 # in terms of these SDGs. The mutate function was used to create a new variable column, which will be 
 # the column defining this sub-SDG which is SDG3.1_2.

# 4 -----------------------------------------------------------------------
   SDG3.2_1 <- read_csv("WHO_SDG3.2_neonatal_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1")
   #In this section we are creating a code that will help in monitoring the SDG2.2_1(Number of neonatal deaths),
   # these are the babies that die during first 28 days of life.
   # the filter function is used to only retain row data for the year 2016, also the Dim1 is a column,
   # containing genders, then in this instance we are interested in both genders so the row data,with both
   # sexes was retained. Then select was used to select and retain these variable columns that are 
   #suitable for this analysis. Mutate was used to to create a new variable column 
   #for this sub-SDG which is SDG3.2_1.

# 5 -----------------------------------------------------------------------
   SDG3.2_2  <- read_csv("WHO_SDG3.2_under_5_deaths.csv") %>% 
   filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2")
   #In this section we are creating a code that will help in monitoring the SDG2.2_2(Number of under-five deaths),
   # these are the the babies that die before they reach their 5th birthday. 
   # the filter function is used to only retain row data for the year 2016, also the Dim1 is a column,
   # containing genders, then in this instance we are interested in both genders so the row data,with both
   # sexes was retained. Then select was used to select and retain these variable columns that are 
   #suitable for this analysis. Mutate was used to to create a new variable column 
   #for this sub-SDG which is SDG3.2_2. 
# 6 -----------------------------------------------------------------------
   SDG3.2_3  <- read_csv("WHO_SDG3.2_infant_deaths.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3")
   #In this section we are creating a code that will help in monitoring the SDG2.2_3(Number of infant deaths),
   # these are the the babies that die before they reach their first birthday. 
   # the filter function is used to only retain row data for the year 2016, also the Dim1 is a column,
   # containing genders, then in this instance we are interested in both genders so the row data,with both
   # sexes was retained. Then select was used to select and retain these variable columns that are 
   #suitable for this analysis. Mutate was used to to create a new variable column for this sub-SDG
   #which is SDG3.2_3. 

# 7 -----------------------------------------------------------------------
   SDG3.3_1  <- read_csv("WHO_SDG3.3_new_HIV_infections.csv") %>% 
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1")
   #In this section we are creating a code that will assist in the 
   #monitoring of SDG3.3_1(New HIV infections (per 1000 uninfected population)).
   #This will analyse rate of infection per 1000 people in a given population.
  #The filter function in this instance is used to retain row data for the year 2015,also the Dim1 is a column,
   # containing genders, then in this instance we are interested in both genders so the row data,with both
   # sexes was retained. Then select was used to select and retain these variable columns that are 
   #suitable for this analysis. Mutate was used to to create a new variable column for this 
   #sub-SDG which is SDG3.3_1.
   
# 8 -----------------------------------------------------------------------
   SDG3.3_2   <- read_csv("WHO_SDG3.3_TB.csv") %>% 
   filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2")
   #In this section we are creating a code that will assist in the 
   #monitoring of SDG3.3_2(Incidence of tuberculosis (per 100 000 population per year)).
   #This will analyse the incidences of TB infection per 100000 people in a given population.
   #The filter function in this instance is used to retain row data for the year 2016,Then the
   #select was used to select and retain these variable columns that are 
   #suitable for this analysis. Mutate was used to create a new variable column for 
   #this sub-SDG which is SDG3.3_2.

# 9 -----------------------------------------------------------------------
   SDG3.3_3   <- read_csv("WHO_SDG3.3_malaria.csv") %>% 
   filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3")
   #In this section we are creating a code that will assist in the 
   #monitoring of SDG3.3_3(Malaria incidence (per 1 000 population at risk)).
   #This will analyse incidences of malaria per 1000 people at risk in a given population.
   #The filter function in this instance is used to retain row data for the year 2016,Then the
   #select was used to select and retain these variable columns that are 
   #suitable for this analysis. Mutate was used to create a new variable column for 
   # this sub-SDG which is SDG3.3_3.
   
# 10 ----------------------------------------------------------------------
   SDG3.3_4  <- read_csv("WHO_SDG3.3_hepatitis_B.csv") %>% 
   filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4")
   
# In this section we are creating a code that will assist in monitoring of SDG3.3_4, 
# using the indicator Hepatitis B surface antigen (HBsAg) prevalence among children under 5 years.
# The filter function was used to retain row data for the year 2015, then the select function
# select was used to select and retain these variable columns that are suitable for this analysis.
# Mutate was used to create a new column for this sub-SDG which is SDG3.3_4.


# 11 ----------------------------------------------------------------------
SDG3.3_5 <- read.csv("WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5")
# In this section we are creating a code that will assist in monitoring of SDG3.3_5,
#using the indicator reported number of people requiring interventions against NTDs.
# This will analyse people that are requiring interventions against NTDs, then the filter
#function is used to retain row data for the year 2016, then the select function is used 
# to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.3_5



# 12 ----------------------------------------------------------------------
SDG3.4_1 <- read_csv("WHO_SDG3.4_adult_death_prob.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1")
# In this section we are creating a code that will assist in monitoring of SDG3.4_1,
#using the indicator reported number of people requiring interventions against NTDs.
# This will analyse adult mortality rate.i.e probability of people dying between15 and 60 years
#per 1000 population, then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained.
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.4_1.

# 13 ----------------------------------------------------------------------
SDG3.4_2 <- read_csv("WHO_SDG3.4_NCD_by_cause.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Diabetes mellitus") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2")
# In this section we are creating a code that will assist in monitoring of SDG3.4_2,
#using the indicator number of deaths attributed to non-communicable diseases.
# This will analyse number of deaths attributed to non-communicable diseases.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. Dim2 is used to specify which non- communicable
# disease are we interested to analyse which is Diabetes melltus in this case. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.4_2.

SDG3.4_3<- read_csv("WHO_SDG3.4_NCD_by_cause.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Cardiovascular diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3")
# In this section we are creating a code that will assist in monitoring of SDG3.4_3,
#using the indicator number of deaths attributed to non-communicable diseases.
# This will analyse number of deaths attributed to non-communicable diseases.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. Dim2 is used to specify which non- communicable
# disease are we interested to analyse which is Cardiovascular diseases in this case. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.4_3.

SDG3.4_4 <- read_csv("WHO_SDG3.4_NCD_by_cause.csv") %>% 
 filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")
# In this section we are creating a code that will assist in monitoring of SDG3.4_4,
#using the indicator number of deaths attributed to non-communicable diseases.
# This will analyse number of deaths attributed to non-communicable diseases.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. Dim2 is used to specify which non- communicable
# disease are we interested to analyse which is Respiratory diseases in this case. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.4_4.


# 14 ----------------------------------------------------------------------
SDG3.4_5 <- read_csv("WHO_SDG3.4_suicides.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")
# In this section we are creating a code that will assist in monitoring of SDG3.4_5,
#using the indicator Crude suicide rates (per 100 000 population).
# This will analyse the rate of crude suicide per 100 000 population.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.4_5.


# 15 ----------------------------------------------------------------------
SDG3.4_6 <- read_csv("WHO_SDG3.4_NCD_data_total.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")
# In this section we are creating a code that will assist in monitoring of SDG3.4_6,
#using the indicator Total NCD Deaths (in thousands).
# This will analyse the total number of deaths of non-communicable diseases in thousands.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.4_6.


# 16 ----------------------------------------------------------------------
SDG3.5 <- read_csv("WHO_SDG3.5_alcohol_consumption.csv") %>% 
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")
# In this section we are creating a code that will assist in monitoring of SDG3.5,
#using the indicator Alcohol, total per capita (15+) consumption (in litres of pure alcohol).
# This will analyse the total consumption of alcohol per capita from age 15 upwards.
#then the filter function is used to retain row data for the year 2015,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.5.



# 17 ----------------------------------------------------------------------
SDG3.6 <- read_csv("WHO_SDG3.6_traffic_deaths_prop.csv") %>% 
   filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")
# In this section we are creating a code that will assist in monitoring of SDG3.6 ,
#using the indicator Estimated road traffic death rate (per 100 000 population)) .
# This will analyse the death rate of road traffic per 100 000 population.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.6.


# 18 ----------------------------------------------------------------------
SDG3.7 <- read_csv("WHO_SDG3.7_adolescent_births.csv") %>% 
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")
# In this section we are creating a code that will assist in monitoring of SDG3.7 ,
#using the indicator Adolescent birth rate (per 1000 women aged 15-19 years)).
# This will analyse the birth rate of women between ages 15 -19 per 1000 women.
#then the filter function is used to retain row data for the year 2016,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.7.



# 19 ----------------------------------------------------------------------
SDG3.8_1 <- read_csv("WHO_SDG3.8_UHC_data_availability.csv") %>% 
  filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")
# In this section we are creating a code that will assist in monitoring of SDG3.8_1 ,
#using the indicator UHC Index of service coverage (SCI).
# This will analyse the Coverage index for essential health services 
#(based on tracer interventions that include reproductive, maternal,
#newborn and child health, infectious diseases, noncommunicable diseases and
#service capacity and access). It is presented on a scale of 0 to 100.
#then the filter function is used to retain row data for the years from 2013 to 2017,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.8_1.


# 20 ----------------------------------------------------------------------
SDG3.8_2 <- read_csv("WHO_SDG3.8_UHC_index_of_service_coverage.csv") %>% 
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")
# In this section we are creating a code that will assist in monitoring of SDG3.8_2,
#using the indicator Data availability for UHC index of essential service coverage (%)) .
# This will analyse(based on data availability) the Coverage index for essential health services 
#(based on tracer interventions that include reproductive, maternal,
#newborn and child health, infectious diseases, noncommunicable diseases and
#service capacity and access). It is presented on a scale of 0 to 100.
#then the filter function is used to retain available row data for the year 2017,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.8_2.


# 21 ----------------------------------------------------------------------
SDG3.9_1 <- read_csv("WHO_SDG3.9_unintentional_poisoning_prop.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")
# In this section we are creating a code that will assist in monitoring of SDG3.9_1,
#using the indicator  Poison control and unintentional poisoning.
# This will analyse the unintentional poisoning among the population,
# this may occur a person taking or giving too much of a substance did not mean to cause harm, like medication.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.9_1.




# 22 ----------------------------------------------------------------------
SDG3.9_3 <- read_csv("WHO_SDG3.9_WASH_mortalities.csv") %>% 
   filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")
# In this section we are creating a code that will assist in monitoring of SDG3.9_3,
#using the indicator Mortality rate attributed to exposure to unsafe WASH services (per 100 000 population).
# This will analyse the Mortality rate attributed to unsafe water,
#unsafe sanitation and lack of hygiene (per 100000 population).
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3.9_3.


# 23 ----------------------------------------------------------------------
SDG16.1 <- read_csv("WHO_SDG16.1_homicides.csv") %>% 
   filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")
# In this section we are creating a code that will assist in monitoring of SDG16.1,
#using the indicator  Estimates of rate of homicides (per 100 000 population).
# This will analyse the rate of homicides per 100 000 people and South Africa as of 2021,
# it is standing at 33.97 per 100k people.
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG16.1.


# 24 ----------------------------------------------------------------------
   SDG3.a  <- read_csv("WHO_SDG3.a_tobacco_control.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")
# In this section we are creating a code that will assist in monitoring of SDG3a,
#using the indicator  Prevalence of current tobacco use among persons aged 15 years and
#older (age-standardized rate).
# This will analyse the  Age-standardized prevalence of current tobacco
#use among persons aged 15 years and older(%).
#then the filter function is used to retain row data for the year 2016,
#Dim1 function contains genders(male, female and both sexes), so in this instance we are interested on both
# genders, row data with both sexes was then retained. 
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3a.


# 25 ----------------------------------------------------------------------
SDG3.b_1 <- read_csv("WHO_SDG3.b_dev_assistence_for_med_research.csv") %>% 
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")
# In this section we are creating a code that will assist in monitoring of SDG3.b_1,
#using the indicator development assistance for medical research .
# This will analyse the Total net official development assistance to medical research and
#basic health sectors per capita (US$), by recipient country.
#then the filter function is used to retain row data for the year 2016,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3b_1.


# 26 ----------------------------------------------------------------------
SDG3.b_2 <- read_csv("WHO_SDG3.b_measles_vaccine.csv") %>% 
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")
# In this section we are creating a code that will assist in monitoring of SDG3.b_2,
#using the indicator  measles vaccine immunization coverage by the nationally recommended age.
# This will analyse the Measles-containing-vaccine second-dose (MCV2) immunization coverage
#by the nationally recommended age (%).
#then the filter function is used to retain row data for the year 2016,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3b_2.

# 27 ----------------------------------------------------------------------
SDG3.b_3 <- read_csv("WHO_SDG3.b_diphtheria_vaccine.csv") %>% 
   filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")
# In this section we are creating a code that will assist in monitoring of SDG3.b_3,
#using the indicator diphtheria vaccine immunization coverage among 1 year olds.
# This will analyse the  Diphtheria tetanus toxoid and pertussis (DTP3) immunization
#coverage among 1-year-olds (%).
#then the filter function is used to retain row data for the year 2016,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3b_3.


# 28 ----------------------------------------------------------------------
SDG3.b_4 <- read_csv("WHO_SDG3.b_pneumococcal_vaccine.csv") %>% 
   filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")
# In this section we are creating a code that will assist in monitoring of SDG3.b_4,
#using the indicator Pneumococcal conjugate vaccines immunization-coverage-among-1-year-olds.
# This will analyse the Pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%).
#then the filter function is used to retain row data for the year 2016,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3b_4.



# 29 ----------------------------------------------------------------------
SDG3.b_5 <- read_csv("WHO_SDG3.b_HPV_vaccine.csv") %>% 
   filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")
# In this section we are creating a code that will assist in monitoring of SDG3.b_5,
#using the indicator Girls aged 15 years old that received the recommended doses of HPV vaccine.
# This will analyse the number of  girls aged 15 years old that received the recommended doses
#of HPV vaccine .
#then the filter function is used to retain row data for the year 2016,
#then the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDG3b_5.


# 30 ----------------------------------------------------------------------
SDG3.c_1 <- read_csv("WHO_SDG3.c_health_workforce.csv") %>% 
   filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")
# In this section we are creating a code that will assist in tracking the heath work force,
# per preferred population size, in this instance the medical doctors per 1000 people in a 
# given population, then the filter function was used to retain row data for the year 2016,
# also the medical doctors per 10000 indicator was used to specify the number against a 
# preferred number in a given population, select function was used to select and retain variable
# columns that are relevant and suitable for the analysis, and mutate was used to create a new column for 
# this su-SDG.

SDG3.c_2 <- read_csv("WHO_SDG3.c_health_workforce.csv") %>% 
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")
# In this section we are creating a code that will assist in tracking the heath work force,
# per preferred population size, in this instance the nursing and midwifery personnel per 1000 people in a 
# given population, then the filter function was used to retain row data for the year 2016,
# also the nursing and midwifery personnel per 10000 indicator was used to specify the number against a 
# preferred number in a given population, select function was used to select and retain variable
# columns that are relevant and suitable for the analysis, and mutate was used to create a new column for 
# this sub-SDG.

SDG3.c_3 <- read_csv("WHO_SDG3.c_health_workforce.csv") %>% 
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")
# In this section we are creating a code that will assist in tracking the heath work force,
# per preferred population size, in this instance the dentists per 1000 people in a 
# given population, then the filter function was used to retain row data for the year 2016,
# also the dentists per 10000 indicator was used to specify the number against a 
# preferred number in a given population, select function was used to select and retain variable
# columns that are relevant and suitable for the analysis, and mutate was used to create a new column for 
# this sub-SDG.

SDG3.c_4 <- read_csv("WHO_SDG3.c_health_workforce.csv") %>% 
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")
# In this section we are creating a code that will assist in tracking the heath work force,
# per preferred population size, in this instance the pharmacists per 1000 people in a 
# given population, then the filter function was used to retain row data for the year 2016,
# also the pharmacists per 10000 indicator was used to specify the number against a 
# preferred number in a given population, select function was used to select and retain variable
# columns that are relevant and suitable for the analysis, and mutate was used to create a new column for 
# this sub-SDG.


# 32 ----------------------------------------------------------------------
SDG3.d_1 <- read_csv("WHO_SDG3.d_health_risks.csv") %>% 
   filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")
#In thus section we are creating a code that will assist in tracking the progress in terms of
# implementing the Average of 13 International Health Regulations core capacity scores, graded to 100%.
# Filter function was used to retain row data for the year 2016, then select function was used to select and 
# retain variable columns that are relevant and suitable for the analysis,and mutate was used to create a new column for 
# this sub-SDG.


# 33 ----------------------------------------------------------------------
other_1 <- read.csv("WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1") 
# In this section we are creating a code that will assist in monitoring of SDGother_1,
#using the indicator life expectancy at birth.
# This will analyse the average number of years that a newborn could expect to live if he or
#she were to pass through life subject to the age-specific mortality rates of a given period.
# filter function is used to retain row data for the year 2015,Dim1 column was used to retain row dat 
# both sexes, then Indicator was used to specify to retain only row data " life expectancy at birth"then
#the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDGother_1.


# 34 ----------------------------------------------------------------------
other_2 <- read.csv("WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2")
## In this section we are creating a code that will assist in monitoring of SDGother_2,
#using the indicator life expectancy age 60 years.
#This will analyze the overall mortality level of a population over 60 years. 
#It summarizes the mortality pattern that prevails across all age groups above 60 years.
# filter function is used to retain row data for the year 2015,Dim1 column was used to retain row data 
# both sexes, then Indicator was used to specify to retain only row data " life expectancy at age 60"then
#the select function is used to select and retain these variable columns that are suitable for this analysis.
#Mutate function is used to create a new column defining this sub-SDG which is SDGother_2.



# 36 ----------------------------------------------------------------------
health <- do.call("rbind", lapply(ls(),get))
head(health)
#The rbind function is used to combine several vectors, matrices and/or data frames by rows.


# 37 ----------------------------------------------------------------------
unique(health[, c(5, 1)])
#The unique function is used to eliminate or delete the duplicate values or 
#the rows present in the vector, data frame, or matrix.

# 38 ----------------------------------------------------------------------
health_wide <- health %>%
  arrange(Location) %>%
  select(-Indicator) %>%
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>%
  as_tibble()
# Arrange is used to order the rows of the data frame by the values of selected columns.
# Select is used to choose the Indicator variable and remove it from the dataset.
#pivot_wider function is used to widen data, increasing the number of columns and 
#decreasing the number of rows.


# 39 ----------------------------------------------------------------------
popl <- read_csv("WHO_population.csv") %>%
  filter(Year == 2016) %>%
  rename(popl_size = `Population (in thousands) total`,
         Location = Country) %>%
  select(Location, popl_size) %>%
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000)

health_wide <- health_wide %>%
  left_join(popl)
#A left join is used to merge operation between heath_wide and population where the merge returns
#all of the rows from one table (the left side) and any matching rows from the second table.


# 40 ----------------------------------------------------------------------
health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)
# In this section we are creating new columns of these stated SDGs.


# 41 ----------------------------------------------------------------------
# calculate histograms
health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, function(x) sum(is.na(x)))
hist(health_wide$na_count, breaks = 14, plot = TRUE)
#


# 42 ----------------------------------------------------------------------
# remove rows where there are more than 10 NAs
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)
# In this section we are removing raws that contain more than 10 missing values because so,
#much missing values can give rise to unrepresentative analysis.


# 43 ----------------------------------------------------------------------
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)
#In this section we are doing a correlation analysis, to check which SDGs are closely reated.

# visualization of the correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "grey60",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE)
#Visualization of a correlation matrix using ggplot2

# 44 ----------------------------------------------------------------------
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs
# In this section we are using imputePCA to impute the missing values of a dataset 
#with the Principal Components Analysis model.
# This is a preliminary step before performing a PCA on an completed dataset.
# 45 ----------------------------------------------------------------------
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize")
health_pca <- rda(health_wide_complete_std)
health_pca
# summary(health_pca)
# In this section we are standardising our data variables because they are measured on different scales.

# 46 ----------------------------------------------------------------------
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))
# In this section we are creating PCA ordination diagrams for better visualization,
#the scaling 1 biplot will show us the relationship among the SDGs, whereas scaling 2 biplot
# will show us a relationship among locations.

# 47 ----------------------------------------------------------------------

pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG")
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
# text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

# This section is for ordiplot visualization for site scores.

# 48 ----------------------------------------------------------------------
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location)
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)

ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = ParentLocation)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")

# In this section we are creating a visualization diagram fer the complete PCA analysis.



# Question 2 --------------------------------------------------------------
SDG_complete <- read_csv("SDG_complete.csv")
SDG_description <- read_csv("SDG_description.csv")


Selected_Scores <- site_scores %>% 
filter(Location %in% c("US", "Italy", "China", "France", "United Kingdom", "Germany",
                       "South Africa", "Singapore", "Seychelles", "Burundi","Cape Verde"))# Selected 
#countries for comparison to South Africa. Although US and Cape Verde are not appearing on the selected 
#dataset.


ggplot(data = Selected_Scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = Location)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")



SDG_complete <- read_csv("SDG_complete.csv")
Select_SDG <- SDG_complete %>% 
  filter(Location %in% c("US", "Italy", "China", "France", "United Kingdom", "Germany",
                         "South Africa", "Singapore", "Seychelles", "Burundi","Cape Verde"))# Filtered
# so that I can be able to visualize a complete list of these selected countries.


#I will discuss in this section using some of the SDGs to illustrate how South Africa is doing
# in terms of attaining these SDGs comparing to the above selected countries. 
#When we look at SDGother_1(Life expectancy at birth (years)), South Africa is not doing great 
# in terms of this SDG as it s standing at 62.57 years average whereas countries that are in 
# Europe and Western Pacific are doing fairly good with averages over 80 years with the exception 
# of China that is standing at 76.64 average years. In the African region Sychelles is doing great
# compared to South Africa standing at 73.20 average, with Burundi having a close average with
# South Africa standing at 62.05 years. This can be attributed to the fact that countries in Europe
# and Western Pacific have better nutrition, heath care and food security compared to South Africa(SA).
#Seychelles is doing great than SA because it have less population and people of that country benefit
#because it is a world tourism destination, so its systems are made to meet world standards.

# On SDGother_2(Life expectancy at age 60 (years)), South Africa is also not performing great
# in terms of this SDG compared to its counterparts in Europe and Western Pacific with SA's 
# average age standing at 17.68, whereas in Europe and Western Pacific they have averages 
# standing above 20 years, with Sychelles in the region also doing better than SA at average
# of 1.53, with only Burundi below at 16.18.

# In terms of SDG1.a(Domestic general government health expenditure (GGHE-D) as 
#percentage of general government expenditure (GGE) (%)), SA is quite doing great in this
# SDG with only Germany that has significant high expenditure standing at 19.59 and SA is
# standing at 13.34, with China, Sychelles and Burundi spending less than 10%.

# In terms of SDG16.1(Estimates of rates of homicides per 100 000 population), SA is performing
# very bad compared to its counterparts with the rate of homicide standing at 38.59, whereas
# the European and Western Pacific counterparts have homicide rate less than 1. Then in the
#African region Burundi and Sychelles are standing at 6.37 and 13.78. This can be attributed 
# to the fact that South Africa have high rate of crime that can lead to these homicides.

#In terms of SDG3.1_1(Maternal mortality ratio (per 100 000 live births)), SA is performing bad
# in terms of this SDG with the maternal mortality ratio of SA standing at 122 people per 100 000
# live births with only Burundi that is worst performing than SA standing at 558 people per 100 000.

#SDG3.2_1(Number of neonatal deaths), SA is also performing bad in terms of this SDG with the 
# number of neonatal deaths at 23.98, Burundi being the only country that is worst performing
#than SA standing at a number of 90.935 neonatal deaths.

#SDG3.2_2(Number of under-five deaths), SA is performing  bad in terms of this SDG with
# the number of under five deaths standing at 77.190, with only Burundi that is worst performing
#than SA standing at a high number of 20500.247 under five deaths.

#SDG3.2_3(Number of infant deaths), SA is also doing bad in terms of this SDG than its counterparts
#with the number of infant deaths standing at 60.149, with Burundi being the only country that is worst
# performing than SA with numbers standing at 175,038.

#SDG3.3_1(New HIV infections (per 1000 uninfected population)), SA is performing bad in terms of this
#SDG with the rate of new HIV infection of 5.940 people per 1000 uninfected population, and it is the highest
#among its selected counterparts, and it is higher than the African counterparts.

#SDG3.3_2(Incidence of tuberculosis (per 100 000 population per year)), SA is performing very bad 
#in terms of this SDG with the incidence per 100 000 population standing at 805.0, with all its 
# counterparts standing at less than 100 people, except for Burundi that is standing at 118.0.

#SDG3.3_3(Malaria incidence (per 1 000 population at risk)), SA is not performing good, compared to
# the European and Western Pacific with incidence number at 0.770 but it is doing fairly good compared
# to the African counterparts with Sychelles and Burundi standing at 4.735 and 229.6 respectively.

#SDG3.9_3(Mortality rate attributed to exposure to unsafe WASH services (per 100 000 population)), SA is
# is performing bad in terms of this SDG with mortality rate standing at 13.670, whereas its counterparts
# are standing at below 1, with only Burundi that is standing 65.400. This means SA have poor Water and 
#Sanitation approach or planing.

#SDG3.b_1(Total net official development assistance to medical research and basic health sectors per capita (US$),
#by recipient country), SA is doing quite good in terms of this SDG with the assistance when compared to
# the European and Western Pacific standing at $1.550, with the African counterparts having better numbers
# than SA and the rest of other countries, with Burundi and Sychelles standing at $34.170 and $9.570
#respectively.

#SDG3.C_1(Medical doctors (per 10,000)), SA is not performing well compared to its counterparts,
#with medical doctors per 1000 people standing at a low number of 7.68 with Italy having a high
#number of 79 doctors per 1000 people, with Sychelles doing better at 21 doctors per 1000 people,
# then Burundi is doing very bad at -1.106.
 
#Based on the above used SDGs, I can conclude that South Africa is not doing good in terms of attaining
# these Sustainable Development Goals, although it have a high spending on the Health sector the overall
# results are inversely proportion to this spending this may be due to the fact that most people live 
# under the poverty line in South Africa and there is a lot of squatter camps or informal settlements.
# On the issue of HIV it may be due to the fact that most people are irresponsible when it comes to 
# having unprotected sex.But it seems this is not a South African as
#other SADC countries such a Lesotho, ESwatini, Botswana also have high rate of new HIV infections.


# Question 3 --------------------------------------------------------------

# The African region countries are the worst performing in terms of these SDG, and that may be
# due to the factor Africa generally is a developing continent so it will take time to implement
# them because of lack of resources in terms of capital and also trained human resource. Africa is faced 
# with a high unemployment, poverty and unemployment rates so the well being of people in general is poor.
#Seychelles although in Africa it has quite good performing economy with GDP standing at 17,448.27 USD
# as of 2019.
#On the other hand the European and Western Pacific region have the vast availability resources and
#also they have highly skilled human resource. So although these SDGs were introduced in 2012 they are
#able to implement these SDGs because their health care system is already intact.
# Many developing nations lack adequate health care and family planning, 
#and pregnant women have minimal access to skilled labor and emergency care.



# Section B ---------------------------------------------------------------
#load data
SDGs <- SDG_complete

unique(SDGs$ParentLocation)

length(SDGs$Location)

corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white", 
           colors = c("navy", "white", "#FC4E07"), 
           lab = TRUE)
SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize")
# SDGs_euc <- vegdist(SDGs_std, method = "euclidian")
rownames(SDGs_std) <- SDGs$Location # carry location names into output

# using silhouette analysis
plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()

# total within cluster sum of square / elbow analysis
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()

# gap statistics
plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()

ggarrange(plt1, plt2, plt3, nrow = 3)

SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)# 3 clusters

SDGs_pam1 <- pam(SDGs_std, metric = "euclidean", k = 4)# 4 clusters

SDGs_pam2 <- pam(SDGs_std, metric = "euclidean", k = 5)# 5 clusters

SDGs_pam4 <- pam(SDGs_std, metric = "euclidean", k = 6)# 6 clusters

# This is used to visualize 5 clusters.
fviz_cluster(SDGs_pam2, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3","#660099","#FF9900","#00CCCC"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)


# 1 -----------------------------------------------------------------------

#1 When we use pam() to create  4, 5 or 6 clusters they give us a more detailed grouping of these
# countries in terms of implementing these SDGs, they are more specific in grouping countries with more
# similarities and give a best segmentation possible. 

# 2 -----------------------------------------------------------------------
#2 Five clusters can be appropriate for our analysis as it can break down even small dissimilarities/similarities among
# the locations and give sufficient best possible data for great visualization and analysis.


# 3 -----------------------------------------------------------------------

#3 Analysis using the hclust() technique.

sdg.dist = dist(SDG_complete)


sdg.hclust = hclust(sdg.dist )

plot(sdg.hclust,main='Default from hclust')

# Compute hierarchical clustering and cut into 5 clusters
res <- hcut(sdg.dist, k = 5, stand = TRUE)

# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07","#FF9900"))


#Yes, they are marked differently, I will proceed with the pam() clustering. They are not easy
# to visualize and analyze.


# 4 -----------------------------------------------------------------------


#4 I will use pam() clustering and the number of clusters I am going to use is 5.
# In cluster 1 it is developing countries that are predominantly from Africa, these countries
# have bad performing economies, with low GDP per capita, most of these countries have GDP per capita 
#less than $1000.In most of these countries the citizens are
# living below the poverty line. These countries are performing poor in terms of attaining these 
#SDGs.

#Countries in cluster 2 are countries with middle income economies, they have fairly good GDPs per capita
# and the citizens of these countries are affording basic commodities and basic health care requirements.
# These countries are doing quite fairly in terms of attaining these SDGs.

#Countries in cluster 3 have economic status that is good(affluent)than those in cluster 2,
# and the GDP per capita in these countries is quite good compared to the cluster 1 and cluster 2 countries,
# The citizens of these countries are affording in terms of health care and basic commodities. These 
# countries are performing good in attaining these SDGs.

#Countries in cluster 4 are extremely rich countries, some of them are in the G7, some countries in this
# cluster have GDP per capita that is more than $60000(countries such as Norway). These countries are
# doing great in terms of these SGDs.

# Countries in cluster 5 are a mixture of low income countries and middle income countries, these countries
# have GDP per capita that is more than $1000 but less than $10000. In these countries they is a big gap 
# between the rich and the poor. There are citizens that live below the poverty line, whereas there are
# those that are extremely wealthy and rich. These countries are not performing good in terms of these SDGs 
# although there are some positives.


# 5 -----------------------------------------------------------------------


#5 This can be attributed to the fact that South Africa is an economic powerhouse of the African region,
# and its GDP is quite high compared to its African counterparts and also South Africa is doing quite 
# better in terms of attaining these SDGs than its ParentLocation counterparts.

























