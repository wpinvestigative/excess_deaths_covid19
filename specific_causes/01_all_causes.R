
# loading packages
library(tidyverse)
library(RSocrata)
library(hablar)
library(geofacet)
library(lubridate)
library(janitor)

week_max <- 30

weeks_df <- read_csv("specific_causes/weeks.csv")
weeks_df <- weeks_df %>% 
  mutate(start_date=dmy(start_date)-1,
         end_date=dmy(end_date)-1) 


# importing cdc cause specific death data for 2014-2018, grabbing columns for cause, omitting flag columns. Full list here: https://dev.socrata.com/foundry/data.cdc.gov/3yf8-kanr
causes_1418 <- read_csv(
  "https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD") %>% 
  data.frame() %>% 
  clean_names() %>% 
  select(jurisdiction_of_occurrence,
         mmwryear=mmwr_year, mmwrweek=mmwr_week, weekendingdate=week_ending_date,
         allcause=all_cause,naturalcause=natural_cause,
         malignant_neoplasms_c00_c97, alzheimer_disease_g30,
         chronic_lower_respiratory=chronic_lower_respiratory_diseases_j40_j47 , symptoms_signs_and_abnormal=symptoms_signs_and_abnormal_clinical_and_laboratory_findings_not_elsewhere_classified_r00_r99,
         diseases_of_heart_i00_i09=diseases_of_heart_i00_i09_i11_i13_i20_i51, cerebrovascular_diseases=cerebrovascular_diseases_i60_i69 ,
         septicemia_a40_a41, diabetes_mellitus_e10_e14,
         influenza_and_pneumonia_j10=influenza_and_pneumonia_j10_j18,
         other_diseases_of_respiratory=other_diseases_of_respiratory_system_j00_j06_j30_j39_j67_j70_j98,
         nephritis_nephrotic_syndrome=nephritis_nephrotic_syndrome_and_nephrosis_n00_n07_n17_n19_n25_n27) %>% 
  mutate(weekendingdate=mdy(weekendingdate),
         covid19_multiple_causes_of_death=NA,
         covid19_underlying_cause_of_death=NA,
         mmwryear=as.numeric(mmwryear),
         mmwrweek=as.numeric(mmwrweek))

# importing cdc cause specific death data for 2019-2020
#causes_1920 <- read.socrata(
#  "https://data.cdc.gov/resource/muzy-jte6.json?$select=jurisdiction_of_occurrence,mmwryear,mmwrweek,weekendingdate,allcause,naturalcause,septicemia_a40_a41,malignant_neoplasms_c00_c97,diabetes_mellitus_e10_e14,alzheimer_disease_g30,influenza_and_pneumonia_j10,chronic_lower_respiratory,other_diseases_of_respiratory,nephritis_nephrotic_syndrome,symptoms_signs_and_abnormal,diseases_of_heart_i00_i09,cerebrovascular_diseases"
#)

#cause_names <- colnames(causes_1418)
library(stringr)
library(lubridate)
library(jsonlite)
library(janitor)

causes_1920 <- read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD") %>% 
  data.frame() %>% 
  clean_names() %>% 
  select(jurisdiction_of_occurrence,
         mmwryear=mmwr_year, mmwrweek=mmwr_week, weekendingdate=week_ending_date,
         allcause=all_cause,naturalcause=natural_cause,
         malignant_neoplasms_c00_c97, alzheimer_disease_g30,
         chronic_lower_respiratory=chronic_lower_respiratory_diseases_j40_j47 , symptoms_signs_and_abnormal=symptoms_signs_and_abnormal_clinical_and_laboratory_findings_not_elsewhere_classified_r00_r99,
         diseases_of_heart_i00_i09=diseases_of_heart_i00_i09_i11_i13_i20_i51, cerebrovascular_diseases=cerebrovascular_diseases_i60_i69 ,
         septicemia_a40_a41, diabetes_mellitus_e10_e14,
         influenza_and_pneumonia_j10=influenza_and_pneumonia_j09_j18,
         other_diseases_of_respiratory=other_diseases_of_respiratory_system_j00_j06_j30_j39_j67_j70_j98,
         nephritis_nephrotic_syndrome=nephritis_nephrotic_syndrome_and_nephrosis_n00_n07_n17_n19_n25_n27,
         covid19_multiple_causes_of_death=covid_19_u071_multiple_cause_of_death,
         covid19_underlying_cause_of_death=covid_19_u071_underlying_cause_of_death) %>% 
  mutate(weekendingdate=mdy(weekendingdate))

# append the 19-20 data to the older data
causes_1420 <- bind_rows(causes_1920, causes_1418)

# using hablar package's 'convert' to convert cause of death columns from character type to doubles to perform calculations
causes_1420 <- causes_1420 %>%
  convert(dbl(allcause,
              naturalcause,
              septicemia_a40_a41,
              malignant_neoplasms_c00_c97,
              diabetes_mellitus_e10_e14,
              alzheimer_disease_g30,
              influenza_and_pneumonia_j10,
              chronic_lower_respiratory,
              other_diseases_of_respiratory,
              nephritis_nephrotic_syndrome,
              symptoms_signs_and_abnormal,
              diseases_of_heart_i00_i09,
              cerebrovascular_diseases,
              covid19_multiple_causes_of_death,
              covid19_underlying_cause_of_death
  ))
# calculate unnatural causes of death
causes_1420 <- causes_1420 %>%
  mutate(unnaturalcause = allcause - naturalcause)
# dropping Puerto Rico to count only jurisdictions in 50 states, per CDC guidance
causes_1420 <- causes_1420 %>%
  filter(jurisdiction_of_occurrence != "Puerto Rico")
# summing states and grouping data by week
causes_us_1420 <- causes_1420 %>%
  group_by(weekendingdate) %>%
  summarize(allcause = sum(allcause, na.rm=TRUE),
            naturalcause = sum(naturalcause, na.rm=TRUE),
            unnaturalcause = sum(unnaturalcause, na.rm=TRUE),
            septicemia_a40_a41 = sum(septicemia_a40_a41, na.rm=TRUE),
            malignant_neoplasms_c00_c97 = sum(malignant_neoplasms_c00_c97, na.rm=TRUE),
            diabetes_mellitus_e10_e14 = sum(diabetes_mellitus_e10_e14, na.rm=TRUE),
            alzheimer_disease_g30 = sum(alzheimer_disease_g30, na.rm=TRUE),
            influenza_and_pneumonia_j10 = sum(influenza_and_pneumonia_j10, na.rm=TRUE),
            chronic_lower_respiratory = sum(chronic_lower_respiratory, na.rm=TRUE),
            other_diseases_of_respiratory = sum(other_diseases_of_respiratory, na.rm=TRUE),
            nephritis_nephrotic_syndrome = sum(nephritis_nephrotic_syndrome, na.rm=TRUE),
            symptoms_signs_and_abnormal = sum(symptoms_signs_and_abnormal, na.rm=TRUE),
            diseases_of_heart_i00_i09 = sum(diseases_of_heart_i00_i09,na.rm=TRUE),
            cerebrovascular_diseases = sum(cerebrovascular_diseases, na.rm=TRUE),
            covid19_multiple_causes_of_death=sum(covid19_multiple_causes_of_death, na.rm=T),
            covid19_underlying_cause_of_death=sum(covid19_underlying_cause_of_death, na.rm=T)) %>%
  mutate(naturalcause_perc = round(naturalcause/allcause,3),
         unnaturalcause_perc = round(unnaturalcause/allcause,3),
         septicemia_perc = round(septicemia_a40_a41/allcause,3),
         malignant_neoplasms_perc = round(malignant_neoplasms_c00_c97/allcause,3),
         diabetes_mellitus_perc = round(diabetes_mellitus_e10_e14/allcause,3),
         alzheimer_perc = round(alzheimer_disease_g30/allcause,3),
         influenza_pneumonia_perc = round(influenza_and_pneumonia_j10/allcause,3),
         chronic_lower_respiratory_perc = round(chronic_lower_respiratory/allcause,3),
         other_respiratory_diseases_perc = round(other_diseases_of_respiratory/allcause,3),
         nephritis_nephrotic_syndrome_perc = round(nephritis_nephrotic_syndrome/allcause,3),
         symptoms_signs_and_abnormal_perc = round(symptoms_signs_and_abnormal/allcause,3),
         heart_disease_perc = round(diseases_of_heart_i00_i09/allcause,3),
         cerebrovascular_diseases_perc = round(cerebrovascular_diseases/allcause,3),
         covid19_multiple_causes_of_death_perc=round(covid19_multiple_causes_of_death/allcause,3),
         covid19_underlying_cause_of_death_perc=round(covid19_underlying_cause_of_death/allcause,3)
  )

causes_us_1420 <- causes_us_1420 %>% 
  mutate(one=1,
         state="US")

## states


state.abb <- c(state.abb, "DC", "NYC")
state.name <- c(state.name, "District of Columbia", "New York City")
state_names <- data.frame(state.abb, state.name)
state_names$state.abb <- as.character(state_names$state.abb)
state_names$state.name <- as.character(state_names$state.name)


causes_1420_long <- causes_1420 %>%
#  filter(weekendingdate != '2020-05-09',
#         weekendingdate != '2020-05-02',
#         weekendingdate != '2020-04-25') %>% #,
  #weekendingdate != '2020-04-18')
  mutate(covid19_multiple_causes_of_death=as.numeric(covid19_multiple_causes_of_death),
         covid19_underlying_cause_of_death=as.numeric(covid19_underlying_cause_of_death)) %>% 
  ungroup() %>% 
  pivot_longer(cols=5:ncol(causes_1420),
               names_to="type",
               values_to="count") %>% 
  mutate(weekendingdate=ymd(weekendingdate)) %>% 
  #left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  filter(WEEK <= week_max) %>% 
  left_join(state_names, by=c("jurisdiction_of_occurrence"= "state.name"))

#### slice out cases ---

## diseases_of_heart
causes_1a <- causes_1420_long %>% 
  filter(type=="diseases_of_heart_i00_i09") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=diseases_of_heart_i00_i09, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save1 <- causes_join %>% 
  mutate(type="diseases_of_heart_i00_i09")

write_csv(causes_join, paste0("specific_causes/diseases_of_heart_i00_i09_", Sys.Date(), ".csv"))

## alzheimer_disease_g30
causes_1a <- causes_1420_long %>% 
  filter(type=="alzheimer_disease_g30") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=alzheimer_disease_g30, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save2 <- causes_join %>% 
  mutate(type="alzheimer_disease_g30")

write_csv(causes_join, paste0("specific_causes/alzheimer_disease_g30_", Sys.Date(), ".csv"))


## cerebrovascular_diseases
causes_1a <- causes_1420_long %>% 
  filter(type=="cerebrovascular_diseases") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=cerebrovascular_diseases, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))


causes_save3 <- causes_join %>% 
  mutate(type="cerebrovascular_diseases")

write_csv(causes_join, paste0("specific_causes/cerebrovascular_diseases_", Sys.Date(), ".csv"))


## chronic_lower_respiratory
causes_1a <- causes_1420_long %>% 
  filter(type=="chronic_lower_respiratory") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=chronic_lower_respiratory, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save4 <- causes_join %>% 
  mutate(type="chronic_lower_respiratory")

write_csv(causes_join, paste0("specific_causes/chronic_lower_respiratory_", Sys.Date(), ".csv"))


## covid19_multiple_causes_of_death

causes_1a <- causes_1420_long %>% 
  filter(type=="covid19_multiple_causes_of_death") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=covid19_multiple_causes_of_death, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save5 <- causes_join %>% 
  mutate(type="covid19_multiple_causes_of_death")

write_csv(causes_join, paste0("specific_causes/covid19_multiple_causes_of_death_", Sys.Date(), ".csv"))

## covid19_underlying_cause_of_death

causes_1a <- causes_1420_long %>% 
  filter(type=="covid19_underlying_cause_of_death") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=covid19_underlying_cause_of_death, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save6 <- causes_join %>% 
  mutate(type="covid19_underlying_cause_of_death")

write_csv(causes_join, paste0("specific_causes/covid19_underlying_cause_of_death_", Sys.Date(), ".csv"))

## diabetes_mellitus_e10_e14

causes_1a <- causes_1420_long %>% 
  filter(type=="diabetes_mellitus_e10_e14") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=diabetes_mellitus_e10_e14, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save7 <- causes_join %>% 
  mutate(type="diabetes_mellitus_e10_e14")

write_csv(causes_join, paste0("specific_causes/diabetes_mellitus_e10_e14_", Sys.Date(), ".csv"))


## influenza_and_pneumonia_j10

causes_1a <- causes_1420_long %>% 
  filter(type=="influenza_and_pneumonia_j10") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=influenza_and_pneumonia_j10, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save8 <- causes_join %>% 
  mutate(type="influenza_and_pneumonia_j10")

write_csv(causes_join, paste0("specific_causes/influenza_and_pneumonia_j10_", Sys.Date(), ".csv"))

## malignant_neoplasms_c00_c97 

causes_1a <- causes_1420_long %>% 
  filter(type=="malignant_neoplasms_c00_c97") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=malignant_neoplasms_c00_c97, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save9 <- causes_join %>% 
  mutate(type="malignant_neoplasms_c00_c97")

write_csv(causes_join, paste0("specific_causes/malignant_neoplasms_c00_c97_", Sys.Date(), ".csv"))

## nephritis_nephrotic_syndrome

causes_1a <- causes_1420_long %>% 
  filter(type=="nephritis_nephrotic_syndrome") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=nephritis_nephrotic_syndrome, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save10 <- causes_join %>% 
  mutate(type="nephritis_nephrotic_syndrome")

write_csv(causes_join, paste0("specific_causes/nephritis_nephrotic_syndrome_", Sys.Date(), ".csv"))

## other_diseases_of_respiratory

causes_1a <- causes_1420_long %>% 
  filter(type=="other_diseases_of_respiratory") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=other_diseases_of_respiratory, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save11 <- causes_join %>% 
  mutate(type="other_diseases_of_respiratory")

write_csv(causes_join, paste0("specific_causes/other_diseases_of_respiratory_", Sys.Date(), ".csv"))

## septicemia_a40_a41

causes_1a <- causes_1420_long %>% 
  filter(type=="septicemia_a40_a41") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=septicemia_a40_a41, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
causes_join$one <- 1
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save12 <- causes_join %>% 
  mutate(type="septicemia_a40_a41")

write_csv(causes_join, paste0("specific_causes/septicemia_a40_a41_", Sys.Date(), ".csv"))

## symptoms_signs_and_abnormal

causes_1a <- causes_1420_long %>% 
  filter(type=="symptoms_signs_and_abnormal1") %>% 
  select(state=state.abb, week_end=weekendingdate, count, WEEK, SEASON)
causes_1b <- causes_us_1420 %>% 
  left_join(weeks_df, by=c("weekendingdate"="end_date")) %>% 
  select(state, week_end=weekendingdate, count=symptoms_signs_and_abnormal, WEEK, SEASON)
causes_join <- rbind(causes_1a, causes_1b)
#causes_join$one <- 1
causes_join$one <- .5
causes_join$state <- as.character(causes_join$state)
causes_join <- causes_join %>% 
  filter(!is.na(count))

causes_save13 <- causes_join %>% 
  mutate(type="symptoms_signs_and_abnormal")

write_csv(causes_join, paste0("specific_causes/symptoms_signs_and_abnormal_", Sys.Date(), ".csv"))


causes_long <- rbind(causes_save1, causes_save2, causes_save3, causes_save4,
                     causes_save5, causes_save6, causes_save7, causes_save8,
                     causes_save9, causes_save10, causes_save11, 
                     causes_save12, causes_save13)

write_csv(causes_long, paste0("specific_causes/causes_long_", Sys.Date(), ".csv"))

          