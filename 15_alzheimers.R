library(tidyverse)
library(lubridate)
library(ExcessILI)
library(DT)
library(janitor)
source('specific_causes/00_format_table.R')

states_list <- c("GA", "NYC", "FL",  "NJ", "MI", "US")

#states_list <- "US"
#specific_causes <- c("diseases_of_heart_i00_i09", "alzheimer_disease_g30", "cerebrovascular_diseases", "diabetes_mellitus_e10_e14")

# run 01_all_causes.R to generate the spreadsheet below

causes_join <- read_csv("https://data.cdc.gov/api/views/u6jv-9ijr/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
causes_join <- clean_names(causes_join)
#causes_join <- read_csv("specific_causes/causes_long_2020-08-06.csv")
causes_join$one <- 1

causes_join <- causes_join %>% 
  filter(state_abbreviation %in% states_list)

causes_join <- filter(causes_join, type=="Unweighted")

#causes_join <- causes_join %>% 
#  filter(type %in% specific_causes)

#type_list <- unique(causes_join$type)
#type_list <- unique(causes_join$cause_group)
type_list <- unique(causes_join$cause_subgroup)


for (i in 1:length(type_list)) {
#  if (!i %in% c(5,6)) {
    type_save <- type_list[i]
    df <- causes_join %>% 
      #filter(cause_group==type_save) %>% 
      filter(cause_subgroup==type_save) %>% 
      filter(!is.na(number_of_deaths)) %>% 
      #filter(!is.na(WEEK)) %>% 
      filter(!is.na(week)) %>% 
      unique()
    
    # model time
    excess_cause <-
      excessCases(ds = df,
                  datevar      = "week_ending_date",
                  state        = "state_abbreviation",
                  #agevar       = "none",
                  adj.flu      = "none",
                  denom.var    = "one",
                  use.syndromes = c("number_of_deaths"),
                  sum.dates     = ymd("2020-02-29"),
                  extrapolation.date = ymd("2020-01-25"),
                  model.type="poisson",
                  time.res="week",
                  stage1.samples=100,
                  stage2.samples=100,
                  extend.epiyear=TRUE)
    
    #### uh -----
    ds <- excess_cause
    dates1 <-
      ds[[1]][[1]][[1]]$date
    
    sum.pred.iter <-    
      excessExtract(ds = ds,
                    syndrome = 'number_of_deaths',
                    extract.quantity = "sum.pred.iter")
    sum.obs <-
      excessExtract(ds = ds,
                    syndrome = 'number_of_deaths',
                    extract.quantity = "sum.obs")
    
    sum.obs.state <- apply(sum.obs,2,sum)
    
    sum.cases.excess <- sapply(1:length(sum.obs.state),
                               function(x){
                                 sum.obs.state[x] -  sum.pred.iter[,x,1]
                               })
    
    
    # fails here
    cause.summary.state <- format_excess_func(excess_cause,'number_of_deaths')
    cause.summary.state$type <- type_list[i]
    #res.ac$type <- age_groups[i]
    
    
    ### come back here and save! ----
    
    
    if (i==1) {
      summary_save <- cause.summary.state
      
    } else {
      summary_save <- rbind(summary_save, cause.summary.state)
      
    }
    print(type_list[i])
#  }
}

cause.summary.state.df <- cause.summary.state.df %>% 
  filter(mmwr_year==2020) %>% 
  filter(mmwr_week %in% 10:33) 

cause.summary.state.df <- summary_save %>% 
  filter(mmwr_year==2020) %>% 
  filter(mmwr_week %in% 10:33) 
write_csv(cause.summary.state.df, paste0("cause.summary.state.df_", Sys.Date(), ".csv"))

#### all states alzheimer


causes_join <- read_csv("https://data.cdc.gov/api/views/u6jv-9ijr/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
causes_join <- clean_names(causes_join)
#causes_join <- read_csv("specific_causes/causes_long_2020-08-06.csv")
causes_join$one <- 1

#causes_join <- causes_join %>% 
#  filter(state_abbreviation %in% states_list)

causes_join <- filter(causes_join, type=="Unweighted")



  #  if (!i %in% c(5,6)) {
  type_save <- "Alzheimer disease and dementia"
  df <- causes_join %>% 
    #filter(cause_group==type_save) %>% 
    filter(cause_subgroup==type_save) %>% 
    filter(!is.na(number_of_deaths)) %>% 
    #filter(!is.na(WEEK)) %>% 
    filter(!is.na(week)) %>% 
    unique()
  
  
  df_count <- df %>% 
    #count(state_abbreviation, cause_group) %>% 
    count(state_abbreviation, cause_subgroup) %>% 
    filter(n>50) %>% 
    pull(state_abbreviation)
  
  df <- df %>% 
    filter(state_abbreviation %in% df_count)
  
  # model time
  excess_cause <-
    excessCases(ds = df,
                datevar      = "week_ending_date",
                state        = "state_abbreviation",
                #agevar       = "none",
                adj.flu      = "none",
                denom.var    = "one",
                use.syndromes = c("number_of_deaths"),
                sum.dates     = ymd("2020-02-29"),
                extrapolation.date = ymd("2020-01-25"),
                model.type="negbin",
                time.res="week",
                stage1.samples=100,
                stage2.samples=100,
                extend.epiyear=TRUE)
  
  #### uh -----
  ds <- excess_cause
  dates1 <-
    ds[[1]][[1]][[1]]$date
  
  sum.pred.iter <-    
    excessExtract(ds = ds,
                  syndrome = 'number_of_deaths',
                  extract.quantity = "sum.pred.iter")
  sum.obs <-
    excessExtract(ds = ds,
                  syndrome = 'number_of_deaths',
                  extract.quantity = "sum.obs")
  
  sum.obs.state <- apply(sum.obs,2,sum)
  
  sum.cases.excess <- sapply(1:length(sum.obs.state),
                             function(x){
                               sum.obs.state[x] -  sum.pred.iter[,x,1]
                             })
  
  
  # fails here
  cause.summary.state <- format_excess_func(excess_cause,'number_of_deaths')
  cause.summary.state$type <- type_save
  #res.ac$type <- age_groups[i]
  
  
  ### come back here and save! ----
  
  

    summary_save <- cause.summary.state
    
cause.summary.state.df <- cause.summary.state.df %>% 
  filter(mmwr_year==2020) %>% 
  filter(mmwr_week %in% 10:30) 

cause.summary.state.df <- summary_save %>% 
  filter(mmwr_year==2020) %>% 
  filter(mmwr_week %in% 10:30) 
write_csv(cause.summary.state.df, paste0("alz.cause.summary.state.df_", Sys.Date(), ".csv"))



## double checking



us <- read_csv("https://data.cdc.gov/api/views/u6jv-9ijr/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
us <- clean_names(us)

us <- us %>% 
  #filter(type=="Unweighted" & cause_group=="Alzheimer disease and dementia") %>% 
  filter(type=="Unweighted") %>% 
  filter(state_abbreviation=="US") %>% 
  filter(week >9 & week <31) %>% 
  mutate(year_of=case_when(
    year==2020 ~ "2020",
    TRUE ~ "2015-2019"
  )) %>% 
  mutate(year=as.factor(as.character(year)))

us_agg <-us %>% 
  group_by(cause_group, year_of, week) %>% 
  summarize(deaths=mean(number_of_deaths)) %>% 
  pivot_wider(names_from="year_of",
              values_from="deaths") %>% 
  clean_names() %>% 
  group_by(cause_group) %>% 
  summarize(x2015_2019=sum(x2015_2019), 
            x2000=sum(x2020)) %>% 
  mutate(change=round((x2000-x2015_2019)/x2015_2019*100,2))



ggplot(us, aes(x=week, y=number_of_deaths, group=year, color=year)) +
  geom_line() +
  labs(title="alzheimer deaths over time") +
  theme_minimal()

us_agg <- us %>% 
  group_by(year_of, week) %>% 
  summarize(deaths=mean(number_of_deaths))

us_agg %>% group_by(year_of) %>% 
  summarize(deaths=sum(deaths))

ggplot(us_agg, aes(x=week, y=deaths, group=year_of, color=year_of)) +
  geom_line() +
  labs(title="alzheimer deaths 2020 vs avg") +
  theme_minimal()


cause.summary.state.df %>% 
  filter(type=="Alzheimer disease and dementia") %>% 
  filter(state=="US") %>% 
ggplot() +
  geom_line(aes(x=mmwr_week, y=obs), color="blue") +
  geom_line(aes(x=mmwr_week, y=pred), color="red") +
  labs(title="alzheimer deaths 2020 vs expected baseline") +
  theme_minimal()
