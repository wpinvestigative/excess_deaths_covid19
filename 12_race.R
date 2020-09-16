  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(ExcessILI)
  source("functions/format_table.R")
  # provisional covid deaths by county
  # https://data.cdc.gov/api/views/kn79-hsxy/rows.csv?accessType=DOWNLOAD
  
  # weekly state deaths by age group
  # https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD
  
  # weekly covid deaths by age group
  # https://data.cdc.gov/api/views/9bhg-hcku/rows.csv?accessType=DOWNLOAD
  
  # weekly covid deaths by age group plus total 
  # https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD
  
  # weekly state deaths by race group
  race <- read_csv("https://data.cdc.gov/api/views/qfhf-uhaa/rows.csv?accessType=DOWNLOAD")
  race <- read.csv("https://data.cdc.gov/api/views/qfhf-uhaa/rows.csv?accessType=DOWNLOAD")
  #race2 <- read_csv("https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD")
  #race2 <- clean_names(race2)
  race <- clean_names(race)
  
  race <- race  %>% 
    mutate(week_ending_date=mdy(week_ending_date)) %>% 
    mutate(one=1) %>% 
    filter(type=="Predicted (weighted)")
  
  race_groups <- race %>%
    filter(race_ethnicity!="Other") %>% 
    pull(race_ethnicity) %>% unique()
  
  # 1. 25-44 years are no good
  # 2. 
  for (i in 1:length(race_groups)) {
    
    sliced <- race %>% 
      filter(race_ethnicity == race_groups[i]) %>% 
      filter(outcome=="All Cause") %>% 
      filter(!is.na(number_of_deaths)) #%>% 
    #    filter(race_group == "65-74 years") %>% 
    #filter(jurisdiction=="United States")
    
    # 1 - 17
    # 2 - 18
    # 3 - 20
    # 4 - 46
    # 5 - 0
    
    filter_n <- c(17,20,45,35,0)
    

    sliced_count <- sliced %>% 
      count(state_abbreviation, race_ethnicity) %>% 
      filter(n>filter_n[i]) %>% 
      pull(state_abbreviation)

    
    sliced <- sliced %>% 
      filter(state_abbreviation %in% sliced_count)
    
    excess_age <-
      excessCases(ds = sliced,
                  datevar       = "week_ending_date",
                  agevar        = "race_ethnicity",
                  statevar      = "state_abbreviation",
                  adj.flu       = "none",
                  denom.var     = "one",
                  use.syndromes = c("number_of_deaths"),
                  sum.dates     = ymd("2020-02-29"),
#                  sum.dates     = ymd("2020-03-01"),
                  extrapolation.date = ymd("2020-01-25"),
#                  extrapolation.date = ymd("2020-01-26"),
                  model.type="negbin",
                  time.res="week",
                  stage1.samples=100,
                  stage2.samples=100,
                  extend.epiyear=TRUE)
    
    ds <- excess_age
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
    
    # this one's okay
    sum.excess.deaths.range.pi <-t(
      apply(sum.cases.excess,2,quantile,  probs=c(0.025,0.5,0.975)))
    
    sum.excess.deaths.range.pi <- cbind.data.frame('jurisdiction'=unique(sliced$jurisdiction), sum.excess.deaths.range.pi)
    
    # fails here
    cause.summary.state <- format_excess_func(excess_age,'number_of_deaths')
    cause.summary.state$type <- race_groups[i]
    
    if (i==1) {
      summary_save <- cause.summary.state
    } else {
      summary_save <- rbind(summary_save, cause.summary.state)
    }
    print(race_groups[i])
  }
  
  write_csv(summary_save, "race_groups_073020.csv", na="")
  
  ### testing ---
  
  race <- race %>% 
    mutate(Year=case_when(
      mmwr_year ==2020 ~ "2020",
      TRUE ~ "15-19"
    ))
  
  covid <- race %>% 
    filter(mmwr_year==2020) %>% 
    filter(outcome=="COVID-19") %>% 
    mutate(Year="COVID-19") %>% 
    mutate(outcome="All Cause")
  
  race2 <- rbind(race, covid)
  
  #race2 %>% 
  race %>% 
    filter(state_abbreviation=="US") %>% 
    filter(outcome=="All Cause") %>% 
    filter(mmwr_week<29) %>% 
    ggplot(aes(x=mmwr_week, y=number_of_deaths, color=Year, group=mmwr_year)) +
    geom_line() +
#    facet_wrap(~race_ethnicity, scale="free_y") +
    facet_wrap(~race_ethnicity) +
    theme_minimal() +
    labs(title="Historical all-causes deaths by age group")
  