library(tidyverse)
library(jsonlite)
library(janitor)
library(lubridate)

test <- fromJSON("https://data.cdc.gov/resource/hc4f-j6nb.json")
url2 <- "https://data.cdc.gov/resource/r8kw-7aab.json"
cdc1 <- read_csv("https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD")
cdc1 <- clean_names(cdc1) %>% 
  mutate(end_week=mdy(end_week))

cdc1 %>% 
  ggplot(aes(x=end_week, y=covid_19_deaths)) +
  geom_line() +
  facet_wrap(~state, ncol=5, scales="free_y")

cdc2 <- read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD")

cdc2 <- clean_names(cdc2) %>% 
  mutate(end_week=ymd(week_ending_date)) %>% 
  select(state=jurisdiction_of_occurrence, mmwr_year, mmwr_week,
         end_week, all_cause, covid_19_u071_multiple_cause_of_death,
         covid_19_u071_underlying_cause_of_death)

cdc3 <- left_join(cdc1, cdc2)

florida <- cdc3 %>% 
  filter(state=="Florida") %>% 
  mutate(check_covid=case_when(
    covid_19_deaths >  covid_19_u071_multiple_cause_of_death ~ "More",
    covid_19_deaths ==  covid_19_u071_multiple_cause_of_death ~ "Equal",
    covid_19_deaths <  covid_19_u071_multiple_cause_of_death ~ "Less"
  ),
  check_all=case_when(
    total_deaths >  all_cause ~ "More",
    total_deaths ==  all_cause ~ "Equal",
    total_deaths <  all_cause ~ "Less"
  )) %>% 
  select(start_week, end_week, state, covid_19_deaths_a=covid_19_deaths, total_deaths_a=total_deaths, 
         covid_19_deaths_b=covid_19_u071_multiple_cause_of_death,
         total_deaths_b=all_cause)



us1 <- cdc3 %>% 
  filter(state=="Florida") %>% 
  mutate(check_covid=case_when(
    covid_19_deaths >  covid_19_u071_multiple_cause_of_death ~ "More",
    covid_19_deaths ==  covid_19_u071_multiple_cause_of_death ~ "Equal",
    covid_19_deaths <  covid_19_u071_multiple_cause_of_death ~ "Less"
  ),
  check_all=case_when(
    total_deaths >  all_cause ~ "More",
    total_deaths ==  all_cause ~ "Equal",
    total_deaths <  all_cause ~ "Less"
  )) %>% 
  select(start_week, end_week, state, covid_19_deaths_a=covid_19_deaths, total_deaths_a=total_deaths, 
         covid_19_deaths_b=covid_19_u071_multiple_cause_of_death,
         total_deaths_b=all_cause)



us2 <- cdc3 %>% 
  mutate(check_covid=case_when(
    covid_19_deaths >  covid_19_u071_multiple_cause_of_death ~ "More",
    covid_19_deaths ==  covid_19_u071_multiple_cause_of_death ~ "Equal",
    covid_19_deaths <  covid_19_u071_multiple_cause_of_death ~ "Less"
  ),
  check_all=case_when(
    total_deaths >  all_cause ~ "More",
    total_deaths ==  all_cause ~ "Equal",
    total_deaths <  all_cause ~ "Less"
  )) %>% 
  select(start_week, end_week, state, covid_19_deaths_a=covid_19_deaths, 
         covid_19_deaths_b=covid_19_u071_multiple_cause_of_death) %>% 
  pivot_longer(cols=4:5, names_to="type", values_to="deaths")

ggplot(us2, aes(x=end_week, y=deaths, color=type)) +
  geom_line() +
  facet_wrap(~state, ncol=5, scales="free_y")



us1 <- cdc3 %>% 
  mutate(check_covid=case_when(
    covid_19_deaths >  covid_19_u071_multiple_cause_of_death ~ "More",
    covid_19_deaths ==  covid_19_u071_multiple_cause_of_death ~ "Equal",
    covid_19_deaths <  covid_19_u071_multiple_cause_of_death ~ "Less"
  ),
  check_all=case_when(
    total_deaths >  all_cause ~ "More",
    total_deaths ==  all_cause ~ "Equal",
    total_deaths <  all_cause ~ "Less"
  )) %>% 
  select(start_week, end_week, state, total_deaths_a=total_deaths, 
         total_deaths_b=all_cause) %>% 
  pivot_longer(cols=4:5, names_to="type", values_to="deaths")

ggplot(us1, aes(x=end_week, y=deaths, color=type)) +
  geom_line() +
  facet_wrap(~state, ncol=5, scales="free_y")
