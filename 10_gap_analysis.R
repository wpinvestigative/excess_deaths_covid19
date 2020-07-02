library(tidyverse)
library(janitor)
library(lubridate)

cdc1 <- read_csv("https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD")
cdc1 <- clean_names(cdc1) %>% 
  mutate(end_week=mdy(end_week)) %>% 
  group_by(state) %>%
  arrange(end_week) %>% 
  mutate(week=row_number()+4)

#cdc1 %>% 
#  ggplot(aes(x=end_week, y=covid_19_deaths)) +
#  geom_line() +
#  facet_wrap(~state, ncol=5, scales="free_y")

cdc2 <- read_csv("data/weekly_fluview/State_Custom_data25.csv")

cdc2 <- clean_names(cdc2) %>% 
  filter(season=="2019-20") %>% 
  select(week, state=sub_area, total_deaths2=total_deaths) %>% 
  mutate(total_deaths2=gsub(",", "", total_deaths2)) %>% 
  mutate(total_deaths2=as.numeric(total_deaths2))



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
  mutate(check_all=case_when(
    total_deaths >  total_deaths2 ~ "More",
    total_deaths  ==  total_deaths2 ~ "Equal",
    total_deaths  <  total_deaths2 ~ "Less"
  )) %>% 
  select(start_week, end_week, state, total_deaths_a=total_deaths, 
         total_deaths_b=total_deaths2) %>% 
  pivot_longer(cols=4:5, names_to="type", values_to="deaths")

ggplot(us1, aes(x=end_week, y=deaths, color=type)) +
  geom_line() +
  facet_wrap(~state, ncol=5, scales="free_y")

gap_state <- cdc3 %>% 
  filter(week<=23) %>% 
  group_by(state) %>% 
  summarize(total1=sum(total_deaths, na.rm=T),
            total2=sum(total_deaths2, na.rm=T)) %>% 
  mutate(difference=total1-total2)

gap_state_summary <-
  gap_state %>% 
  filter(total2!=0) %>% 
  summarize(total1=sum(total1),
            total2=sum(total2))
