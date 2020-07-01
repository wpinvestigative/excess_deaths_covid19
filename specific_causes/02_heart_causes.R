library(tidyverse)
library(lubridate)
library(ExcessILI)
library(DT)
source('00_format_table.R')

states_list <- c("NY", "NYC", "IL", "MA", "NJ", "MI")

# run 15_all_causes.R to generate the spreadsheet below

#causes_join <- read_csv("data/specific_causes/causes_long_2020-05-20.csv")
#causes_join <- read_csv("data/specific_causes/causes_long_2020-06-04.csv")
causes_join_old <- read_csv("causes_long_2020-06-19.csv")
causes_join <- read_csv("causes_long_2020-06-25.csv")
causes_join$one <- 1
causes_join <- causes_join %>% 
  filter(state %in% states_list)

type_list <- unique(causes_join$type)


for (i in 1:length(type_list)) {
  if (!i %in% c(5,6)) {
  type_save <- type_list[i]
  df <- causes_join %>% 
    filter(type==type_save) %>% 
    unique()
  
  # model time
  excess_cause <-
    excessCases(ds = df,
                datevar      = "week_end",
                state        = "state",
                adj.flu      = "none",
                denom.var    = "one",
                use.syndromes = c("count"),
                sum.dates = ymd("2020-05-16"),
                extrapolation.date = ymd("2020-01-26"),
                model.type="negbin",
                time.res="week",
                stage1.samples=1000,
                stage2.samples=1)
  
  ds <- excess_cause
  dates1 <-
    ds[[1]][[1]][[1]]$date
  
  sum.pred.iter <-    
    excessExtract(ds = ds,
                  syndrome = 'count',
                  extract.quantity = "sum.pred.iter")
  sum.obs <-
    excessExtract(ds = ds,
                  syndrome = 'count',
                  extract.quantity = "sum.obs")
  sum.obs.state <- apply(sum.obs,2,sum)
  sum.cases.excess <- sapply(1:length(sum.obs.state),
                             function(x){
                               sum.obs.state[x] -  sum.pred.iter[,x,1]
                             })
  
  sum.excess.deaths.range.pi <-t(
    apply(sum.cases.excess,2,quantile,  probs=c(0.025,0.5,0.975)))
  
  sum.excess.deaths.range.pi <- cbind.data.frame('state'=unique(causes_join$state), sum.excess.deaths.range.pi)
  
  cause.summary.state <- format_excess_func(excess_cause,'count' )
  cause.summary.state$type <- type_save
  
  if(i==1) {
  cause.summary.state.df <- cause.summary.state  
  } else {
    cause.summary.state.df <- rbind(cause.summary.state.df, cause.summary.state)
  }
  print(i)
}
}
cause.summary.state.df <- cause.summary.state.df %>% 
  filter(mmwr_year==2020) %>% 
  filter(mmwr_week %in% 10:23) 
write_csv(cause.summary.state.df, paste0("cause.summary.state.df_", Sys.Date(), ".csv"))


