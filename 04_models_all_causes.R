library(ExcessILI)
source("functions/format_table.R")

#### National analysis of all-cause deaths ----


#Run analysis
analysis.data$one <- 1
analysis.data2 <-
  analysis.data[!is.na(analysis.data$total_pni),]
analysis.data$one <- 365000000/100000
set.seed(123)
excess_ac_natl <-
  excessCases(ds = analysis.data2,
              datevar       = "week_end",
              statevar      = "state",
              adj.flu       = "none" ,
              denom.var     = "prop.complete.offset",
              use.syndromes = c("all_deaths"),
              covs= c('log.flu.deseasonalize.0'),
              extrapolation.date = extrap.date,
              sum.dates=count.start.date,
              model.type='poisson',
              time.res='week')
ds <- excess_ac_natl
dates1 <-
  ds[[1]][[1]][[1]]$date

sum.obs.ac <-
  excessExtract(ds = ds,
                syndrome = 'all_deaths',
                extract.quantity = "sum.obs")
denom.check <-
  excessExtract(ds = ds,
                syndrome = 'all_deaths',
                extract.quantity = "denom")
##National data is created by summing state data
sum.pred.iter.ac <-    
  excessExtract(ds = ds,
                syndrome = 'all_deaths',
                extract.quantity = "sum.pred.iter")
ac.nat.obs <-    
  excessExtract(ds = ds,
                syndrome = 'all_deaths',
                extract.quantity = "y")
ac.nat.obs <- ac.nat.obs[,dimnames(ac.nat.obs)[[2]]!='US',1]
ac.nat.obs <- apply(ac.nat.obs,1,sum)
pred.iter.ac <-    
  excessExtract(ds = ds,
                syndrome = 'all_deaths',
                extract.quantity = "pred.iter")
pred.iter.ac2 <- array(pred.iter.ac,dim=c(length(dates1),10000,dim(pred.iter.ac)[2],1 ))
dimnames(pred.iter.ac2)[[3]] <- dimnames(pred.iter.ac)[[2]]
#Sum the state-level estimates to get national estimate
pred.iter.ac2.nat <-
  pred.iter.ac2[,,dimnames(pred.iter.ac2)[[3]]!='US',1]
pred.iter.ac2.nat.sum <- apply(pred.iter.ac2.nat,c(1,2),sum)
pred.iter.ac2.nat.q <- t(apply(pred.iter.ac2.nat.sum,1, quantile, probs=c(0.025,0.5,0.975)))
res.ac.nat <- cbind.data.frame('week_start'=dates1,'all_cause'=ac.nat.obs,pred.iter.ac2.nat.q)
names(res.ac.nat) <- c('week_start','obs','lpi','pred','upi')
res.ac.nat$week_end <- res.ac.nat$week_start+days(7)
covid.deaths.nat <- aggregate(cd1[cd1$state!='US',"covid.death.hybrid",drop=F], by=list('week_end'=cd1$week_end[cd1$state!='US']), FUN=sum)
covid.deaths.nat$week_start <- covid.deaths.nat$week_end -days(6)
res.ac.nat <- merge(res.ac.nat, covid.deaths.nat, by=c('week_start', 'week_end') , all=T)
res.ac.nat$unexplained.cases <- res.ac.nat$obs - res.ac.nat$pred
res.ac.nat$state <- 'US.agg'
mmwr.ag1 <- mmwr_week(res.ac.nat$week_end)
res.ac.nat <- cbind.data.frame(mmwr.ag1,res.ac.nat)
res.ac.nat <- res.ac.nat[!is.na(res.ac.nat$pred),]
sum.obs.ac2 <- apply(sum.obs.ac,2,sum)
sum.ac.excess <- sapply(1:length(sum.obs.ac2),
                        function(x){
                          sum.obs.ac2[x] -  sum.pred.iter.ac[,x,1]
                        })
sum.excess.deaths.range.ac <-t(
  apply(sum.ac.excess,2,quantile,  probs=c(0.025,0.1,0.3,0.5,0.975)))
sum.excess.deaths.range.ac <- cbind.data.frame('state'=unique(analysis.data2$state), sum.excess.deaths.range.ac)
res.ac <- format_excess_func(excess_ac_natl,'all_deaths')
res.ac$state <- as.character(res.ac$state)

# fix this error here
#res.ac2 <- cd1 %>% 
#  select(covid.death.hybrid, state, week_end) %>% 
#  full_join(res.ac)
res.ac <- merge(res.ac,cd1[,c("covid.death.hybrid", 'state', 'week_end')] , by=c('state', 'week_end'))
res.ac <- bind_rows(res.ac.nat,res.ac)
res.ac <- res.ac[!is.na(res.ac$pred),]


#national: sum state estimates
state.sum.pred.iter.ac <- sum.pred.iter.ac[,dimnames(sum.pred.iter.ac)[[2]]!='US',]
nat.sum.pred.iter.ac <- apply(state.sum.pred.iter.ac,1,sum)
sum.obs.ac.nat <- sum(sum.obs.ac2[names(sum.obs.ac2)!='US'])
excess.nat.iter <-  sum.obs.ac.nat - nat.sum.pred.iter.ac
sum.excess.deaths.range.ac.nat <-
  quantile(excess.nat.iter,  probs=c(0.025,0.1,0.3,0.5,0.975))
sum.excess.deaths.range.ac <- bind_rows(sum.excess.deaths.range.ac.nat,sum.excess.deaths.range.ac)
sum.excess.deaths.range.ac$state <- as.character(sum.excess.deaths.range.ac$state)
sum.excess.deaths.range.ac$state[is.na(sum.excess.deaths.range.ac$state)] <- 'US.agg'


nat.range.ac <-sum.excess.deaths.range.ac.nat
formatted.ac <- 
  paste0(round(nat.range.ac['50%'],-2),
         '(' ,
         round(nat.range.ac['2.5%'],-2), ',' ,
         round(nat.range.ac['97.5%'],-2), ')'
  )
formatted.ac
nat.range.ac

res.pi2 <- res.pi %>% 
  mutate(week_start=week_end-6) %>% 
  select(year=mmwr_year, week=mmwr_week, day=mmwr_day,
         week_start, state, baseline_pi=pred, baseline_pi_lower=lpi,
         baseline_pi_upper=upi, pneumonia_influenza_covid=obs,
         excess_pneumonia_influenza_covid=unexplained.cases,
         pic.denom=denom, week_end_date=week_end)

res.ac2 <- res.ac %>% 
  select(-denom) %>% 
  select(state, week_end_date=week_end,
         week_start_date=week_start, 
         baseline_all_cause=pred,
         baseline_all_cause_lower=lpi,
         baseline_all_cause_upper=upi,
         all_cause_deaths=obs, 
         excess_all_cause_deaths=unexplained.cases,
         covid.death.hybrid)

comb1 <- merge(res.ac2, res.pi2,
               by.x=c('state','week_end_date','week_start_date'),by.y=c('state','week_end_date','week_start'), all=T)

comb1 <- full_join(res.ac2, res.pi2)

comb2 <- comb1

#Caution: NYC and NYstate combined in covidtracking data

#test.ds3.spl$week_end_date <- 
#  test.ds3.spl$date + days(6)
#comb2 <- merge(comb2, test.ds3.spl,
#               by=c('state','week_end_date'), all=T)

comb2 <- comb2[comb2$state!='US',] #this is unadjusted

write.csv(comb2,'data/outputs/national_and_state_summary.csv' )
