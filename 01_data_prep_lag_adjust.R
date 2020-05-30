#### This produces data that calculates state-level reporting lag

# generates these files
# data/outputs/NobBs.complete.csv
# data/outputs/NobBs.preds.csv
# data/outputs/NobBs.complete.iters.rds

library(tidyverse)
library(MMWRweek)
library(cdcfluview)
library(parallel)
library(lubridate)
library(rjags)
library(reshape2)
library(pbapply)
library(HDInterval)

# to update with latest week's data

#### 1. Scrape past national provisional data from NCHS ----
week.pad <-sprintf("%02d", c(1:52))

for(i in week.pad){
  
  for(j in c('2016-2017', '2017-2018', '2018-2019', '2019-2020')){
  #j='2019-2020'
    download.file(paste0('https://www.cdc.gov/flu/weekly/weeklyarchives', j,'/data/NCHSData',i,'.csv'),paste0('./data/provisional_pi/provisional', j,'_','week_',i,'.csv'))
  }
}

#### 2. Load state provisional data from NCHS fluview starting back in mid March ----

# downloaded from https://gis.cdc.gov/grasp/fluview/mortality.html
# week 14 not available because they switched from a 2-week lag to a 1-week lag around that time

wk11 <- 
  read.csv('data/weekly_fluview/State_Custom_Data11.csv')
wk11$max.date.report <- as.Date('2020-03-14')
wk11$report.date <- as.Date('2020-03-27')
wk12 <- 
  read.csv('data/weekly_fluview/State_Custom_Data12.csv')
wk12$max.date.report <- as.Date('2020-03-21')
wk12$report.date <- as.Date('2020-04-03')
wk13 <- 
  read.csv('data/weekly_fluview/State_Custom_Data13.csv')
wk13$max.date.report <- as.Date('2020-03-28')
wk13$report.date <- as.Date('2020-04-10')
wk15 <- 
  read.csv('data/weekly_fluview/State_Custom_Data15.csv')
wk15$max.date.report <- as.Date('2020-04-11')
wk15$report.date <- as.Date('2020-04-17')
wk16 <- 
  read.csv('data/weekly_fluview/State_Custom_Data16.csv')
wk16$max.date.report <- as.Date('2020-04-18')
wk16$report.date <- as.Date('2020-04-24')
wk17 <- 
  read.csv('data/weekly_fluview/State_Custom_Data17.csv')
wk17$max.date.report <- as.Date('2020-04-25')
wk17$report.date <- as.Date('2020-05-01')
wk18 <- 
  read.csv('data/weekly_fluview/State_Custom_Data18.csv')
wk18$max.date.report <- as.Date('2020-05-02')
wk18$report.date <- as.Date('2020-05-08')
wk19 <- 
  read.csv('data/weekly_fluview/State_Custom_Data19.csv')
wk19$max.date.report <- as.Date('2020-05-09')
wk19$report.date <- as.Date('2020-05-15')
wk20 <- 
  read.csv('data/weekly_fluview/State_Custom_Data20.csv')
wk20$max.date.report <- as.Date('2020-05-16')
wk20$report.date <- as.Date('2020-05-22')



#### 3. Load national provisional data from NCHS -----

provis.list <- lapply(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'),
                      function(x){
                        d1 <- read.csv(paste0('data/provisional_pi/provisional', '2019-2020','_','week_',x,'.csv'))
                        names(d1) <- toupper(names(d1))
                        d1$week.death <- MMWRweek2Date(d1$YEAR, d1$WEEK) + days (6)
                        d1$max.date.report <- max(d1$week.death)
                        # d1$report.date <- d1$max.date.report + days(12)
                        d1$report.date <- mmwr_week_to_date(2020,week= as.numeric(x))+ days(12)
                        d1$SUB.AREA <- 'US'
                        d1$epiyr <- d1$YEAR
                        d1$epiyr[d1$WEEK<=26] <- d1$YEAR[d1$WEEK<=26] - 1
                        return(d1)
                      })
nat.reports <- do.call('rbind.data.frame',provis.list)
all.reports <- rbind.data.frame(wk11,wk12,wk13,wk15, wk16, wk17, wk18, wk19, wk20)

all.reports <- all.reports %>% 
  mutate(epiyr= as.numeric(as.character(substr(SEASON,1,4))),
         year=case_when(
           WEEK <= 26 ~ epiyr+1,
           TRUE ~ epiyr),
         week.death = mmwr_week_to_date(year, WEEK)+6,
         NUM.INFLUENZA.DEATHS=as.numeric(gsub(',','',NUM.INFLUENZA.DEATHS)),
         NUM.PNEUMONIA.DEATHS=as.numeric(gsub(',','',NUM.PNEUMONIA.DEATHS)),
         TOTAL.DEATHS=as.numeric(gsub(',','',TOTAL.DEATHS)))

nat.reports2 <- nat.reports %>% 
  select(week.death, SUB.AREA, report.date, TOTAL.DEATHS=ALL.DEATHS)

all.reports2 <- all.reports %>% 
  select(week.death, SUB.AREA, report.date, TOTAL.DEATHS) %>% 
  rbind(nat.reports2) 

compare.m <- all.reports2 %>% 
  mutate(variable="TOTAL.DEATHS") %>% 
  select(death_date=week.death, state=SUB.AREA, report_date=report.date, 
         variable, N_deaths=TOTAL.DEATHS) %>% 
  mutate(report_date=as.Date(report_date),
         death_death=as.Date(death_date),
         complete.weeks=round(as.vector(difftime(report_date,
                                                 death_date, units='weeks'))))

compare.c <- compare.m %>% 
  select(death_date, state, report_date, N_deaths) %>% 
  pivot_wider(names_from="report_date",
              values_from="N_deaths")



#### 4. Set up reporting triangle ----

#Ignore reports from first week
#Filter if death date hasn't happened yet or happened a week ago
#Ignore reports from first week
compare.m.alt <- compare.m %>% 
  filter(complete.weeks <= 26) %>% 
  filter(as.numeric(report_date - death_date) > 6)
  
#Split by state and death date
compare.m.alt.spl <- 
  split(compare.m.alt, paste0(compare.m.alt$state,compare.m.alt$death_date)) 
#Sort by report date
compare.m.alt.spl <- 
  lapply(compare.m.alt.spl, function(x){
    x[order(x$report_date),]
  })


#Subtract previous value to calculate new value
compare.m.alt.spl <- lapply(compare.m.alt.spl, function(x){
  
  #Remove redundant rows if deaths was >6 months ago
  #If no observations before 26 weeks, just take last obs
  if(max(x$complete.weeks)==26 & min(x$complete.weeks)==26){
    x <-   x[nrow(x),] #take most complete observation
    x$complete.weeks <-999
  }
  
  #If some observations before and after 26 weeks
  if(max(x$complete.weeks)==26 & min(x$complete.weeks)<=26){
    x$wk26 <-0
    x$wk26[x$complete.weeks>=26] <-1
    x.early<-x[x$wk26==0,]  
    x.late <-x[x$wk26==1,]  
    x.late <-   x.late[nrow(x.late),, drop=F] #take most complete observation
    #x.late$complete.weeks <- 999
    x <- rbind.data.frame(x.early, x.late)
    x$wk26 <- NULL
  }
  
  #Combine first and second weeks of observation
  x$complete.weeks[x$complete.weeks==1] <-2
  
  x$new.reports <- x$N_deaths
  x$first.week.measured<- x$complete.weeks[1]
  if(nrow(x)>1){
    
    #If first observation we have for a week s after week 2, put in the 'most complete' column 999
    if(x$complete.weeks[1]>2){
      x$complete.weeks[1] <-999
    }
    
    for(i in 2:nrow(x)){
      x$new.reports[i]<-x$N_deaths[i]-x$N_deaths[(i-1)]
    }
  }
  return(x)
}) 

compare.m.alt.spl2 <-
  do.call(rbind.data.frame,compare.m.alt.spl)

#replace negative values with 0s
compare.m.alt.spl2$new.reports <- ifelse(compare.m.alt.spl2$new.reports<0, 0, compare.m.alt.spl2$new.reports)

us.test <- compare.m.alt.spl2 %>% 
  filter(state=="US")

#### 5. Model time ----

source("functions/jags_negbin4.R")

jags.func <- function(state.select){
  ds.select <- compare.m.alt.spl2[compare.m.alt.spl2$state==state.select,]
  ds.select$complete.weeks[ds.select$complete.weeks>26] <-999
  ds.select$first.week.measured[ds.select$first.week.measured>26] <-26
  ds7 <- acast( ds.select[,c('death_date','complete.weeks',"new.reports", 'first.week.measured' )],  death_date ~ complete.weeks, value.var="new.reports" , fun.aggregate = sum)
  ds7.length <- acast( ds.select[,c('death_date','complete.weeks',"new.reports", 'first.week.measured' )],  death_date ~ complete.weeks, value.var="new.reports" , fun.aggregate = length)
  #If we don't have observation, set to NA
  ds7[ds7.length==0] <-NA
  #what is first week for which we have a report for each state? For dates where we don't have >26 weeks of data and don't observed the first week, we need to sum th eprobabilities (betas)
  first.measured.date <- as.data.frame(unique(ds.select[,c('death_date','first.week.measured')]))
  max.measured <- max(first.measured.date$first.week.measured[first.measured.date$first.week.measured!=999])
  first.measured.date$first.week.measured[first.measured.date$first.week.measured==999] <- max.measured+1
  #get rid of columns where we don't have a meausurement
  #ds7 <- ds7[,c(1:max.measured, dim(ds7)[2])]
  st1 <- ds7
  date.sum <- apply(st1,1,sum, na.rm=T) #total observations for 
  death_date <- as.Date(dimnames(st1)[[1]])
  death_yr <- year(death_date)
  death_week <- week(death_date)
  death_epiyr <- death_yr
  death_epiyr[death_week<=26] <- 
    death_yr[death_week<=26]-1
  death_epiyr.index <-as.numeric(as.factor(death_epiyr))
  st2 <- st1[, -ncol(st1)] #remove column '99'
  beta.priors <- rep(0.1, times=(ncol(st2)))
  reporting.triangle <- st1
  reporting.triangle[reporting.triangle[,1]>0, ncol(reporting.triangle)] <-NA
  max_D <- ncol(reporting.triangle)-1
  ##############################################################
  #Model Fitting
  ##############################################################
  inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
  inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
  inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')
  ##############################################
  #Model Organization
  ##############################################
  model_spec<-textConnection(model_string_negbin4)
  model_jags<-jags.model(model_spec, 
                         inits=list(inits1,inits2, inits3),
                         data=list('n.dates' =
                                     nrow(reporting.triangle),
                                   'n' = reporting.triangle,
                                   'D' = ncol(reporting.triangle)-1,
                                   
                                   
                                   alphat.shape.prior=0.001,
                                   alphat.rate.prior=0.001,
                                   'N.first.obs'=(first.measured.date$first.week.measured-1),
                                   'beta.priors'=beta.priors
                                   
                         ),
                         n.adapt=5000, 
                         n.chains=3)
  params<-c('sum.n','sum.lambda',
            'beta.logged', 'alpha','sum.beta')
  ##############################################
  #Posterior Sampling
  ##############################################
  posterior_samples<-coda.samples(model_jags, 
                                  params, 
                                  n.iter=5000)
  posterior_samples.all<-do.call(rbind,posterior_samples)
  #post1.summary<-summary(posterior_samples)
  #post_means<-colMeans(posterior_samples.all)
  out.list=list('posterior_samples.all'=posterior_samples.all,'date.sum'=date.sum,'death_date'=death_date)
  return(out.list)
}

# SKIP TO LINE 281 TO GET MODEL RESULTS
# OTHERWISE THIS COULD TAKE A WHILE DEPENDING ON HOW MANY
# CORES OUR COMPUTER HAS

n_cores<- detectCores() -1
states.test <- unique(compare.m.alt.spl2$state)
#states.test <- c('New York', 'California', 'Louisiana', 'Georgia', 'Florida','Kentucky')
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(lubridate, quietly = TRUE)
  library(reshape2, quietly = TRUE)
  library(rjags, quietly = TRUE)
})
clusterExport(cl, c('jags.func','compare.m.alt.spl2','model_string_negbin4'), environment())
mod1<-pblapply(cl = cl,X=states.test,FUN=jags.func)
stopCluster(cl)
names(mod1) <- states.test
saveRDS(mod1,'data/jags_results/mod1.rds')

## Skip to here if you don't want to run the model
mod1<- readRDS('data/jags_results/mod1.rds')
jags_extract <- function(ds){
  posterior_samples.all <- ds$posterior_samples.all
  
  death_date <- ds$death_date
  
  date.sum <- ds$date.sum
  
  post_means<-apply(posterior_samples.all, 2, median)
  sample.labs<-names(post_means)
  
  ci<-t(hdi(posterior_samples.all, credMass = 0.95))
  ci<-matrix(sprintf("%.1f",round(ci,1)), ncol=2)
  row.names(ci)<-sample.labs
  post_means<-sprintf("%.1f",round(post_means,1))
  names(post_means)<-sample.labs
  
  #pred.index <- grep('sum.lambda',sample.labs)
  pred.index2 <- grep('sum.n',sample.labs)
  
  beta.index <- grep('beta.logged',sample.labs)
  alpha.index <- grep('alpha',sample.labs)
  baseline.index <- grep('baseline.n',sample.labs)
  sum.lambda.index <- grep('sum.lambda',sample.labs)
  
  pred.means <- post_means[pred.index2]
  lambda.means <- post_means[sum.lambda.index]
  
  lambda.samps <-
    t(posterior_samples.all[,sum.lambda.index])
  pred.samps <-
    t(posterior_samples.all[,pred.index2])
  
  #divide obs and predicted
  complete.prop.est.iter <- 
    apply(pred.samps,2,function(x) date.sum/x) 
  
  complete.prop.est.iter <- 
    cbind.data.frame('n.weeks.since.death'=(nrow(complete.prop.est.iter)+1):2, complete.prop.est.iter)
  
  beta.log.means <- as.numeric(post_means[beta.index])
  probs <- exp(beta.log.means)
  probs.samps <- posterior_samples.all[,beta.index]
  alpha <- as.numeric(post_means[alpha.index])
  
  pred.ci <- ci[pred.index2,]
  all.preds<- as.numeric(as.character(cbind(pred.means, pred.ci)))
  all.preds<- as.data.frame(matrix(all.preds,ncol=3))
  names(all.preds) <- c('pred.med','pred.lcl','preds.ucl')
  
  all.preds <- cbind.data.frame(all.preds, 'obs'=date.sum, 'death_date'=death_date)
  
  out.list=list('preds'=all.preds,'probs'=probs,'probs.samps'=probs.samps ,'lambda.samps'=lambda.samps,'complete.prop.est.iter'=complete.prop.est.iter)
  return(out.list)
}
res1 <- lapply(mod1, jags_extract)
all.preds1 <- lapply(res1,'[[', 'preds')
complete.prop.est.iter <- lapply(res1,'[[', 'complete.prop.est.iter')
for(i in 1:length(all.preds1)){
  all.preds1[[i]]$state <- names(all.preds1)[i]
  complete.prop.est.iter[[i]]$state <- names(all.preds1)[i]
}
all.preds.df <- do.call('rbind.data.frame', all.preds1)
complete.prop.est.iter.df <- do.call('rbind.data.frame', complete.prop.est.iter)


write.csv(all.preds.df,'data/outputs/NobBs.preds.csv')
saveRDS(complete.prop.est.iter.df,'data/outputs/NobBs.complete.prop.est.iter.df.rds')
prop.report.wk <- sapply(res1,'[[', 'probs')
complete.wk <- apply(prop.report.wk, 2,cumsum)
complete.wk <- cbind.data.frame('week'=2:(nrow(complete.wk)+1), complete.wk)
write.csv(complete.wk,'data/outputs/NobBs.complete.csv')

prop.report.wk.iter <- sapply(res1,'[[', 'probs.samps', simplify='array')
complete.wk.iter <- apply(prop.report.wk.iter, c(1,3),function(x) cumsum(exp(x)))
dimnames(complete.wk.iter)[[1]] <- 2:(dim(complete.wk.iter)[1]+1)
saveRDS(complete.wk.iter,'data/outputs/NobBs.complete.iters.rds')
