library(cdcfluview)

source("functions/archive.R")

extrap.date <- as.Date("2020-01-26")
count.start.date <- as.Date("2020-03-01")
end.data.date <- as.Date("2020-05-09")
last.date.format <- 
  format(end.data.date, '%b %d, %Y')

exclude.states <- c('CT', 'NC', 'PR') #no up-to-date number


params <- data.frame(n.days.filter=20, agg.level="state")


state.name2 <- c(state.name, 'District of Columbia','Puerto Rico', 'United States', 'New York City')
state.abb2 <- c(state.abb, 'DC','PR','US','NYC')


state.abbr <- c(state.abb, "DC", "NYC", "PR", "US")
state.names <- c(state.name, "District of Columbia", "New York City", "Puerto Rico", "United States")
state_names <- data.frame(state.abbr, state.names) %>% 
  mutate(state.abbr=as.character(state.abbr),
         state.names=as.character(state.names))

today <- as.character(Sys.Date())

#cross walk file to map states to hhs regions

hhs_states <- cdcfluview::hhs_regions
hhs_states <- hhs_states %>% 
  left_join(state_names, by=c("state_or_territory"="state.names")) %>% 
  rename(state=state.abbr) %>% 
  mutate(state_region=region) %>% 
  # might need to do collapse here for grouping later
  mutate(state_region.lab=state) %>% 
  select(state, state_region) %>% 
  # not dealing with regions
  mutate(state_region=state)

#Get rid of states that have no data in recent time points
hhs_states.cw <- hhs_states %>% 
  #filter(state %in% pi.data.not.miss.states)
  filter(!state %in% exclude.states)

if(params$agg.level== 'state_region'){
  hhs_states.cw.spl <- split(hhs_states.cw, 
                             hhs_states.cw$state_region)
}else{
  hhs_states.cw.spl <- split(hhs_states.cw, 
                             hhs_states.cw$state)
}


hhs_states.cw <-do.call('rbind.data.frame',hhs_states.cw.spl)

hhs_states.cw <- hhs_states.cw %>% 
  mutate(state=as.character(state),
         state_region=as.character(state_region),
         state_region.lab=state_region)


nyc.state <- as.data.frame(matrix(c('NYC','NYC','NYC'), nrow=1 ))
names(nyc.state) <- names(hhs_states.cw)
hhs_states.cw <- rbind.data.frame(hhs_states.cw,nyc.state)



#### download NREVSS data ----
#download the NREVSS data

nrevvs.state <- cdcfluview::who_nrevss(region = c("state"))
clin <- nrevvs.state$clinical_labs

# ARCHIVE
write_csv(clin, paste0("data/archive/nrevss.state_", today, ".csv"), na="")

clin <- clin %>% 
  left_join(state_names, by=c("region"="state.names")) %>% 
  rename(state=state.abbr) 

cw.file <- cdcfluview::hhs_regions

# !!!

hhs_states.cw$state_region <- hhs_states.cw$state

# this brings over file with missing states like CT and DE
clin2 <- left_join(clin, hhs_states.cw, by="state") %>% 
  #filter(!is.na(state_or_territory)) %>% 
  filter(!is.na(state_region)) %>% 
  #rename(region=region.x) %>% 
  inner_join(cw.file, by=c("region"="state_or_territory")) %>% 
  #clin2.subsetvars <- clin2 %>%
  select(state=region, hhs_region=region_number, year, week,
         wk_date, total_a, total_b, total_specimens,
         state_region) %>% 
  mutate(total_specimens=as.numeric(total_specimens),
         total_a=as.numeric(total_a),
         total_b=as.numeric(total_b)) #%>% 
  #group_by(state_region, wk_date, hhs_region) %>% 
  #summarize(total_specimens=sum(total_specimens, na.rm=T),
  #          total_a=sum(total_a, na.rm=T),
  #          total_b=sum(total_b, na.rm=T)) %>% 
  #rename(state=state_region)

##Florida doesn't have ILI data, so use regions ILI dat

nrevvs_hhs <- cdcfluview::who_nrevss(region = c("hhs"))
clin.hhs <- nrevvs_hhs$clinical_labs

# ARCHIVE
write_csv(clin.hhs, paste0("data/archive/nrevss.hhs_", today, ".csv"), na="")


clin.hhs <- clin.hhs %>% 
  select(region, wk_date, total_a, total_b, total_specimens) %>% 
  mutate(region = as.numeric(gsub("Region ", "", region))) %>% 
  select(hhs_region=region, wk_date, hhs_total_a=total_a, 
         hhs_total_b=total_b, hhs_total_specimens=total_specimens)

clin3 <- clin2 %>%
  left_join(clin.hhs, by = c("hhs_region", "wk_date"))  %>% 
  mutate(total_specimens=as.numeric(total_specimens),
         total_a=as.numeric(total_a),
         total_b=as.numeric(total_b),
         hhs_total_specimens=as.numeric(hhs_total_specimens),
         hhs_total_a=as.numeric(hhs_total_a),
         hhs_total_b=as.numeric(hhs_total_b)) %>% 
  mutate(
    total_specimens=case_when(
      is.na(total_specimens) ~ hhs_total_specimens,
      #total_specimens==0 ~ hhs_total_specimens,
      TRUE ~ total_specimens
    ),
    total_a=case_when(
      is.na(total_a) ~ hhs_total_a,
      TRUE ~ total_a
    ),
    total_b=case_when(
      is.na(total_b) ~ hhs_total_b,
      TRUE ~ total_b
    )) %>% 
  mutate(flu_pct_adj=(total_a+total_b)/total_specimens)

clin4 <- clin3 %>% 
  ungroup() %>% 
  select(state, flu_pct_adj, wk_date)

clin4.lag1<-clin4 %>% 
  mutate(wk_date = wk_date + days(7)) %>% 
  rename(flu_pct_adj_lag1=flu_pct_adj)

clin4.lag2<- clin4 %>% 
  mutate(wk_date = wk_date + days(14)) %>% 
  rename(flu_pct_adj_lag2=flu_pct_adj)

clin4.lags <- left_join(clin4, clin4.lag1)
clin4.lags <- left_join(clin4.lags, clin4.lag2)
clin4.lags <- clin4.lags %>% 
  filter(!is.na(flu_pct_adj_lag2)) %>% 
  select(state, wk_date, flu_pct_adj, flu_pct_adj_lag1, flu_pct_adj_lag2)

#### national flu data ----

nrevvs.natl <- cdcfluview::who_nrevss(region = c("national"))
nrevvs.natl <- nrevvs.natl$clinical_labs


# ARCHIVE
write_csv(nrevvs.natl, paste0("data/archive/nrevss.national_", today, ".csv"), na="")

nrevvs.natl <- nrevvs.natl %>%
  select(wk_date, flu_pct_adj=percent_positive) %>% 
  mutate(flu_pct_adj=flu_pct_adj/100)
# mutate(week_end = wk_date+6)

natl.lag1 <- nrevvs.natl %>% 
  mutate(wk_date=wk_date+days(7)) %>% 
  rename(flu_pct_adj_lag1=flu_pct_adj)

natl.lag2 <- nrevvs.natl %>% 
  mutate(wk_date=wk_date+days(14)) %>% 
  rename(flu_pct_adj_lag2=flu_pct_adj)

nrevvs.natl <- nrevvs.natl %>% 
  left_join(natl.lag1) %>% 
  left_join(natl.lag2) %>% 
  mutate(state="United States") %>% 
  select(state, wk_date, flu_pct_adj, flu_pct_adj_lag1, flu_pct_adj_lag2)


nrevvs.combo <-
  rbind.data.frame(nrevvs.natl,clin4.lags) %>% 
  left_join(state_names, by=c("state"="state.names")) %>% 
  select(-state) %>% 
  rename(state=state.abbr) 


#NYC duplicate
ny.nrevvs <- nrevvs.combo %>% 
  filter(state=="NY") %>% 
  mutate(state="NYC")

nrevvs.combo <- rbind.data.frame(nrevvs.combo,ny.nrevvs)
nrevvs.combo.spl <- 
  split(nrevvs.combo, nrevvs.combo$state)

nrevvs.combo.spl <- lapply(nrevvs.combo.spl, function(x){
  x<- x[order(x$wk_date),]
  month1 <- month(x$wk_date)
  cont.correct <- min(x$flu_pct_adj_lag1[x$flu_pct_adj_lag1>0], na.rm=T)/2
  x$log.flu.lag1 <- as.numeric(scale(log(x$flu_pct_adj_lag1+cont.correct )))
  log.flu.lag1.serf <- x$log.flu.lag1
  log.flu.lag1.serf[month1 %in% c(12,1,2) ] <- NA
  log.flu.lag1.serf[x$wk_date >=as.Date('2020-03-01') ] <- NA
  time <- 1:nrow(x)
  sin52 <- sin(2*pi*time/52.1775)
  sin26 <- sin(2*pi*time*2/52.1775)
  cos52 <- cos(2*pi*time/52.1775)
  cos26 <- cos(2*pi*time*2/52.1775)
  mod.flu <- lm(log.flu.lag1.serf ~ sin52 +cos52 +sin26 +cos26 )
  pred.flu <- predict(mod.flu, newdata=x)
  x$log.flu.deseasonalize <- x$log.flu.lag1-pred.flu
  x$log.flu.deseasonalize.0 <- x$log.flu.deseasonalize
  x$log.flu.deseasonalize.0[x$log.flu.deseasonalize.0<0] <-0
  return(x)
})

nrevvs.combo <- do.call('rbind.data.frame', nrevvs.combo.spl)



### Explanation of NCHS data sources
#NCHS mostly provides data based on location of death rather than location of residence. 
#For states like Florida, where there is a large population of seasonal residents, 
#this can make a big difference. 
#NCHS also info on underlying cause for specific causes like pneumonia&influenza. 
#The exception to this is the new P&I&Coronavirus data, 
#which is based on multiple-cause of death statistics CDC fluview provides data 
#on all-cause and P&I based on location of residence, and provides information 
#on multiple-cause (e.g., P&I listed anywhere on death certificate.) 
#To get a baseline for all-cause deaths, we use the NCHS all-cause data, 
#and combine this with the covidView all-cause data. 
#For excess P&I&C, we use combine the all-cause data from NCHS 
#from the baseline period with the *proportion* of deaths that are due to P&I. 
#The assumption is that this proportion is the same for deaths based on residence 
#and location of death. We then back-calculate the number of deaths that were
#P&I from this number and combine with the updated covidView data.

#Reporting delays from NobBS
delays <- readRDS('data/outputs/NobBs.complete.iters.rds')
dimnames(delays)[[3]][dimnames(delays)[[3]]=='US'] <- 'United States'

dimnames(delays)[[3]] <-
  state.abb2[match(dimnames(delays)[[3]] , state.name2)]

states2.match <- dimnames(delays)[[3]]


#commn.states <- Reduce(intersect, list(states1.match,states2.match))
#delays <- delays[,,commn.states]
delays <- delays[,1:10000,]
delays.m <- melt(delays)
names(delays.m) <- c('time.since.death','iter','state','prop')
delays.med <- apply(delays,c (1,3),median)
delays.med.m <- melt(delays.med)
delays.med.m$Var1 <- as.numeric(as.character(delays.med.m$Var1))
names(delays.med.m) <- c('weeks.since.death','state','prop.complete')
delays.med.m$state <- as.character(delays.med.m$state)

#Import the P&I data from fluview--this is used to set the historical baseline; updated weekly

#ARCHIVE

#Use NCHS new weekly counts of death by select cause data 2014-2018

nchs.base1.data <- read_csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD") 

nchs.base1.data <- nchs.base1.data %>% 
  select(state=`Jurisdiction of Occurrence`, year=`MMWR Year`,
         week=`MMWR Week`, week_end=`Week Ending Date`,
         all_cause=`All  Cause`, flag_ac=flag_allcause) %>% 
  mutate(week_end=mdy(week_end))

#Use NCHS new weekly counts of death by select cause data 2019-2020

nchs.base2.data <-  read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD") 

nchs.base2.data <- nchs.base2.data %>% 
  select(state=`Jurisdiction of Occurrence`, year=`MMWR Year`,
         week=`MMWR Week`, week_end=`Week Ending Date`,
         all_cause=`All Cause`, flag_ac=flag_allcause) 

nchs.base.comb <-
  bind_rows(nchs.base1.data,nchs.base2.data)

write_csv(nchs.base.comb, paste0("data/archive/nchs.base.comb_", today, ".csv", na=""))
          
#Import the cdcfluview data



pi.data <- pi_mortality(coverage_area='state')
# ARCHIVE
write_csv(pi.data, paste0("data/archive/pi_mortality_state_", today, ".csv"), na="")

pi.data <- pi.data %>% 
  filter(coverage_area=="state") %>% 
  select(week_end, percent_pni, region_name)

pi.data <- nchs.base.comb %>% 
  rename(region_name=state) %>% 
  full_join(pi.data) %>% 
  rename(state=region_name)

pi.data <- pi.data %>% 
  mutate(pi.fv = round(all_cause*percent_pni))

pi.data <- pi.data %>% 
  filter(week_end <= end.data.date)


pi.data.last <- pi.data %>% 
  filter(week_end == max(week_end))



last.date.avail.pi <- max(pi.data$week_end)

pi.data <- pi.data %>% 
  filter(!is.na(week_end))



pi.data.miss.states <- pi.data.last %>% 
  #filter(is.na(total_pni)) %>% 
  pull(state) %>% unique()

pi.data.not.miss.states <- pi.data.last %>% 
  select(state.names=state)

pi.data.not.miss.states <- pi.data.not.miss.states %>% 
  left_join(state_names) %>% 
  filter(!is.na(state.abbr)) %>% 
  pull(state.abbr) %>% as.character()

#official CDC data, updated daily on P&I, covid, all deaths
#Updated week days


url.cdc.covid <- "https://data.cdc.gov/resource/r8kw-7aab.json"

cdc.data <- fromJSON(url.cdc.covid)

write_csv(cdc.data, paste0("data/archive/cdc.data_", today, ".csv"), na="")

cdc.data <- cdc.data %>% 
  rename(nchs.covid.deaths=covid_deaths,
         nchs.total.deaths=total_deaths,
         percent_complete=percent_of_expected_deaths,
         nchs.pneu.death=pneumonia_deaths,
         nchs.pneum.w.covid=pneumonia_and_covid_deaths,
         nchs.flu.death=influenza_deaths,
         nchs.pic=pneumonia_influenza_or_covid_19_deaths) %>% 
  select(-footnote, -start_week)


data.vintage <- cdc.data %>% 
  pull(data_as_of) %>% 
  unique() %>% 
  substr(1,10)


cdc.data <- cdc.data %>% 
  mutate(end_week=ymd(substr(end_week, 1,10)))


most.recent <- max(cdc.data$data_as_of)

cdc.data <- cdc.data %>% 
  filter(data_as_of==most.recent) %>% 
  mutate(nchs.covid.deaths=as.numeric(nchs.covid.deaths),
         nchs.total.deaths=as.numeric(nchs.total.deaths),
         percent_complete=as.numeric(percent_complete),
         nchs.pneu.death=as.numeric(nchs.pneu.death),
         nchs.pneum.w.covid=as.numeric(nchs.pneum.w.covid),
         nchs.flu.death=as.numeric(nchs.flu.death),
         nchs.pic=as.numeric(nchs.pic))

#NCHS suppresses cells with counts of 1-9, in these instances, take diff of PIC and P (assumes flu=0)

cdc.data <- cdc.data %>% 
  mutate(nchs.covid.deaths = case_when(
    is.na(nchs.covid.deaths) ~ nchs.pic - nchs.pneu.death,
    TRUE ~ nchs.covid.deaths
  ))

cdc.summary.wk <- cdc.data %>% 
  select(state, end_week,
         nchs.covid.deaths, nchs.total.deaths,
         percent_complete, nchs.pneu.death,
         nchs.pneum.w.covid, nchs.flu.death, nchs.pic)

##aggregate cdc fluview data to national-level (excluding exclusion states, to be used for national baseline)

pi.data.exclude <- pi.data

pi.data.exclude <- pi.data.exclude %>% 
  left_join(state_names, by=c("state"="state.names")) %>% 
  select(-state) %>% 
  rename(state=state.abbr) %>% 
  filter(!state %in% exclude.states)

natl.pi.data <- pi.data.exclude %>% 
  rename(year_week_num=week) %>% 
  group_by(week_end, year_week_num) %>% 
  summarize(pi.fv=sum(pi.fv, na.rm=T),
            all_cause=sum(all_cause, na.rm=T)) %>% 
  filter(!is.na(year_week_num)) %>% 
  mutate(state="United States") %>% 
  rename(total_pni=pi.fv,
         all_deaths=all_cause)


state.pi.data <- pi.data %>% 
  mutate(year=week_end) %>% 
  select(week_end, year_week_num=week, total_pni=pi.fv, all_deaths=all_cause, state)

pi.data.combined <- 
  rbind.data.frame(state.pi.data, natl.pi.data) %>% 
  filter(week_end >= as.Date("2015-01-05"))

#Combine the fluview P&I data with the CDC's covid data
cdc.comb1 <- pi.data.combined %>% 
  rename(end_week=week_end) %>% 
  left_join(cdc.summary.wk) %>% 
  rename(week_end=end_week) %>% 
  filter(week_end <= end.data.date) %>% 
  mutate(year=year(week_end))


max.wk <- cdc.comb1 %>% 
  filter(ymd(week_end)==ymd(max(cdc.comb1$week_end))) %>% 
  pull(year_week_num) %>% unique()

wk.range <- c(10,max.wk)


# NCHS data from api only available since late-Jan; fill in with historical data from fluview P&I data

cdc.comb1$all_deaths <- ifelse(!is.na(cdc.comb1$nchs.total.deaths), cdc.comb1$nchs.total.deaths, 
                               cdc.comb1$all_deaths)
cdc.comb1$nchs.total.deaths <- ifelse(is.na(cdc.comb1$nchs.total.deaths), cdc.comb1$all_deaths,
                                      cdc.comb1$nchs.total.deaths)

cdc.comb1$total_pni <- ifelse(!is.na(cdc.comb1$nchs.pic), cdc.comb1$nchs.pic,
                              cdc.comb1$total_pni)
cdc.comb1$nchs.pic <- ifelse(is.na(cdc.comb1$nchs.pic), cdc.comb1$total_pni,
                             cdc.comb1$nchs.pic)

cdc.comb1 <- cdc.comb1 %>% 
  arrange(state, week_end)


## clean_pi_data1

cdc.comb2 <- cdc.comb1

cdc.comb2 <- cdc.comb2 %>% 
  left_join(state_names, by=c("state"="state.names")) %>% 
  filter(!state %in% exclude.states) %>% 
  select(-state) %>% 
  rename(state=state.abbr)


cdc.data.ny.separate <- cdc.comb2

#cdc.comb2$state <- gsub("NYC", "NY", cdc.comb2$state)

cdc.comb2 <- full_join(cdc.comb2, hhs_states.cw)

cdc.comb2$state_region <- ifelse(cdc.comb2$state=="US", "US", cdc.comb2$state_region)


pi.data.agg <- cdc.comb2 %>% 
  group_by(state, week_end) %>% 
  summarize(total_pni=sum(total_pni, na.rm=T),
            all_deaths=sum(all_deaths, na.rm=T),
            nchs.covid.deaths=sum(nchs.covid.deaths, na.rm=T),
            nchs.total.deaths=sum(nchs.total.deaths, na.rm=T),
            nchs.pneu.death=sum(nchs.pneu.death, na.rm=T),
            nchs.pneum.w.covid=sum(nchs.pneum.w.covid, na.rm=T),
            nchs.flu.death=sum(nchs.flu.death, na.rm=T),
            nchs.pic=sum(nchs.pic, na.rm=T))

spl1<-split(pi.data.agg, pi.data.agg$state)

min.state <- lapply(spl1, function(x){ x$miss.x<-min(x$total_pni, na.rm=T)
return(x)
})

pi.data.clean <- do.call('rbind.data.frame',min.state)

pi.data.clean <- pi.data.clean[!is.na(pi.data.clean$miss.x),]


pi.data.clean$nchs.total.deaths[pi.data.clean$week_end < as.Date('2020-02-01')] <-
  pi.data.clean$all_deaths[pi.data.clean$week_end < as.Date('2020-02-01')]

pi.data.clean$nchs.pic[pi.data.clean$week_end < as.Date('2020-02-01')] <-
  pi.data.clean$total_pni[pi.data.clean$week_end < as.Date('2020-02-01')]

nrevvs.combo$week_end <- nrevvs.combo$wk_date +days(6)

analysis.data <- full_join(pi.data.clean, nrevvs.combo) %>% 
  filter(week_end <= end.data.date) %>% 
  filter(!is.na(flu_pct_adj_lag1))


states.cdc <- unique(analysis.data$state)
states.cdc <- states.cdc[states.cdc!='US']

date.print <- max(analysis.data$week_end, na.rm=T)


#Merge in reporting delay info
analysis.data$vintage <- data.vintage

analysis.data <- analysis.data %>% 
  mutate(vintage=data.vintage,
         weeks.since.death=round(as.numeric(difftime(vintage, week_end, units="weeks")))) %>% 
  full_join(delays.med.m) %>% 
  rename(n.weeks.ago=weeks.since.death) %>% 
  mutate(prop.complete = case_when(
    is.na(prop.complete) ~ 1,
    TRUE ~ prop.complete
  )) %>% 
  filter(!is.na(all_deaths)) %>% 
  mutate(prop.complete.offset = prop.complete-0.5) %>% 
  filter(state!="US") %>% 
  as.data.frame()


### washington post deaths data


wapo_deaths <- read_csv("https://inv-covid-data-prod.elections.aws.wapo.pub/us-daily-historical/us-daily-historical-combined.csv") %>% 
  group_by(state) %>% 
  arrange(date) %>% 
  mutate(deaths_daily=lag(deaths)) %>% 
  mutate(deaths_daily=deaths-deaths_daily) %>% 
  ungroup()

week_of <- MMWRweek(wapo_deaths$date)
wapo_deaths <- cbind(wapo_deaths, week_of)

wapo_deaths <- wapo_deaths %>% 
  group_by(state, statePostal, statePost, MMWRyear, MMWRweek) %>% 
  summarize(deaths=sum(deaths_daily, na.rm=T)) 

wapo_deaths <- wapo_deaths %>% 
  ungroup() %>% 
  dplyr::select(state=statePostal, year=MMWRyear, week=MMWRweek, wapo.deaths=deaths)

analysis_weeks <- data.frame(MMWRweek(analysis.data$week_end))

cd1 <- cbind(analysis.data, analysis_weeks) %>% 
  rename(year=MMWRyear, week=MMWRweek) %>% 
  left_join(wapo_deaths) %>% 
  mutate(covid.death.hybrid=case_when(
    nchs.covid.deaths==0 & !is.na(wapo.deaths) ~ wapo.deaths,
    TRUE ~ nchs.covid.deaths
  ))


