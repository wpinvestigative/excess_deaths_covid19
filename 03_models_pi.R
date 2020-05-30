library(ExcessILI)
source("functions/format_table.R")

#### flu and pneumonia excess deaths

analysis.data$one <- 1
analysis.data2 <-
  analysis.data[!is.na(analysis.data$total_pni),]
set.seed(123)
excess_pi_natl <-
  excessCases(ds = analysis.data2,
              datevar       = "week_end",
              statevar      = "state",
              adj.flu       = "none",
              denom.var     = "nchs.total.deaths",
              use.syndromes = c("nchs.pic"),
              covs= c('log.flu.deseasonalize.0'),
              extrapolation.date = extrap.date ,
              sum.dates = count.start.date,
              model.type='poisson',
              time.res='week')

ds <- excess_pi_natl
dates1 <-
  ds[[1]][[1]][[1]]$date

sum.pred.iter <-    
  excessExtract(ds = ds,
                syndrome = 'nchs.pic',
                extract.quantity = "sum.pred.iter")
sum.obs <-
  excessExtract(ds = ds,
                syndrome = 'nchs.pic',
                extract.quantity = "sum.obs")
sum.obs.state <- apply(sum.obs,2,sum)
sum.cases.excess <- sapply(1:length(sum.obs.state),
                           function(x){
                             sum.obs.state[x] -  sum.pred.iter[,x,1]
                           })
sum.excess.deaths.range.pi <-t(
  apply(sum.cases.excess,2,quantile,  probs=c(0.025,0.5,0.975)))
sum.excess.deaths.range.pi <- cbind.data.frame('state'=unique(analysis.data2$state), sum.excess.deaths.range.pi)

res.pi <- format_excess_func(excess_pi_natl,'nchs.pic' )
write.csv(res.pi, 'data/outputs/p_i_natl_obs_expected.csv')


##National data is created by summing state data
sum.pred.iter.pic <-    
  excessExtract(ds = excess_pi_natl,
                syndrome = 'nchs.pic',
                extract.quantity = "sum.pred.iter")
pic.nat.obs.state <-    
  excessExtract(ds = excess_pi_natl,
                syndrome = 'nchs.pic',
                extract.quantity = "y")
pic.nat.obs <- pic.nat.obs.state[,dimnames(pic.nat.obs.state)[[2]]!='US',1]
pic.nat.obs <- apply(pic.nat.obs,1,sum)
pred.iter.pic <-    
  excessExtract(ds = excess_pi_natl,
                syndrome = 'nchs.pic',
                extract.quantity = "pred.iter")
pred.iter.pic2 <- array(pred.iter.pic,dim=c(length(dates1),10000,dim(pred.iter.pic)[2],1 ))
dimnames(pred.iter.pic2)[[3]] <- dimnames(pred.iter.pic)[[2]]
#Sum the state-level estimates to get national estimate
pred.iter.pic2.nat <-
  pred.iter.pic2[,,dimnames(pred.iter.pic2)[[3]]!='US',1]
pred.iter.pic2.nat.sum <- apply(pred.iter.pic2.nat,c(1,2),sum)
pred.iter.pic2.nat.q <- t(apply(pred.iter.pic2.nat.sum,1, quantile, probs=c(0.025,0.5,0.975)))
res.pi.nat <- cbind.data.frame('week_start'=dates1,'all_cause'=pic.nat.obs,pred.iter.pic2.nat.q)
names(res.pi.nat) <- c('week_start','obs','lpi','pred','upi')
res.pi.nat$week_end <- res.pi.nat$week_start+days(6)
res.pi.nat$unexplained.cases <- 
  res.pi.nat$obs- res.pi.nat$pred
res.pi.nat$state <- 'US.agg'
mmwr.ag1 <- mmwr_week(res.pi.nat$week_end)
res.pi.nat <- cbind.data.frame(mmwr.ag1,res.pi.nat)
sum(res.pi.nat$unexplained.cases[res.pi.nat$week_start>=params$count.start.date] )
res.pi.nat <- bind_rows(res.pi.nat, res.pi)
#sum.excess.deaths.range.pi
#national: sum state estimates
state.sum.pred.iter.pi <- sum.pred.iter.pic[,dimnames(sum.pred.iter.pic)[[2]]!='US',]
nat.sum.pred.iter.pi <- apply(state.sum.pred.iter.pi,1,sum)
obs.state2 <- cbind.data.frame(dates1,pic.nat.obs.state[,,1])
obs.state2 <- obs.state2[obs.state2$dates1 >=params$count.start.date,]
sum.obs.pi.nat <-
  sum(obs.state2[,-1] )
excess.nat.iter <-  sum.obs.pi.nat - nat.sum.pred.iter.pi
sum.excess.deaths.range.pi.nat <-
  quantile(excess.nat.iter,  probs=c(0.025,0.1,0.3,0.5,0.975))
sum.excess.deaths.range.pi <- bind_rows(sum.excess.deaths.range.pi.nat,sum.excess.deaths.range.pi)
sum.excess.deaths.range.pi$state <- as.character(sum.excess.deaths.range.pi$state)
sum.excess.deaths.range.pi$state[is.na(sum.excess.deaths.range.pi$state)] <- 'US.agg'
