mylog<-read.csv("... .csv",stringsAsFactors = F)
mylog<-(mylog[which(mylog$AcOpeningDate!="#N/A"),])
mylog1<-data.frame(cbind(mylog$AcCode,mylog$Month,mylog$TxnValDiscounted))
colnames(mylog1)<-c("cust","date","sales")
mylog1$date<-as.Date(as.POSIXct(mylog1$date,format="%Y-%m-%d",tz="GMT"))
end.of.cal.period=as.Date("2017-12-31")
mylog1.cal<-(mylog1[which(mylog1$date <= end.of.cal.period),])

#BTYD part - not the right models in this case - better for Netflix subscription type cases
split.data <- dc.SplitUpElogForRepeatTrans(mylog1.cal)
clean.elog <- split.data$repeat.trans.elog
freq.cbt <- dc.CreateFreqCBT(clean.elog)
tot.cbt <- dc.CreateFreqCBT(mylog1)
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)
birth.periods <- split.data$cust.data$birth.per
last.dates <- split.data$cust.data$last.date
cal.cbs.dates <- data.frame(birth.periods, last.dates,
                            end.of.cal.period)
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates,
                                      per="month")
params <- pnbd.EstimateParameters(cal.cbs)
LL <- pnbd.cbs.LL(params, cal.cbs)

#BTYDplus part - MBGCNBD-k best suited for this case
#also tried models with MCMC sampling - less accurate
library(BTYDplus)
#plotTimingPatterns(mylog1, n = 30, T.cal,
                  # headers = c("Past", "Future"), title = "")
mylog1$sales<-as.numeric(as.character(mylog1$sales))
mylog1.cal<-(mylog1[which(mylog1$date <= end.of.cal.period),])
mylog1.cal$sales<-as.numeric(as.character(mylog1.cal$sales))
cal.cbs1<-elog2cbs(mylog1.cal,units="weeks",T.cal="2017-12-31",T.tot="2018-07-01")
params1<-mbgcnbd.EstimateParameters(cal.cbs1)

#check with example
#x<-cal.cbs1$x[which(cal.cbs1$cust=="10123786")]
#t.x<-cal.cbs1$t.x[which(cal.cbs1$cust=="10123786")]
#T.cal<-cal.cbs1$T.cal[which(cal.cbs1$cust=="10123786")]
#mbgcnbd.PAlive(params1, x, t.x, T.cal)
#mbgcnbd.PAlive(params1, x, t.x, 190)
#mbgcnbd.ConditionalExpectedTransactions(params1, T.star=52,x, t.x, T.cal)

cbs.predict<- mbgcnbd.ConditionalExpectedTransactions(
  params = params1, T.star = 78,
  x = cal.cbs1$x, t.x=cal.cbs1$t.x, T.cal = cal.cbs1$T.cal)
predictedtxns<-data.frame(mbgcnbd.ConditionalExpectedTransactions(params1, T.star=35,cal.cbs1$x, cal.cbs1$t.x,cal.cbs1$T.cal))
which(cal.cbs1$cust=="10123111")
predictedtxns$mbgcnbd.ConditionalExpectedTransactions.params1..T.star...26..[7]
output1<-data.frame(cal.cbs1$cust,round(predictedtxns$mbgcnbd.ConditionalExpectedTransactions.params1..T.star...26..))
write.csv(output1,"Output1.csv")
#x<-zeros()
#j=2

x<-array(0,dim =length(output3$cal.cbs1.cust))
t.x<-array(0,dim =length(output3$cal.cbs1.cust))
t.start<-array(0,dim =length(output3$cal.cbs1.cust))
startdate<-array(0,dim =c(40,length(output3$cal.cbs1.cust)))
p.alive<-array(0,dim =c(40,length(output3$cal.cbs1.cust)))
actuallife<-data.frame(array(0,dim=length(explife$cal.cbs1.cust)))
for(j in 1:length(output3$cal.cbs1.cust)){
  x[j]<-cal.cbs1$x[j]
  t.x[j]<-cal.cbs1$t.x[j]
  t.start[j]<-cal.cbs1$T.cal[j]
for(i in seq(1,40)){
  startdate[i,j]=(t.start[j]+i)
  p.alive[i,j]<-mbgcnbd.PAlive(params1, x=x[j],t.x=t.x[j],T.cal = startdate[i,j])
  if(p.alive[i,j]<1e-06)
    break;
  #print 
}
  T.alive[j]=t.start[j]+i-1
 # print(p.alive[i-1,j])
 # print(T.cal[i-1,j])
}

actualtxns2018<-array(0,dim =length(mylog1.cal$cust))
for(i in 1:length(mylog1.cal$cust)){
actualtxns2018[i]<-nrow(mylog1[which(mylog1$cust==mylog1.cal$cust[i]&mylog1$date>end.of.cal.period),])
}
