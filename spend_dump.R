require(BTYD)
ave.spend <- cdnowSummary$m.x
tot.trans <- cdnowSummary$cbs[,"x"]
# There will be many warnings due to the zeroes that are
# included in the data above. To avoid them, use the following:
# (see example for spend.LL)
ave.spend <- ave.spend[which(tot.trans > 0)]
tot.trans <- tot.trans[which(tot.trans > 0)]
# We will let the spend function use default starting parameters
spend.EstimateParameters(ave.spend, tot.trans)
spend.expected.value(params, m.x, x)

for(i in seq(1,40)){
  startdate[i]=(T.cal+i)
  p.alive[i]<-mbgcnbd.PAlive(params1, x,t.x,T.cal = startdate[i])
  if(p.alive[i]<1e-06)
    break;
}
T.alive=T.cal+i-1

params.bgnbd <- BTYD::bgnbd.EstimateParameters(cal.cbs1) # BG/NBD
params.bgcnbd <- bgcnbd.EstimateParameters(cal.cbs1) # BG/CNBD-k
params.mbgnbd <- mbgnbd.EstimateParameters(cal.cbs1) # MBG/NBD
params.mbgcnbd <- mbgcnbd.EstimateParameters(cal.cbs1) # MBG/CNBD-k
row <- function(params, LL) {
  names(params) <- c("k", "r", "alpha", "a", "b")
  c(round(params, 3), LL = round(LL))
}
#print LLs to compare
rbind(`BG/NBD` = row(c(1, params.bgnbd),
                     BTYD::bgnbd.cbs.LL(params.bgnbd, cal.cbs1)),
      `BG/CNBD-k` = row(params.bgcnbd,
                        bgcnbd.cbs.LL(params.bgcnbd, cal.cbs1)),
      `MBG/NBD` = row(params.mbgnbd,
                      mbgcnbd.cbs.LL(params.mbgnbd, cal.cbs1)),
      `MBG/CNBD-k` = row(params.mbgcnbd,
                         mbgcnbd.cbs.LL(params.mbgcnbd, cal.cbs1)))

exptrans<-mbgcnbd.ConditionalExpectedTransactions(params.mbgcnbd,T.star = 52, x = 1:5, t.x = 12, T.cal = 52)

params<-mbgcnbd.EstimateParameters(cal.cbs)
mbgcnbd.LL(params,cal.cbs$x,cal.cbs$t.x,cal.cbs$T.cal,cal.cbs$litt)
mbgcnbd.cbs.LL(params,cal.cbs)

params.mcmcpggg <- pggg.mcmc.DrawParameters(cal.cbs1)

m.x<-cal.cbs1$sales/(cal.cbs1$x+1)
spend.params <- spend.EstimateParameters(m.x, cal.cbs1$x + 1)
cal.cbs1$sales.avg.est <- BTYD::spend.expected.value(spend.params, m.x, cal.cbs1$x + 1)
cal.cbs1$sales.mbgcnbd <- cal.cbs1$sales.avg.est * output3$round.predictedtxns.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...31...
c("Estimated Sales" = sum(cal.cbs1$sales.mbgcnbd),"Actual Sales"    = sum(cal.cbs1$sales.star))

predictedtxns<-data.frame(mbgcnbd.ConditionalExpectedTransactions(params1, T.star=52,cal.cbs1$x, cal.cbs1$t.x,cal.cbs1$T.cal))
output4<-data.frame(cal.cbs1$cust,round(predictedtxns$mbgcnbd.ConditionalExpectedTransactions.params1..T.star...52..))
cal.cbs1$sales.in.2018<-cal.cbs1$sales.avg.est*(output4$round.predictedtxns.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...52...)
write.csv(cal.cbs1,"Expected Spend.csv")

params.mcmcpggg <- pggg.mcmc.DrawParameters(cal.cbs1)
xstar.draws <- mcmc.DrawFutureTransactions(cal.cbs1, params.mcmcpggg,T.star=31)
outputmcmc<-data.frame(colMeans(xstar.draws))
outputmcmc<-cbind(cal.cbs1$cust,outputmcmc)
write.csv(outputmcmc,"Output MCMC.csv")
j=1
sales.mbgcnbd.upd.q<-array(0,dim=c(length(cal.cbs1$cust),200))
for (i in seq(0.01,2,by=0.01)){
  spend.params1[2]<-i
  sales.mbgcnbd.upd.q[,j] <- (BTYD::spend.expected.value(spend.params1, m.x, cal.cbs1$x + 1)* output4$round.predictedtxns.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...52...)
  j=j+1
}
max(colSums(sales.mbgcnbd.upd.q))

for(i in 1:length(cal.cbs1$x)){
  if(cal.cbs1$sales.in.2018[i]<0){
    cal.cbs1$sales.in.2018[i]=52*(output4$round.predictedtxns.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...52...[i])
  }
}

#checked by replacing q, but looks like replacing negative average spend by m.x suffices; tinkering with gamma parameters not needed
#spend.params1[2]<-0.78
cal.cbs1$sales.avg.est <- BTYD::spend.expected.value(spend.params, m.x, cal.cbs1$x + 1)
for(i in 1:length(cal.cbs1$x)){
        if(cal.cbs1$sales.avg.est[i]<0){
              cal.cbs1$sales.avg.est[i]=m.x[i]
        }
}
#output3 is predicted txns in 01jan-31jul'18
cal.cbs1$sales.mbgcnbd <- cal.cbs1$sales.avg.est * output3$round.predictedtxns.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...31...
c("Estimated Sales" = sum(cal.cbs1$sales.mbgcnbd),"Actual Sales"    = sum(cal.cbs1$sales.star))
cal.cbs1$sales.in.2018 <- cal.cbs1$sales.avg.est * output4$round.predictedtxns.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...52...
sum(cal.cbs1$sales.in.2018)

clients2018<-mylog[which(mylog$AcOpeningDate > end.of.cal.period),]
clients2018<-data.frame(cbind(clients2018$AcCode,clients2018$Month,clients2018$TxnValDiscounted))
colnames(clients2018)<-c("cust","date","sales")
clients2018$date<-as.Date(as.POSIXct(clients2018$date,format="%Y-%m-%d",tz="GMT"))
clients2018$sales<-as.numeric(as.character(clients2018$sales))
cbs2018<-elog2cbs(clients2018,units="week",T.cal="2018-7-31",
                  T.tot="2019-03-31")

txns2018<-data.frame(mbgcnbd.ConditionalExpectedTransactions(params1,
        T.star=cbs2018$T.star,x=cbs2018$x, t.x=cbs2018$t.x,T.cal=cbs2018$T.cal))
output2018<-data.frame(cbs2018,round(
  txns2018$mbgcnbd.ConditionalExpectedTransactions.params1..T.star...cbs2018.T.star..))
m.x2018<-cbs2018$sales/(cbs2018$x+1)
cbs2018$sales.avg.est <- BTYD::spend.expected.value(
                                spend.params, m.x2018, cbs2018$x + 1)
for(i in 1:length(cbs2018$x)){
     if(cbs2018$sales.avg.est[i]<0|cbs2018$sales[i]<cbs2018$sales.avg.est[i]){
           cbs2018$sales.avg.est[i]=m.x2018[i]
       }
}
cbs2018$sales.mbgcnbd <- cbs2018$sales.avg.est *
  output2018$round.txns2018.mbgcnbd.ConditionalExpectedTransactions.params1..T.star...cbs2018.T.star...
sum(cbs2018$sales.mbgcnbd)
cbs2018$sales.in.2018<-cbs2018$sales+cbs2018$sales.mbgcnbd
sum(cbs2018$sales.in.2018)
sum(cbs2018$sales.in.2018)+sum(cal.cbs1$sales.in.2018)
sum(cbs2018$sales.in.2018)*12/7+sum(cal.cbs1$sales.in.2018)
