
library(openMSE)

setwd("G:/Shared drives/BM shared/1. Projects/Anchoveta - Pew/OM Specification")

out<-readRDS("CondOM.rda")
OM<-out@OM


# --- 1 --- Reference Case -----------------------------------------------------------------

OM_1<-OM
OM_1@Name<-"OM #1: Reference Case"
saveRDS(OM_1,"OM_1.rda")

# --- 2 --- Steepness h=0.85 ----------------------------------------------------------------

OM_2<-OM
OM_2@Name<-"OM #2: Lower resilience"
OM_2@cpars$hs[]<-0.85
saveRDS(OM_2,"OM_2.rda")

# --- 3 --- M var log normal CV 0.2 ---------------------------------------------------------

OM_3<-OM
OM_3@Name<-"OM #3: Higher M variability"
dims<-dim(OM_3@cpars$M_ageArray)
Mmult<-array(trlnorm(prod(dims[c(1,3)]),1,0.2),dims[c(1,3)])
mind<-as.matrix(expand.grid(1:dims[1],1:dims[2],1:dims[3]))
OM_3@cpars$M_ageArray[mind]<-Mmult[mind[,c(1,3)]]
saveRDS(OM_3,"OM_3.rda")

# --- 4 --- 1% Increase in M ---------------------------------------------------------------- 

OM_4<-OM
OM_4@Name<-"OM #4: 1% increase in M"
pind<-OM@nyears+(1:OM@proyears)
Mmult<-array(rep(1.01^(1:length(pind)),each=prod(dims[1:2])),c(dims[1:2],length(pind)))
OM_4@cpars$M_ageArray[,,pind]<-OM_4@cpars$M_ageArray[,,pind]*Mmult
saveRDS(OM_4,"OM_4.rda")

# --- 5 --- 1% Decrease in M ---------------------------------------------------------------- 

OM_5<-OM
OM_5@Name<-"OM #4: 1% decrease in M"
Mmult<-array(rep(0.99^(1:length(pind)),each=prod(dims[1:2])),c(dims[1:2],length(pind)))
OM_5@cpars$M_ageArray[,,pind]<-OM_5@cpars$M_ageArray[,,pind]*Mmult
saveRDS(OM_5,"OM_5.rda")

# --- 6 and 7 --- low and high recruitment --------------------------------------------------

getstagmu<-function(x,period=10){
  nvals<-length(x)-period+1
  mus<-rep(NA,nvals)
  for(i in period:length(x))mus[i-period+1]<-mean(x[(i-period+1):period])
  mus
}

OM_6<-OM_7<-OM
OM_6@Name<-"OM #6: low future recruitment"
OM_7@Name<-"OM #7: high future recruitment"

pind<-OM@nyears+(1:OM@proyears)
muy<-apply(OM_6@cpars$Perr_y,1,getstagmu)
maxs<-apply(muy,2,max)
mins<-apply(muy,2,min)
mus<-apply(OM_6@cpars$Perr_y[,pind],1,mean)

mults<-array(mins/mus,c(OM_6@nsim,OM_6@maxage+1,length(pind)))
OM_6@cpars$M_ageArray[,,pind]<-OM_6@cpars$M_ageArray[,,pind]*mults
saveRDS(OM_6,"OM_6.rda")

mults<-array(maxs/mus,c(OM_7@nsim,OM_7@maxage+1,length(pind)))
OM_7@cpars$M_ageArray[,,pind]<-OM_7@cpars$M_ageArray[,,pind]*mults
saveRDS(OM_7,"OM_7.rda")


# === End of script ================================================================
