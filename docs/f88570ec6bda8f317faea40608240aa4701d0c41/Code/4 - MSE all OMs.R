# Run Anchoveta MSEs

library(AnchovetaMSE)
setwd("G:/Shared drives/BM shared/1. Projects/Anchoveta - Pew/MSE runs")


doMSE<-function(x,OMlist){
  OM<-SubCpars(OMlist[[x]],sims=1:48)
  test2<-runMSE(OM,extended=T,Hist=T)
  test2@SampPars$Obs$AddInd_Stat[[1]][,2]<-1E-5
  Project(test2,MPs=c(paste0("PA_Ref_",c(15,20,25,30,35)),
                              paste0("PA_HS_",c("01","05","10","25","50")),
                              paste0("PA_HS_L",5:10)),extended=T,parallel=T)
}

OMlist<-list()
for(i in 1:7)OMlist[[i]]<-get(paste0("PA_OM_",i))

for(i in 1:7){
  MSE<-doMSE(1,OMlist)
  saveRDS(MSE,paste0("MSE_",i,".rda"))
}

# Zero catch projection

MSEzero<-runMSE(PA_OM_1,MPs=c("NFref","FMSYref50","FMSYref"))
saveRDS(MSEzero,"MSEzero.rda")


# ====
setup(cpus=7)
sfLibrary(AnchovetaMSE)
sfExport(list=c("doMSE","OMlist"))
sfExport(list=c(paste0("PA_Ref_",c(15,20,25,30,35)),
                paste0("PA_HS_",c("01","05","10","25","50")),
                paste0("PA_HS_L",5:10)),namespace="AnchovetaMSE")

MSEs<-sfLapply(x=1:7,doMSE,OMlist=OMlist)
saveRDS(MSEs,"MSEs.rda")
