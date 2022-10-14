
library(AnchovetaMSE)
library(RPC)
setwd("G:/Shared drives/BM shared/1. Projects/Anchoveta - Pew/")

MSE<-readRDS("MSE runs/MSE_1.rda")
MSE<-Sub(MSE,MPs=MSE@MPs[2:MSE@nMPs])
MPcols=c(rep("black",4),rep("#00ff0090",5),rep("#ff000090",6))


jpeg("Figures/RCBproj.jpg",res=600,width=12,height=10.5,units="in")
  TSplot(MSE,MPcols=MPcols)
dev.off()

jpeg("Figures/RCCproj.jpg",res=600,width=12,height=10.5,units="in")
  TSplot(MSE,slot="Catch",name="Catch (Mt)",MPcols=MPcols)
dev.off()

MSEzero<-readRDS("MSE runs/MSEzero.rda")

jpeg("Figures/RCzeroC_SSB0.jpg",res=600,width=8,height=5.5,units="in")
  stoch_plot(MSEzero, MSEzero@MPs[1], qval = 0.9, type="SSB0")#type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch"))
dev.off()

jpeg("Figures/RCFref50_SSB0.jpg",res=600,width=8,height=5.5,units="in")
   stoch_plot(MSEzero, MSEzero@MPs[2], qval = 0.9, type="SSB0")#type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch"))
dev.off()


jpeg("Figures/RCHS50_SSB0.jpg",res=600,width=8,height=5.5,units="in")
   stoch_plot(MSE, "PA_HS_50", qval = 0.9, type="SSB0")#type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch"))
dev.off()


stoch_plot(MSEzero, MSEzero@MPs[3], qval = 0.9, type="SSB0")#type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch"))






  jpeg("Figures/RCTOplot.jpg",res=600,width=12,height=6.5,units="in")
  PA_TOplot(MSE,MPcols=MPcols)
  dev.off()
