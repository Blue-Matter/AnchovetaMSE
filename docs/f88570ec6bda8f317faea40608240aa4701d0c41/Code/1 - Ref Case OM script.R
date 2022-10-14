
library(openMSE)

setwd("G:/Shared drives/BM shared/1. Projects/Anchoveta - Pew/OM Specification")
OM = new('OM')

# Misc 
OM@Name = "Peruvian Anchoveta"
OM@Agency = "Peruvian Marine Research Institute (IMARPE)"
OM@Region = "Peru"
OM@Sponsor = "Pew Charitable Trusts"
OM@Latitude = -10
OM@Longitude = -80
OM@nsim = 200
OM@proyears = 30
OM@interval = 1
OM@pstar = 0.5
OM@maxF = 3
OM@reps = 1
OM@cpars =list() # see below
OM@seed = 1
OM@Source = "https://blue-matter.github.io/AnchovetaMSE"

# Stock
OM@Common_Name = "Peruvian Anchoveta"
OM@Species = "Engraulis ringens"
OM@maxage = 5 # this is really 3 or 4 but 8 is used for calculations
OM@R0 = rep(2.067, 2) # place holder Perea et al 2011 see Figure
OM@M = rep(0.8, 2) # Overwritten by cpars. Ñiquen et al. 2000, IMARPE 2010
OM@h = c(0.95, 0.95) # to be updated post RCM
OM@SRrel = 1 # Beverton Holt
OM@Perr = c(0.4, 0.8) # to be updated by RCM
OM@AC = c(0, 0.8) # to be updated by RCM
OM@Linf = rep(20.5, 2) # overwritten by cpars. Jordán 1976, IMARPE 2010, Perea et al. 2011
OM@K = rep(0.0717*12, 2) # Palomares et al. 1987, IMARPE 2010
OM@t0 = rep(-0.14, 2) # IMARPE 2010
OM@L50 = rep(11, 2) # overwritten by cpars. assuming L95 of 12cm Jordán 1976, Sharp and Csirke 1983, Pauly et al. 1989
OM@L50_95 = rep(1, 2) # most mature at 12.0cm 

# Custom Parameters
#OM@cpars$R0 = rlnorm(OM@nsim,0.726,0.0749) # Perea et al. default dataset.xlsx
OM@cpars$M = rlnorm(OM@nsim,log(0.8),0.1) # Ñiquen et al. 2000, IMARPE 2010),
OM@cpars$Linf = rlnorm(OM@nsim,log(20.5),0.05) # assumed 5% log CV Jordán 1976, IMARPE 2010, Perea et al. 2011
OM@cpars$L50 = rlnorm(OM@nsim,log(11),0.1) # assumed 10% CV on inflection point of maturity Jordán 1976, Sharp and Csirke 1983, Pauly et al. 1989

# no time varying dynamics
OM@Ksd=rep(0, 2)
OM@Linfsd = rep(0, 2)
OM@Msd = rep(0, 2) # place holder annual variability in M
OM@LenCV = rep(0.125, 2)
OM@D=rep(0.8,2) # to be updated by RCM
OM@a=0.00000512 # kg Cárdenas 2015
OM@b=3.074  # Cárdenas 2015
OM@Size_area_1=rep(0.5, 2)
OM@Frac_area_1=rep(0.5, 2)
OM@Prob_staying=rep(0.5, 2)
OM@Fdisc = rep(1, 2)

# Fleet
OM@nyears = 45 # 1964 - 2008
OM@CurrentYr = 2008
OM@EffYears=c(1, 45)    # to be updated by RCM
OM@EffLower=rep(0.9, 2) # to be updated by RCM
OM@EffUpper=rep(1, 2)   # to be updated by RCM
OM@Esd = c(0, 0)        # to be updated by RCM
OM@qinc = c(0, 0)       # no catchability increases
OM@qcv = c(0, 0)        # no interannual variability in catchability
OM@L5=c(12, 12)
OM@LFS=c(14, 14)
OM@Vmaxlen=c(1, 1)
OM@isRel=F
OM@Rmaxlen = c(1, 1)
OM@DR=0
OM@Spat_targ=1
OM@MPA=F

#Observation
OM@Cobs<-rep(0.025, 2)
OM@Cbiascv<-rep(0, 2)
OM@CAA_nsamp<-rep(100, 2)
OM@CAA_ESS<-rep(100, 2)
OM@CAL_nsamp<-rep(100, 2)
OM@CAL_ESS<-rep(100, 2)
OM@Iobs<-rep(0.15, 2)
OM@Btobs<-rep(0.15, 2)
OM@Dobs<-rep(0.2, 2)
OM@Eobs<-rep(0.1, 2)
OM@Btbiascv<-OM@LenMbiascv<-OM@Mbiascv<-OM@Kbiascv<-OM@t0biascv<-OM@Linfbiascv<-OM@LFCbiascv<-OM@LFSbiascv<-OM@FMSY_Mbiascv<-OM@BMSY_B0biascv<-OM@Irefbiascv<-OM@Brefbiascv<-OM@Crefbiascv<-OM@Dbiascv<-OM@hbiascv<-OM@Recbiascv<-OM@sigmaRbiascv<-OM@Ebiascv<-rep(0, 2)
OM@beta<-rep(1, 2)

#Implementation
OM@TACFrac=rep(1,2)
OM@TACSD=rep(0,2)
OM@TAEFrac=rep(1,2)
OM@TAESD=rep(0,2)
OM@SizeLimFrac=rep(1,2)
OM@SizeLimSD=c(0,2)

#OM2<-LH2OM(OM)
#OMinit(name="Peruvian_Anchoveta_2",files='rmd')

# Data
dat<-new('RCMdata')
raw<-read.csv("../Data/Extracted survey and catches/Extracted.csv",header=F)
ny<-nrow(raw)
ny==OM@nyears
dat@Chist<-raw[,3]*1E9
dat@C_sd<-rep(0.025,ny)
dat@Index<-raw[,2]*1E9
dat@I_sd<-rep(0.1,ny)

dat2<-new('Data')
dat2@Cat<-array(rep(raw[,3],each=OM@nsim),c(OM@nsim,ny))
dat2@CV_Cat<-array(0.025,dim(dat2@Cat))
dat2@Ind<-array(rep(raw[,2],each=OM@nsim),c(OM@nsim,ny))
dat2@CV_Ind<-array(0.1,dim(dat2@Ind))
dat2@CAL_bins<-dat2@CAL_mids<-0

setup()
out<-RCM(data=dat, OM=OM ,drop_nonconv = T, drop_highF=T, cores=8, mean_fit=T)
output<-plot(out, compare=T)
file.copy(output, "RCM.html",overwrite=T)

OM<-out@OM
OMdoc(OM)

saveRDS(out,"CondOM.rda")

# test 
OMs<-SubCpars(OM,sims=1:6)
test<-runMSE(OMs,extended=T)
matplot(t(test@PPD[[1]]@AddInd[,1,]),type="l")
test@Hist@SampPars$Obs$AddInd_Stat

# === End of script ================================================================
