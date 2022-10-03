

exampledat<-function(){

  library(openMSE)
  library(AnchovetaMSE)
  avail('OM')
  x<-1
  Data<-SimulatedData

}


PA_Ref<-function(x,Data, reps=1, cpF=0.2){
  ny<-length(Data@Year)-44
  VB<-sum(Data@Misc$StockPars$VBiomass_P[x,,ny,])
  Rec <- new("Rec")
  Rec@TAC<-VB*cpF
  Rec
}
class(PA_Ref)<-"MP"

PA_Ref_15<-function(x,Data,reps=1)PA_Ref(x=x,Data=Data,reps=reps,cpF=0.15)
PA_Ref_20<-function(x,Data,reps=1)PA_Ref(x=x,Data=Data,reps=reps,cpF=0.2)
PA_Ref_25<-function(x,Data,reps=1)PA_Ref(x=x,Data=Data,reps=reps,cpF=0.25)
PA_Ref_30<-function(x,Data,reps=1)PA_Ref(x=x,Data=Data,reps=reps,cpF=0.3)
PA_Ref_35<-function(x,Data,reps=1)PA_Ref(x=x,Data=Data,reps=reps,cpF=0.35)
PA_Ref_40<-function(x,Data,reps=1)PA_Ref(x=x,Data=Data,reps=reps,cpF=0.40)

class(PA_Ref_15)<-class(PA_Ref_20)<-class(PA_Ref_25)<-class(PA_Ref_30)<-class(PA_Ref_35)<-class(PA_Ref_40)<-"MP"

examplePAdat<-function(){
  test<-runMSE(PA_OM_1,MPs="PA_Ref_15",extended=T)
  Data<-test@PPD[[1]]

  #test<-runMSE(PA_OM_1,MPs=paste0("PA_Ref_",seq(15,40,by=5)))
  #test<-runMSE(PA_OM_1,MPs=c("FMSYref","FMSYref75","FMSYref50"))
  test2<-runMSE(PA_OM_1,MPs="PA_HS",extended=T)
}


PA_HS<-function(x, Data, reps=1, unit=1E6, Fmin=0.2/12, Fmax=0.45/12, M=0.8, cvM=0.1, cvB=0.05, L=5, maxF=0.35, p=0.15){
  ny<-length(Data@Year)-44
  VB<-sum(Data@Misc$StockPars$VBiomass_P[x,,ny,])/unit
  # VB<-20
  nt<-12
  Mm<-0.8/nt
  Fdec<-VBt<-VBo<-TACt<-rep(NA,nt)
  OE<-trlnorm(nt,1,0.1)
  ns<-10000

  slp3<-function(y){
    y<-y[!is.na(y)]
    y<-log(y)
    x1<-1:length(y)
    mux<-mean(x1)
    muy<-mean(y,na.rm=T)
    SS<-sum((x1-mux)^2,na.rm=T)
    (1/SS)*sum((x1-mux)*(y-muy),na.rm=T)

  }

  getcur<-function(yr,plot=F,mAss){

    if(length(yr)==1){
      return(yr)
    }else{

      y<-log(yr)
      slp<-slp3(y)*mean(y)
      x<-1:length(y)
      xint<-mean(y)-mean(x)*slp
      xtick<-c(0,1:length(y))
      ypred<-xint+xtick*slp
      curest<-exp(ypred[length(ypred)])
      if(plot){

        plot(xtick,exp(c(NA,y)),ylim=exp(range(c(y,ypred))),pch=19,xlab="Month",ylab="Survey Observation (Mt)")
        abline(h=seq(0,100,by=1),col="grey",lty=1)
        lines(xtick,exp(ypred),col="blue")
        points(xtick,exp(ypred),col="blue")
        points(length(y),curest,pch=19,cex=1.2,col="red")
        legend('topright',legend=c(paste("Z =",round(-slp,4)),
                                   paste("M =",round(mAss,4)),
                                   paste("F =",round(-slp-mAss,4)),
                                   paste("Cur Est =",round(curest,2))), text.col=c("blue","black","blue","red"))

      }
      return(curest)
    }

  }

  for(i in 1:nt){

    if(i==1) VBt[i]<-VB
    if(i>1)  VBt[i]<-VBt[i-1]*exp(-Mm-Fdec[i-1])

    VBo[i]<-VBt[i]*OE[i]
    VBsmooth<-getcur(yr=VBo[1:i],plot=F,mAss=Mm)

    Fs<--log(L/trlnorm(ns,VBsmooth,cvB))/(nt-i+1)-trlnorm(ns,Mm,cvM)
    Ftemp<-quantile(Fs,p=p)
    if(Ftemp<Fmin)Ftemp<-Fmin
    if(Ftemp>Fmax)Ftemp<-Fmax

    maxFcons<-maxF-(sum(Fdec,na.rm=T)-Ftemp)
    if(Ftemp>maxFcons)Ftemp=maxFcons

    Fdec[i]<-Ftemp
    TACt[i]<-Fdec[i]*VBt[i]*exp(-Mm/2)

  }

  Rec <- new("Rec")
  Rec@TAC<-sum(TACt,na.rm=T)
  Rec
}
class(PA_HS)<-"MP"










