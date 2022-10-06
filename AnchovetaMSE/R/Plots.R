# Performance plots

PA_TOplot<-function(MSEobj,refCMP=1,cex=0.9,xadj=6,yadj=1,MPcols=NULL){

  if(is.null(MPcols))MPcols=rep('black',MSEobj@nMPs)
  Catrel<-array(NA,dim(MSEobj@Catch))
  for(i in 1:MSEobj@nMPs)Catrel[,i,]<-MSEobj@Catch[,refCMP,]
  Clong<-apply(MSEobj@Catch[,,21:30]/Catrel[,,21:30],2,mean)*100
  Cshort<-apply(MSEobj@Catch[,,1:10]/Catrel[,,1:10],2,mean)*100

  Brel<-array(NA,dim(MSEobj@B))
  for(i in 1:MSEobj@nMPs)Brel[,i,]<-MSEobj@B[,refCMP,]
  Blong<-apply(MSEobj@B[,,21:30]/Brel[,,21:30],2,mean)*100

  xlim=range(Blong)*c(0.98,1.11)

  par(mfrow=c(1,2),mai=c(0.4,1.1,0.04,0.01),omi=c(0.5,0.01,0.01,0.01))

  plot(Blong,Cshort,col=MPcols,xlim=xlim,pch=19,xlab="", ylab=paste("Mean Shorter-Term Catch (projection years 1-10) relative to",MSEobj@MPs[refCMP],"(%)"))
  grid()
  abline(v=Blong[1],h=Cshort[1],col="dark grey",lty=2)
  lind<-MPcols==MPcols[1]
  lines(Blong[lind],Cshort[lind],col=MPcols[1])
  text(Blong+xadj,Cshort+yadj,MSEobj@MPs,col=MPcols,cex=cex)

  plot(Blong,Clong,col=MPcols,xlim=xlim,pch=19,xlab="",ylab=paste("Mean Longer-Term Catch (projection years 21-30) relative to",MSEobj@MPs[refCMP],"(%)"))
  grid()
  abline(v=Blong[1],h=Clong[1],col="dark grey",lty=2)
  lind<-MPcols==MPcols[1]
  lines(Blong[lind],Clong[lind],col=MPcols[1])
  text(Blong+xadj,Clong+yadj,MSEobj@MPs,col=MPcols,cex=cex)


  mtext(paste("Mean Longer-Term Biomass (projection years 21-30) relative to",MSEobj@MPs[refCMP],"(%)"),1,line=0.35,outer=T)
}



plotquant<-function(x,p=c(0.05,0.25,0.75,0.95),yrs,qcol="light blue",lcol="black",addline=T,ablines=NA){
  #plot(range(yrs),Ylims,col="white")

  ny<-length(yrs)
  x[x==Inf]<-NA
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T,type=3)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T,type=3)
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col='#b3ecff')

  polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=qcol)
  if(!is.na(ablines[1]))abline(h=ablines,col='#99999980')

  if(addline)for(i in 1:2)lines(yrs,x[i,],col=lcol,lty=i)
  lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col="white")
}


TSplot<-function(MSEobj,slot="B",name="Biomass (Mt)",unit=1E9,MPcols=c("black","grey","red","green","blue","orange","brown","purple")){

  n<-1+MSEobj@nMPs
  nr<-ceiling(n^0.5)
  nc<-ceiling(n/nr)

  yrs<-MSEobj@OM$CurrentYr[1]+1:MSEobj@proyears
  BB<-slot(MSEobj,slot)/unit
  dat<-apply(BB,3:2,mean)
  nadd<-9
  dat<-rbind(array(NA,c(nadd,ncol(dat))),dat)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.3,0.05),omi=c(0.4,0.4,0.01,0.01))

  matplot(c(yrs[1]-(nadd:1),yrs),dat,type="l",col=MPcols,lty=1)
  legend('left',legend=MSEobj@MPs,text.col=MPcols,bty="n",cex=0.8)

  Blims <- c(0,quantile(BB,0.95))

  for(i in 1:MSEobj@nMPs){
    plot(range(yrs),Blims,col="white",yaxs="i")
    grid()
    plotquant(x=BB[,i,],yrs=yrs)
    mtext(MSEobj@MPs[i],3,line=0.2,font=1)
  }

  mtext(name,2,line=0.7,outer=T)
  mtext("Year",1,line=0.7,outer=T)

}

# Robustness plots


#  MSElist<-lapply(1:7,function(x)readRDS(paste0("MSE runs/MSE_",x,".rda")))

PA_rob_plot<-function(MSElist,refMSE=1,MPnam="PA_HS_50",
                      OMnams=c("2 Less Resil.","3 Time var. M","4 Inc. M","5 Dec. M",
                               "6 Low Rec.","7 High Rec.")){

  rMSE<-MSElist[[refMSE]]
  MPind<-match(MPnam,rMSE@MPs)

  Clong<-function(X,MSElist,rMSE){mean(MSElist[[X]]@Catch[,MPind,21:30]/rMSE@Catch[,MPind,21:30])*100}
  Cshort<-function(X,MSElist,rMSE){mean(MSElist[[X]]@Catch[,MPind,1:10]/rMSE@Catch[,MPind,1:10])*100}
  Blong<-function(X,MSElist,rMSE){mean(MSElist[[X]]@B[,MPind,21:30]/rMSE@B[,MPind,21:30])*100}

  MSEind<-2:7

  Cs<-sapply(MSEind,FUN=Cshort,MSElist=MSElist,rMSE=rMSE)
  Cl<-sapply(MSEind,FUN=Clong,MSElist=MSElist,rMSE=rMSE)
  Bl<-sapply(MSEind,FUN=Blong,MSElist=MSElist,rMSE=rMSE)

  Clong5<-function(X,MSElist,rMSE){quantile(MSElist[[X]]@Catch[,MPind,21:30]/rMSE@Catch[,MPind,21:30],p=0.05)*100}
  Cshort5<-function(X,MSElist,rMSE){quantile(MSElist[[X]]@Catch[,MPind,1:10]/rMSE@Catch[,MPind,1:10],p=0.05)*100}
  Blong5<-function(X,MSElist,rMSE){quantile(MSElist[[X]]@B[,MPind,21:30]/rMSE@B[,MPind,21:30],p=0.05)*100}

  Cs5<-sapply(MSEind,FUN=Cshort5,MSElist=MSElist,rMSE=rMSE)
  Cl5<-sapply(MSEind,FUN=Clong5,MSElist=MSElist,rMSE=rMSE)
  Bl5<-sapply(MSEind,FUN=Blong5,MSElist=MSElist,rMSE=rMSE)

  ClongA<-function(X,MSElist,rMSE){as.vector(MSElist[[X]]@Catch[,MPind,21:30]/rMSE@Catch[,MPind,21:30])*100}
  CshortA<-function(X,MSElist,rMSE){as.vector(MSElist[[X]]@Catch[,MPind,1:10]/rMSE@Catch[,MPind,1:10])*100}
  BlongA<-function(X,MSElist,rMSE){as.vector(MSElist[[X]]@B[,MPind,21:30]/rMSE@B[,MPind,21:30])*100}

  CsA<-sapply(MSEind,FUN=CshortA,MSElist=MSElist,rMSE=rMSE)
  ClA<-sapply(MSEind,FUN=ClongA,MSElist=MSElist,rMSE=rMSE)
  BlA<-sapply(MSEind,FUN=BlongA,MSElist=MSElist,rMSE=rMSE)

  nMSEs<-length(MSElist)-1

  nc=nMSEs
  nr=3

  par(mfcol=c(nr,nc),mai=c(0.3,0.3,0.01,0.01),omi=c(0.5,0.35,0.35,0.06))
  ind<-1:length(MSElist)
  ind<-ind[ind!=refMSE]
  breaks<-seq(0,1000,by=2)
  range(BlA,CsA,ClA)
  xlim=c(85,115)
  mline=2.8
  for(i in 1:nMSEs){
    hist(BlA[,i],breaks,main="",xlab="",ylab="",xlim=xlim)
    abline(v=quantile(BlA,p=c(0.05,0.5,0.95)),col='#ff000080',lty=c(2,1,2))
    mtext(OMnams[i],line=1.2,cex=0.8)
    if(i ==1)mtext("Rel. Biomass, yrs 21-30",2,cex=0.8,line=mline)
    hist(CsA[,i],breaks,main="",xlab="",ylab="",xlim=xlim)
    abline(v=quantile(CsA,p=c(0.05,0.5,0.95)),col='#ff000080',lty=c(2,1,2))
    if(i ==1)mtext("Rel. Catch, yrs 1-10",2,cex=0.8,line=mline)
    hist(ClA[,i],breaks,main="",xlab="",ylab="",xlim=xlim)
    abline(v=quantile(ClA,p=c(0.05,0.5,0.95)),col='#ff000080',lty=c(2,1,2))
    if(i ==1)mtext("Rel. Catch, yrs 21-30",2,cex=0.8,line=mline)
  }
  mtext("Value Relative to Reference Case Result (%) by year-simulation",1,outer=T,line=1,cex=0.9)

}




