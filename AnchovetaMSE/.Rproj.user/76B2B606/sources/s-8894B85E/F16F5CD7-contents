# Performance plot



PA_TOplot<-function(MSEobj,refCMP=1){

  Catrel<-array(NA,dim(MSEobj@Catch))
  for(i in 1:MSEobj@nMPs)Catrel[,i,]<-MSEobj@Catch[,refCMP,]
  Clong<-apply(MSEobj@Catch[,,21:30]/Catrel[,,21:30],2,mean)
  Cshort<-apply(MSEobj@Catch[,,1:10]/Catrel[,,1:10],2,mean)

  Brel<-array(NA,dim(MSEobj@B))
  for(i in 1:MSEobj@nMPs)Brel[,i,]<-MSEobj@B[,refCMP,]
  Blong<-apply(MSEobj@B[,,21:30]/Brel[,,21:30],2,mean)

  par(mfrow=c(1,2),mai=c(0.4,0.4,0.04,0.01))

  plot(Blong,Cshort,col="white")
  text(Blong,Cshort,MSEobj@MPs)
  plot(Blong,Clong,col="white")
  text(Blong,Clong,MSEobj@MPs)

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

  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.3,0.05),omi=c(0.4,0.4,0.01,0.01))
  BB<-slot(MSEobj,slot)/unit
  matplot(apply(BB,3:2,mean),type="l",col=MPcols,lty=1)
  legend('top',legend=MSEobj@MPs,text.col=MPcols,bty="n")

  Blims <- c(0,quantile(BB,0.95))
  yrs<-MSEobj@OM$CurrentYr[1]+1:MSEobj@proyears

  for(i in 1:MSEobj@nMPs){
    plot(range(yrs),Blims,col="white",yaxs="i")
    grid()
    plotquant(x=BB[,i,],yrs=yrs)
    mtext(MSEobj@MPs[i],3,line=0.2,font=1)
  }

  mtext(name,2,line=0.7,outer=T)
  mtext("Year",1,line=0.7,outer=T)

}



