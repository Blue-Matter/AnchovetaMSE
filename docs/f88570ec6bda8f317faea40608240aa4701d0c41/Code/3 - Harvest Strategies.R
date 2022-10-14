# Harvest Strategies 

PA_U<-function(x,Data,par=0.3){
  rec <- new("Rec")
  lastyr<-length(Data@AddInd[x,1,])
  rec@TAC<-Data@AddInd[x,1,lastyr]*par
  rec
}

PA_U20 <- function(x,Data,reps)PA_U(x=x,Data=Data,par=0.2)
PA_U25 <- function(x,Data,reps)PA_U(x=x,Data=Data,par=0.25)
PA_U30 <- function(x,Data,reps)PA_U(x=x,Data=Data,par=0.3)
PA_U35 <- function(x,Data,reps)PA_U(x=x,Data=Data,par=0.35)
PA_U40 <- function(x,Data,reps)PA_U(x=x,Data=Data,par=0.4)
PA_U45 <- function(x,Data,reps)PA_U(x=x,Data=Data,par=0.45)

class(PA_U)<-class(PA_U20)<-class(PA_U25)<-class(PA_U30) <-class(PA_U35) <-class(PA_U40) <-class(PA_U45)<-"MP"    