iter<-1000
timestep<-0.001
source('defining parameters.R')

source('discrete_event_models.R')

frame.save.discrete<- subset(framesave, select = c(handRtotal, timeall, a.save, j.save))
frame.save.discrete$model<-rep("discrete",length(frame.save.discrete$handRtotal))
  
source('Markov_models.R')

for(i in 1:iter){
  list<-save.list[[i]]
  if(i==1){
    handRtotal<-list$state3/2
    timeall<-list$time*timestep
    a.save<-list$a
    j.save<-list$j
  }
  else{
    handRtotaltemp<-list$state3/2
    timealltemp<-list$time*timestep
    atemp<-list$a
    jtemp<-list$j
  }
}

frame.save.markov<-data.frame(handRtotal=handRtotal,timeall=timeall,a.save=a.save,j.save=j.save,
                              model=rep("Markov",length(handRtotal)))

framecombine<-rbind(frame.save.markov,frame.save.discrete)

require(ggplot2)
require(ggpubr)

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=handRtotal,group=interaction(a.save,j.save,model),color=model),alpha=0.3)+
  facet_wrap(~j.save,scales="free")