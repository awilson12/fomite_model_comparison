rm(list = ls())
iter<-1000
timestep<-0.001
source('defining parameters.R')

source('discrete_event_models.R')

frame.save.discrete<- subset(framesave, select = c(handRtotal, timeall, a.save, j.save,dose))
frame.save.discrete$model<-rep("discrete",length(frame.save.discrete$handRtotal))

frame.final.dose<-frame.save.discrete[frame.save.discrete$timeall==21,]
frame.final.dose<-data.frame(maxdose=frame.final.dose$dose,a=frame.final.dose$a.save,j=frame.final.dose$j.save,model=frame.final.dose$model)
  
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

frame.save.markov<-data.frame(handRtotal,timeall=timeall,a.save=a.save,j.save=j.save,dose=dose,
                              model=rep("Markov",length(handRtotal)))

framecombine<-rbind(frame.save.markov,frame.save.discrete)

max.dose.markov.all$model<-"Markov"

maxdoseplot<-rbind(max.dose.markov.all,frame.final.dose)


require(ggplot2)
require(ggpubr)

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=handRtotal,group=interaction(a.save,j.save,model),color=model),alpha=0.3)+
  facet_wrap(~j.save,scales="free")+scale_y_continuous(trans="log10")


windows()
ggplot(maxdoseplot)+geom_violin(aes(x=j,y=maxdose,group=interaction(j,model),fill=model),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  scale_y_continuous(trans="log10",name=expression("Log"[10]*phantom(x)*"Dose"))+
  scale_x_continuous(name="Model")+
  scale_fill_discrete(name="",labels=c("Discrete","Markov"))+
  theme_pubr()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),legend.text=element_text(size=20))

