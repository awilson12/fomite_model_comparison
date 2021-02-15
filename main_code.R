
#clear environment
rm(list = ls())

#iteration and time step per scenario
iter<-1000
timestep<-0.001

#run code for defining parameters------------------------------------------------------------------------------------------

source('defining parameters.R')

#run code for the discrete event models------------------------------------------------------------------------------------

source('discrete_event_models.R')

#saving output from discrete event models
frame.save.discrete<- subset(framesave, select = c(handRtotal,handLtotal,fome1total,fome2total,timeall, a.save, j.save,dose))
frame.save.discrete$model<-rep("Discrete",length(frame.save.discrete$handRtotal))
frame.save.discrete$hands<-(frame.save.discrete$handRtotal+frame.save.discrete$handLtotal)/2
frame.save.discrete<-subset(frame.save.discrete,select=c(-handRtotal,-handLtotal))

final.dose.discrete<-frame.save.discrete$dose[frame.save.discrete$timeall==21]

#run code for Markov models-------------------------------------------------------------------------------------------------

source('Markov_models.R')

#saving output from Markov models per minute to avoid memory issues
for(i in 1:iter){
  list<-save.list[[i]]
  list<-list[list$time==1 | list$time%%(1/timestep)==0,]
  if(i==1){
    hands<-list$state3
    fome1total<-list$state1
    fome2total<-list$state2
    dose<-list$dose
    timeall<-list$time*timestep
    a.save<-list$a
    j.save<-list$j
    #finaldose<-max(list$dose)
  }
  else{
    handstemp<-list$state3
    fome1totaltemp<-list$state1
    fome2totaltemp<-list$state2
    timealltemp<-list$time*timestep
    atemp<-list$a
    jtemp<-list$j
    dosetemp<-list$dose
    finaldosetemp<-max(list$dose)
    
    timeall<-c(timeall,timealltemp)
    j.save<-c(j.save,jtemp)
    a.save<-c(a.save,atemp)
    hands<-c(hands,handstemp)
    fome1total<-c(fome1total,fome1totaltemp)
    fome2total<-c(fome2total,fome2totaltemp)
    dose<-c(dose,dosetemp)
    #finaldose<-c(finaldose,finaldosetemp)
  }
}

frame.save.markov<-data.frame(fome1total,fome2total,timeall=timeall,a.save=a.save,j.save=j.save,dose=dose,
                              hands=hands,model=rep("Markov",length(fome1total)))

#Combining discrete event and markov model outputs-------------------------------------------------------------------------
framecombine<-rbind(frame.save.markov,frame.save.discrete)

#combining dose estimates
final.dose.markov=finaldose

#finaldoses<-c(final.dose.discrete,final.dose.markov)

#maxdoseplot<-data.frame(maxdose=finaldoses,
#                        j=c(frame.save.discrete$j.save,frame.save.markov$j.save),
#                        model=c(frame.save.discrete$model,frame.save.markov$model))

#Plots---------------------------------------------------------------------------------------------------------------------

require(ggplot2)
require(ggpubr)

#line plot to compare concentration on hands over time among models

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=fome1total,group=interaction(a.save,j.save,model),color=model),alpha=0.3)+
  facet_wrap(~j.save,scales="free")

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=fome2total,group=interaction(a.save,j.save,model),color=model),alpha=0.3)+
  facet_wrap(~j.save,scales="free")

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=hands,group=interaction(a.save,j.save,model),color=model),alpha=0.3)+
  facet_wrap(~j.save,scales="free")

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=dose,group=interaction(a.save,j.save,model),color=model),alpha=0.3)+
  facet_wrap(~j.save,scales="free")

#violin plots to compare estimated doses among models
#windows()
#ggplot(maxdoseplot)+geom_violin(aes(x=j,y=maxdose,group=interaction(j,model),fill=model),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
#  scale_y_continuous(trans="log10",name=expression("Log"[10]*phantom(x)*"Dose"))+
#  scale_x_continuous(name="Model")+
#  scale_fill_discrete(name="",labels=c("Discrete","Markov"))+
#  theme_pubr()+
#  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),legend.text=element_text(size=20))

