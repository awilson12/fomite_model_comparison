
#clear environment
rm(list = ls())

#iteration and time step per scenario
iter<-5000
timestep<-0.001

#setting seed after checking iterations
set.seed(30)

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
a.save.dose.discrete<-frame.save.discrete$a[frame.save.discrete$timeall==21]
j.save.dose.discrete<-frame.save.discrete$j[frame.save.discrete$timeall==21]

rm(framesave)

#run code for Markov models-------------------------------------------------------------------------------------------------

source('Markov_models_v2.R')

#saving output from Markov models per minute to avoid memory issues
for(i in 1:iter){
  list<-save.list[[i]]
  #list<-list[list$time==1 | list$time%%(1/timestep)==0,]
  if(i==1){
    hands<-list$state3
    fome1total<-list$state1
    fome2total<-list$state2
    dose<-list$dose
    timeall<-list$time*timestep
    a.save<-list$a
    j.save<-list$j
    a.save.dose<-rep(list$a[1],4)
    j.save.dose<-c(1,2,3,4)
    finaldose<-c(max(list$dose[list$j==1]),max(list$dose[list$j==2]),max(list$dose[list$j==3]),max(list$dose[list$j==4]))
  }
  else{
    handstemp<-list$state3
    fome1totaltemp<-list$state1
    fome2totaltemp<-list$state2
    timealltemp<-list$time*timestep
    atemp<-list$a
    jtemp<-list$j
    dosetemp<-list$dose
    finaldosetemp<-c(max(list$dose[list$j==1]),max(list$dose[list$j==2]),max(list$dose[list$j==3]),max(list$dose[list$j==4]))
    a.save.dosetemp<-rep(list$a[1],4)
    j.save.dosetemp<-c(1,2,3,4)
    
    timeall<-c(timeall,timealltemp)
    j.save<-c(j.save,jtemp)
    a.save<-c(a.save,atemp)
    j.save.dose<-c(j.save.dose,j.save.dosetemp)
    a.save.dose<-c(a.save.dose,a.save.dosetemp)
    hands<-c(hands,handstemp)
    fome1total<-c(fome1total,fome1totaltemp)
    fome2total<-c(fome2total,fome2totaltemp)
    dose<-c(dose,dosetemp)
    finaldose<-c(finaldose,finaldosetemp)
  }
}

frame.save.markov<-data.frame(fome1total,fome2total,timeall=timeall,a.save=a.save,j.save=j.save,dose=dose,
                              hands=hands,model=rep("Markov",length(fome1total)))

#Combining discrete event and markov model outputs-------------------------------------------------------------------------
framecombine<-rbind(frame.save.markov,frame.save.discrete)

#combining dose estimates
final.dose.markov=finaldose

finaldoses<-c(final.dose.discrete,final.dose.markov)

maxdoseplot<-data.frame(maxdose=finaldoses,
                        j=c(j.save.dose.discrete,j.save.dose),
                        a=c(a.save.dose.discrete,a.save.dose),
                        model=c(rep("discrete",length(final.dose.discrete)),rep("markov",length(final.dose.markov))))

write.csv(framecombine,'frame_combine.csv')
write.csv(maxdoseplot,'maxdoseplot.csv')
write.scv(paramsave,'paramsave.csv')

#Plots---------------------------------------------------------------------------------------------------------------------

require(ggplot2)
require(ggpubr)

maxdoseplot$symmetry<-NA
maxdoseplot$symmetry[maxdoseplot$j==1 | maxdoseplot$j==2]<-"Symmetric Contact Frequency"
maxdoseplot$symmetry[maxdoseplot$j==3 | maxdoseplot$j==4]<-"Asymmetric Contact Frequency"

#violin plots to compare estimated doses among models
windows()
ggplot(maxdoseplot)+geom_violin(aes(x=model,y=maxdose,group=interaction(as.character(j),model),fill=as.character(j)),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  scale_y_continuous(trans="log10",name=expression("Log"[10]*phantom(x)*"Dose"))+
  scale_x_discrete(name="",labels=c("Discrete","Markov"))+
  scale_fill_discrete(name="Model Scenario")+
  theme_pubr()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=18))+
  facet_wrap(~symmetry)

#------summary statistic function and check---------------------

sumstat<-function(model=model,j=j){
  print("Summary:")
  print(summary(maxdoseplot$maxdose[maxdoseplot$model==model & maxdoseplot$j==j]))
  
  print("IQR:")
  print(IQR(maxdoseplot$maxdose[maxdoseplot$model==model & maxdoseplot$j==j]))
  
  print("SD:")
  print(sd(maxdoseplot$maxdose[maxdoseplot$model==model & maxdoseplot$j==j]))
  }


sumstat(model="markov",j=1)


#---------old checks/notes-------------------------

#checking effect of iteration
#maxdoseplotmarkov<-data.frame(maxdose=final.dose.markov,a=a.save.dose,j=j.save.dose)

#maxdoseplotmarkov$symmetry<-NA
#maxdoseplotmarkov$symmetry[maxdoseplotmarkov$j==1 | maxdoseplotmarkov$j==2]<-"Symmetric Contact Frequency"
#maxdoseplotmarkov$symmetry[maxdoseplotmarkov$j==3 | maxdoseplotmarkov$j==4]<-"Asymmetric Contact Frequency"

#appears the 5,000 iter results look similar on the top as the 1,000 iter results
#ggplot(maxdoseplotmarkov)+geom_violin(aes(x=symmetry,group=as.character(j),y=maxdose))+
#  scale_y_continuous(trans="log10")

#untransformed, you see the top tails, but these tails taper off on a log10 scale much faster than the lower tails
#ggplot(maxdoseplotmarkov)+geom_violin(aes(x=symmetry,group=as.character(j),y=maxdose))

#checking relative to y scale used for direct comparison to figure 4
#ggplot(maxdoseplotmarkov)+geom_violin(aes(x=symmetry,group=as.character(j),y=maxdose))+
#  scale_y_continuous(trans="log10",limits=c(1e-4,1e2))


#line plot to compare concentration on hands over time among models

#A<-ggplot(framecombine)+geom_line(aes(x=timeall,y=fome1total,group=interaction(a.save,j.save,model),color=model),alpha=0.2)+
#  facet_wrap(~j.save,scales="free")+theme_pubr()+theme(axis.title=element_text(size=18),axis.text=element_text(size=18),strip.text=element_text(size=18),
#                                                       legend.text = element_text(size=16))+
#  scale_color_discrete(name="")+
#  scale_x_continuous(name="Time (minutes)")+scale_y_continuous(name=expression("Fomite 1: Viral particles/cm"^2*""))+guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))

#B<-ggplot(framecombine)+geom_line(aes(x=timeall,y=fome2total,group=interaction(a.save,j.save,model),color=model),alpha=0.2)+
#  facet_wrap(~j.save,scales="free")+
#  theme_pubr()+
#  theme(axis.title=element_text(size=18),axis.text=element_text(size=18),strip.text=element_text(size=18),
#         legend.text = element_text(size=16))+
#  scale_color_discrete(name="")+
#  scale_x_continuous(name="Time (minutes)")+scale_y_continuous(name=expression("Fomite 2: Viral particles/cm"^2*""))+guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))

#C<-ggplot(framecombine)+geom_line(aes(x=timeall,y=hands,group=interaction(a.save,j.save,model),color=model),alpha=0.2)+
#  facet_wrap(~j.save,scales="free")+
#  theme_pubr()+
#  theme(axis.title=element_text(size=18),axis.text=element_text(size=18),strip.text=element_text(size=18),
#        legend.text = element_text(size=16))+
#  scale_color_discrete(name="")+
#  scale_x_continuous(name="Time (minutes)")+scale_y_continuous(name=expression("Hands: Viral particles/cm"^2*""))+guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))


#ggplot(framecombine[framecombine$model=="Discrete",])+geom_line(aes(x=timeall,y=hands,group=interaction(a.save,j.save,model),color=model),alpha=0.2)+
#  facet_wrap(~j.save,scales="free")+
#  theme_pubr()+
#  theme(axis.title=element_text(size=18),axis.text=element_text(size=18),strip.text=element_text(size=18),
#        legend.text = element_text(size=16))+
#  scale_color_discrete(name="")+
#  scale_x_continuous(name="Time (minutes)")+scale_y_continuous(name=expression("Hands: Viral particles/cm"^2*""))+guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))

#D<-ggplot(framecombine)+geom_line(aes(x=timeall,y=dose,group=interaction(a.save,j.save,model),color=model),alpha=0.2)+
#  facet_wrap(~j.save,scales="free")+
#  theme_pubr()+
#  theme(axis.title=element_text(size=18),axis.text=element_text(size=18),strip.text=element_text(size=18),
#        legend.text = element_text(size=16))+
#  scale_color_discrete(name="")+
#  scale_x_continuous(name="Time (minutes)")+scale_y_continuous(name="Dose")+guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))

#windows()
#ggarrange(A,B,C,nrow=1,common.legend = TRUE)


#checking  iter discrete
summary(frame.save.markov$dose[frame.save.markov$j==1 & frame.save.markov$timeall==0.021])
IQR(frame.save.markov$dose[frame.save.markov$j==1])

#checking 5,000 iter Markov
#frame.markov<-data.frame(dose=final.dose.markov,a=a.save.dose,j=j.save.dose)

#summary(frame.markov$dose[frame.markov$j==1])