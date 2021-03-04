#Sensitivity Analysis

#--------Iterations with greatest doses relative to mean and sd---------------

#Top 15% doses

#first sort from greatest to lowest max dose


#then subset top 15% for discrete and Markov separately


#pull mean and sd per time step
duration<-21
for (i in 2:(duration+1)){
  for (j in 1:4){
    if(i==2 & j==1){
      #means for doses
      mean.discrete<-c(0,mean(framecombine$dose[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.discrete<-c(0,sd(framecombine$dose[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      mean.markov<-c(0,mean(framecombine$dose[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.markov<-c(0,sd(framecombine$dose[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      
      #means for hands
      mean.discrete.hands<-c(0,mean(framecombine$hands[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.discrete.hands<-c(0,sd(framecombine$hands[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      mean.markov.hands<-c(0,mean(framecombine$hands[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.markov.hands<-c(0,sd(framecombine$hands[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      
      #means for fome 1
      mean.discrete.fome1<-c(100,mean(framecombine$fome1total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.discrete.fome1<-c(0,sd(framecombine$fome1total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      mean.markov.fome1<-c(100,mean(framecombine$fome1total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.markov.fome1<-c(0,sd(framecombine$fome1total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      
      #means for fome 2
      mean.discrete.fome2<-c(5,mean(framecombine$fome2total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.discrete.fome2<-c(0,sd(framecombine$fome2total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      mean.markov.fome2<-c(5,mean(framecombine$fome2total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      sd.markov.fome2<-c(0,sd(framecombine$fome2total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j]))
      
      #all
      j.all<-rep(j,2)
      time<-c(0,i-1)
    }else{
      
      #means for doses
      mean.discretetemp<-mean(framecombine$dose[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discretetemp<-sd(framecombine$dose[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markovtemp<-mean(framecombine$dose[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markovtemp<-sd(framecombine$dose[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #means for hand
      mean.discrete.handstemp<-mean(framecombine$hands[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete.handstemp<-sd(framecombine$hands[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov.handstemp<-mean(framecombine$hands[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov.handstemp<-sd(framecombine$hands[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #means for fome 1
      mean.discrete.fome1temp<-mean(framecombine$fome1total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete.fome1temp<-sd(framecombine$fome1total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov.fome1temp<-mean(framecombine$fome1total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov.fome1temp<-sd(framecombine$fome1total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #means for fome 2
      mean.discrete.fome2temp<-mean(framecombine$fome2total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete.fome2temp<-sd(framecombine$fome2total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov.fome2temp<-mean(framecombine$fome2total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov.fome2temp<-sd(framecombine$fome2total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #all
      j.alltemp<-j
      timetemp<-i-1
      
      if(i==2 & j!=1){
        #means for doses
        mean.discretetemp<-c(0,mean.discretetemp)
        sd.discretetemp<-c(0,sd.discretetemp)
        mean.markovtemp<-c(0,mean.markovtemp)
        sd.markovtemp<-c(0,sd.markovtemp)
        
        #means for hands
        mean.discrete.handstemp<-c(0,mean.discrete.handstemp)
        sd.discrete.handstemp<-c(0,sd.discrete.handstemp)
        mean.markov.handstemp<-c(0,mean.markov.handstemp)
        sd.markov.handstemp<-c(0,sd.markov.handstemp)
        
        #means for fome 1
        mean.discrete.fome1temp<-c(100,mean.discrete.fome1temp)
        sd.discrete.fome1temp<-c(0,sd.discrete.fome1temp)
        mean.markov.fome1temp<-c(100,mean.markov.fome1temp)
        sd.markov.fome1temp<-c(0,sd.markov.fome1temp)
        
        #means for fome 2
        mean.discrete.fome2temp<-c(5,mean.discrete.fome2temp)
        sd.discrete.fome2temp<-c(0,sd.discrete.fome2temp)
        mean.markov.fome2temp<-c(5,mean.markov.fome2temp)
        sd.markov.fome2temp<-c(0,sd.markov.fome2temp)
        
        j.alltemp<-rep(j.alltemp,2)
        timetemp<-rep(timetemp,2)
      }
      
      #append
      
      #doses
      mean.discrete<-c(mean.discrete,mean.discretetemp)
      sd.discrete<-c(sd.discrete,sd.discretetemp)
      mean.markov<-c(mean.markov,mean.markovtemp)
      sd.markov<-c(sd.markov,sd.markovtemp)
      
      #hands
      mean.discrete.hands<-c(mean.discrete.hands,mean.discrete.handstemp)
      sd.discrete.hands<-c(sd.discrete.hands,sd.discrete.handstemp)
      mean.markov.hands<-c(mean.markov.hands,mean.markov.handstemp)
      sd.markov.hands<-c(sd.markov.hands,sd.markov.handstemp)
      
      #fome 1
      mean.discrete.fome1<-c(mean.discrete.fome1,mean.discrete.fome1temp)
      sd.discrete.fome1<-c(sd.discrete.fome1,sd.discrete.fome1temp)
      mean.markov.fome1<-c(mean.markov.fome1,mean.markov.fome1temp)
      sd.markov.fome1<-c(sd.markov.fome1,sd.markov.fome1temp)
      
      #fome 2
      mean.discrete.fome2<-c(mean.discrete.fome2,mean.discrete.fome2temp)
      sd.discrete.fome2<-c(sd.discrete.fome2,sd.discrete.fome2temp)
      mean.markov.fome2<-c(mean.markov.fome2,mean.markov.fome2temp)
      sd.markov.fome2<-c(sd.markov.fome2,sd.markov.fome2temp)
      
      #all
      j.all<-c(j.all,j.alltemp)
      time<-c(time,timetemp)
    }
    
  } #end of j loop
} #end of i loop

means<-c(mean.discrete,mean.markov,
         mean.discrete.hands,mean.markov.hands,
         mean.discrete.fome1,mean.markov.fome1,
         mean.discrete.fome2,mean.markov.fome2)
sds<-c(sd.discrete,sd.markov,
       sd.discrete.hands,sd.markov.hands,
       sd.discrete.fome1,sd.markov.fome1,
       sd.discrete.fome2,sd.markov.fome2)
model<-rep(c(rep("Discrete",length(mean.discrete)),rep("Markov",length(mean.discrete))),4)
type<-c(rep("Dose",length(mean.discrete)*2),
      rep("Hands",length(mean.discrete)*2),
      rep("Fomite 1",length(mean.discrete)*2),
      rep("Fomite 2",length(mean.discrete)*2))
jall<-rep(j.all,8)
timeall<-rep(time,8)

ribbonframe<-data.frame(means,sds,model,type,jall,timeall)

A<-ggplot(ribbonframe[ribbonframe$type=="Dose",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall,scales="free")+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="")+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name="Dose")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
  
B<-ggplot(ribbonframe[ribbonframe$type=="Hands",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2)+
  #geom_ribbon(aes(x=timeall,ymax=means+sds*1.96/sqrt(5000),ymin=means-sds*1.96/sqrt(5000),group=interaction(model,jall,type),fill=model),alpha=0.3)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall,scales="free")+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="")+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name="Hands")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

C<-ggplot(ribbonframe[ribbonframe$type=="Fomite 1",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall,scales="free")+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="")+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name="Fomite 1 ")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

D<-ggplot(ribbonframe[ribbonframe$type=="Fomite 2",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall,scales="free")+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="")+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name="Fomite 2")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

windows()
ggarrange(A,B,C,D,common.legend = TRUE,nrow=2,ncol=2)

#Spearman correlation coeff with estimated dose per model framework and scenario---------------------------------------------------------------

paramsavesub<-subset(paramsave,select=-c(dose))
paramsavesub$dose<-final.dose.markov
paramsavesub$model<-"Markov"
paramsave$model<-"Discrete"

paramsaveall<-rbind(paramsavesub,paramsave)
paramsaveall$j<-rep(c(1,2,3,4),length(paramsaveall$Tehandsurf)/4)

write.csv(paramsaveall,'paramsaveall.csv')

A<-ggplot(paramsaveall)+geom_point(aes(x=Tehandsurf,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Hand-to-Fomite TE")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

B<-ggplot(paramsaveall)+geom_point(aes(x=Tesurfhand,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Fomite-to-Hand TE")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))
  
C<-ggplot(paramsaveall)+geom_point(aes(x=totalhand,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Total Hand SA")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

D<-ggplot(paramsaveall)+geom_point(aes(x=SAfome1,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="SA of Fomite 1")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

E<-ggplot(paramsaveall)+geom_point(aes(x=SAfome2,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="SA of Fomite 2")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

G<-ggplot(paramsaveall)+geom_point(aes(x=kfome,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Inactivation on Fomites")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

H<-ggplot(paramsaveall)+geom_point(aes(x=khand,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dpse")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Inactivation on Hands")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

windows()


