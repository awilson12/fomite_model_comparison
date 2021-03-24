#Sensitivity Analysis

#--------Iterations with greatest doses relative to mean and sd---------------

#pull mean and sd per time step
duration<-21
for (i in 1:(duration+1)){
  for (j in 1:4){
    if(i==1 & j==1){
      #means for doses
      mean.discrete<-mean(framecombine$dose[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete<-sd(framecombine$dose[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov<-mean(framecombine$dose[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov<-sd(framecombine$dose[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #means for hands
      mean.discrete.hands<-mean(framecombine$hands[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete.hands<-sd(framecombine$hands[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov.hands<-mean(framecombine$hands[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov.hands<-sd(framecombine$hands[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #means for fome 1
      mean.discrete.fome1<-mean(framecombine$fome1total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete.fome1<-sd(framecombine$fome1total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov.fome1<-mean(framecombine$fome1total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov.fome1<-sd(framecombine$fome1total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #means for fome 2
      mean.discrete.fome2<-mean(framecombine$fome2total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.discrete.fome2<-sd(framecombine$fome2total[framecombine$model=="Discrete" & framecombine$timeall==i-1 & framecombine$j.save==j])
      mean.markov.fome2<-mean(framecombine$fome2total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      sd.markov.fome2<-sd(framecombine$fome2total[framecombine$model=="Markov" & framecombine$timeall==i-1 & framecombine$j.save==j])
      
      #all
      j.all<-j
      time<-i-1
      
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
paramsaveall$j<-rep(c(1,2,3,4),length(paramsaveall$fome1conc)/4)

write.csv(paramsaveall,'paramsaveall_concexplore.csv')

windows()

ggplot(paramsaveall)+geom_point(aes(x=fome1conc/fome2conc,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Fomite 1 Concentration / Fomite 2 Concentration",trans="log10")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

ggplot(paramsaveall)+geom_point(aes(x=fome1conc,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Fomite 1 Concentration")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

ggplot(paramsaveall)+geom_point(aes(x=fome2conc,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dose")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Fomite 2 Concentration")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

framecombine$startconcratio<-NA
framecombine$startfome1<-NA
framecombine$startfome2<-NA
for (i in 1:iter){
  framecombine$startconcratio[framecombine$a.save==i]<-framecombine$fome1total[framecombine$timeall==0 & framecombine$a.save==i & framecombine$j.save==1 & framecombine$model=="Markov"]/framecombine$fome2total[framecombine$timeall==0 & framecombine$a.save==i & framecombine$j.save==1 & framecombine$model=="Markov"]
  framecombine$startfome1[framecombine$a.save==i]<-framecombine$fome1total[framecombine$timeall==0 & framecombine$a.save==i & framecombine$j.save==1 & framecombine$model=="Markov"]
  framecombine$startfome2[framecombine$a.save==i]<-framecombine$fome2total[framecombine$timeall==0 & framecombine$a.save==i & framecombine$j.save==1 & framecombine$model=="Markov"]
}

windows()
ggplot(framecombine)+geom_line(aes(x=timeall,y=dose,color=frac,group=interaction(a.save,j.save,model)),alpha=0.2)+
  facet_wrap(model~j.save+frac)+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
  
#top 15% greatest doses
framesum<-framecombine[framecombine$timeall==21,]
framecombine2<-framesum[order(-framesum$dose),]
top.15<-0.15*length(framecombine2$dose)

top15.frame<-framecombine2[1:top.15,]

#checking out which scenarios led to greatest doses
table(top15.frame$model)
table(top15.frame$j.save[top15.frame$model=="Markov"])
table(top15.frame$j.save[top15.frame$model=="Discrete"])

#extracting info for these runs
for(i in 1:length(top15.frame$fome1total)){
  if(i==1){
    top15frameall<-framecombine[framecombine$a.save==top15.frame$a.save[i] & framecombine$j.save==top15.frame$j.save[i] & framecombine$model==top15.frame$model[i],]
    top15frameall$run<-i
    
    top15frameparam<-paramsaveall[paramsaveall$j==top15.frame$j.save[i] & paramsaveall$model==top15.frame$model[i] & paramsaveall$dose==top15.frame$dose[i],]
    
  }else{
    frametemp<-framecombine[framecombine$a.save==top15.frame$a.save[i] & framecombine$j.save==top15.frame$j.save[i] & framecombine$model==top15.frame$model[i],]
    frametempparam<-paramsaveall[paramsaveall$j==top15.frame$j.save[i] & paramsaveall$model==top15.frame$model[i] & paramsaveall$dose==top15.frame$dose[i],]
    frametemp$run<-i
    
    top15frameall<-rbind(top15frameall,frametemp)
    top15frameparam<-rbind(top15frameparam,frametempparam)
  }
  
}

top15frameall$jall<-top15frameall$j.save

A<-ggplot(top15.frame)+geom_density_pattern(aes(x=startfome1,fill="High Dose Iterations"),alpha=0.3,pattern="circle")+
  geom_density_pattern(data=paramsaveall,aes(x=fome1conc,fill="All Iterations"),alpha=0.3,pattern="none")+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Fomite 1 Starting Concentration")+
  theme_pubr()+
  scale_fill_manual(labels=c("All Iterations","High Dose Iterations"),values=c("grey","blue"),name="")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

B<-ggplot(top15.frame)+geom_density_pattern(aes(startfome2,fill="High Dose Iterations"),alpha=0.3,pattern="circle")+
  geom_density_pattern(data=paramsaveall,aes(fome2conc,fill="All Iterations"),alpha=0.3,pattern="none")+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Fomite 2 Starting Concentration")+
  theme_pubr()+
  scale_fill_manual(labels=c("All Iterations","High Dose Iterations"),values=c("grey","blue"),name="")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

C<-ggplot(top15.frame)+geom_density_pattern(aes(startfome1/startfome2,fill="High Dose Iterations"),alpha=0.3,pattern="circle")+
  geom_density_pattern(data=paramsaveall,aes(fome1conc/fome2conc,fill="All Iterations"),alpha=0.3,pattern="none")+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Fomite 1 / Fomite 2 Starting Concentration",trans="log10")+
  theme_pubr()+
  scale_fill_manual(labels=c("All Iterations","High Dose Iterations"),values=c("grey","blue"),name="")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

windows()
ggarrange(A,B,C,common.legend = TRUE,nrow=1)
