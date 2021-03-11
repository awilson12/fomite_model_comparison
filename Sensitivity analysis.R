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
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2,alpha=0.5)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall)+
  scale_fill_manual(name="",values=c("#339966","#000066"))+
  scale_color_manual(name="",values=c("#339966","#000066"))+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name="Dose (# viral particles)")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
  
B<-ggplot(ribbonframe[ribbonframe$type=="Hands",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2,alpha=0.5)+
  #geom_ribbon(aes(x=timeall,ymax=means+sds*1.96/sqrt(5000),ymin=means-sds*1.96/sqrt(5000),group=interaction(model,jall,type),fill=model),alpha=0.3)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall)+
  scale_fill_manual(name="",values=c("#339966","#000066"))+
  scale_color_manual(name="",values=c("#339966","#000066"))+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name=expression("Hands (viral particles/cm"^2*")"))+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

windows()
ggplot(ribbonframe[ribbonframe$type=="Hands" & ribbonframe$model=="Markov",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2,alpha=0.5)+
  #geom_ribbon(aes(x=timeall,ymax=means+sds*1.96/sqrt(5000),ymin=means-sds*1.96/sqrt(5000),group=interaction(model,jall,type),fill=model),alpha=0.3)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  scale_fill_manual(name="",values=c("#339966","#000066"))+
  scale_color_manual(name="",values=c("#339966","#000066"))+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name=expression("Hands (viral particles/cm"^2*")"))+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))


C<-ggplot(ribbonframe[ribbonframe$type=="Fomite 1",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2,alpha=0.5)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall)+
  scale_fill_manual(name="",values=c("#339966","#000066"))+
  scale_color_manual(name="",values=c("#339966","#000066"))+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name=expression("Fomite 1 (viral particles/cm"^2*")"))+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

D<-ggplot(ribbonframe[ribbonframe$type=="Fomite 2",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2,alpha=0.5)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall)+
  scale_fill_manual(name="",values=c("#339966","#000066"))+
  scale_color_manual(name="",values=c("#339966","#000066"))+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name=expression("Fomite 2 (viral particles/cm"^2*")"))+
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

I<-ggplot(paramsaveall)+geom_point(aes(x=SH,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dpse")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Fraction of Hand for Fomite Touch")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))


J<-ggplot(paramsaveall)+geom_point(aes(x=SF,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dpse")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Fraction of Hand for Mouth Touch")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

K<-ggplot(paramsaveall)+geom_point(aes(x=TEhandmouth,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dpse")+
  scale_color_continuous(name="")+
  scale_x_continuous(name="Hand-to-Mouth TE")+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))

L<-ggplot(paramsaveall)+geom_point(aes(x=handeff,y=dose,color=j,group=interaction(j,model)),alpha=0.4)+
  facet_wrap(model~j,scales="free",nrow=2,ncol=4)+theme_pubr()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  scale_y_continuous(name="Dpse")+
  scale_color_continuous(name="")+
  scale_x_continuous(name=expression("Log"[10]*phantom(x)*"Hand Hygiene Efficacy"))+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=3)))


#spearman cor
spearmancorval<-function(model="Markov",j=1){
  data<-paramsaveall[paramsaveall$model==model & paramsaveall$j==j,]
  data<-subset(data,select=-c(model))
  signif(cor(data,method=c("spearman")),2)
}
#change line below to get desired coefficients
spearmancorval(model="Markov",j=1)



#top 15% dose exploration----------------------------------------------------------


#top 15% greatest doses
framesum<-framecombine[framecombine$timeall==21,]
framecombine2<-framesum[order(-framesum$dose),]
top.15<-0.15*length(framecombine2$dose)

top15.frame<-framecombine2[1:top.15,]

#checking out which scenarios led to greatest doses
table(top15.frame$model)
length(top15.frame$model[top15.frame$model=="Discrete"])/length(top15.frame$model)
table(top15.frame$j[top15.frame$model=="Markov"])
table(top15.frame$j[top15.frame$model=="Discrete"])

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

#ggplot(top15frameparam)+geom_histogram(aes(Tehandsurf,y=..density..),alpha=0.3,fill="grey",color="black")+
  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
#  geom_histogram(data=paramsaveall,aes(Tehandsurf,y=..density..),alpha=0.3,fill="blue",color="black")+
  #geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")+
#  theme_pubr()+
#theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))

A<-ggplot(top15frameparam)+geom_histogram(aes(Tesurfhand,y=..density..,fill="Top 15% Dose Iterations"),alpha=0.5,color="black")+
  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
  geom_histogram(data=paramsaveall,aes(Tesurfhand,y=..density..,fill="All Iterations"),alpha=0.3,color="black")+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(name="Fomite-to-Hand Transfer Efficiency")+
  scale_y_continuous(name="Density")+
  scale_fill_manual(name="",labels=c("All Iterations","Top 15% Dose Iterations"),values=c("#339966","#000066"))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))
#geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")

#ggplot(top15frameparam)+geom_histogram(aes(totalhand,y=..density..),alpha=0.5,fill="grey",color="black")+
  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
#  geom_histogram(data=paramsaveall,aes(totalhand,y=..density..),alpha=0.3,fill="blue",color="black")+theme_pubr()+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
#geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")

#ggplot(top15frameparam)+geom_histogram(aes(SAfome1,y=..density..),alpha=0.5,fill="grey",color="black")+
  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
#  geom_histogram(data=paramsaveall,aes(SAfome1,y=..density..),alpha=0.3,fill="blue",color="black")+theme_pubr()+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
#geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")

#ggplot(top15frameparam)+geom_histogram(aes(SAfome2,y=..density..),alpha=0.5,fill="grey",color="black")+
  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
#  geom_histogram(data=paramsaveall,aes(SAfome2,y=..density..),alpha=0.3,fill="blue",color="black")+theme_pubr()+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
#geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")

#ggplot(top15frameparam)+geom_histogram(aes(kfome,y=..density..),alpha=0.5,fill="grey",color="black")+
  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
#  geom_histogram(data=paramsaveall,aes(kfome,y=..density..),alpha=0.3,fill="blue",color="black")+theme_pubr()+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
#geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")

B<-ggplot(top15frameparam)+geom_histogram(aes(SH,y=..density..,fill="Top 15% of Dose Iterations"),alpha=0.5,color="black")+
  #geom_density(data=top15frameparam,aes(SH),fill="grey",alpha=0.3,color="black")+
  geom_histogram(data=paramsaveall,aes(SH,y=..density..,fill="All Iterations"),alpha=0.3,color="black")+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(name="Fraction of Hand for Fomite Touch")+
  scale_y_continuous(name="Density")+
  scale_fill_manual(name="",labels=c("All Iterations","Top 15% Dose Iterations"),values=c("#339966","#000066"))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))
 #geom_density(data=paramsaveall,aes(SH),alpha=0.3,fill="blue",color="black")

C<-ggplot(top15frameparam)+geom_histogram(aes(TEhandmouth,y=..density..,fill="Top 15% of Dose Iterations"),alpha=0.5,color="black")+
  #geom_density(data=top15frameparam,aes(SH),fill="grey",alpha=0.3,color="black")+
  geom_histogram(data=paramsaveall,aes(TEhandmouth,y=..density..,fill="All Iterations"),alpha=0.3,color="black")+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(name="Hand-to-Mouth Transfer Efficiency")+
  scale_y_continuous(name="Density")+
  scale_fill_manual(name="",labels=c("All Iterations","Top 15% Dose Iterations"),values=c("#339966","#000066"))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))

D<-ggplot(top15frameparam[top15frameparam$model=="Discrete",])+geom_histogram(aes(handeff,y=..density..,fill="Top 15% of Dose Iterations"),alpha=0.5,color="black")+
  #geom_density(data=top15frameparam,aes(SH),fill="grey",alpha=0.3,color="black")+
  geom_histogram(data=paramsaveall,aes(handeff,y=..density..,fill="All Iterations"),alpha=0.3,color="black")+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(name=expression("Log"[10]*phantom(x)*"Hand Hygiene Efficacy"))+
  scale_y_continuous(name="Density")+
  scale_fill_manual(name="",labels=c("All Iterations","Top 15% Dose Iterations"),values=c("#339966","#000066"))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))

E<-ggplot(top15frameparam[top15frameparam$model=="Discrete",])+geom_histogram(aes(facecontact,y=..density..,fill="Top 15% of Dose Iterations"),binwidth=1,alpha=0.5,color="black")+
  #geom_density(data=top15frameparam,aes(SH),fill="grey",alpha=0.3,color="black")+
  geom_histogram(data=paramsaveall,aes(facecontact,y=..density..,fill="All Iterations"),alpha=0.3,color="black",binwidth = 1)+theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(name="Face contact timing")+
  scale_y_continuous(name="Density")+
  facet_wrap(~j)+
  scale_fill_manual(name="",labels=c("All Iterations","Top 15% Dose Iterations"),values=c("#339966","#000066"))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))

ggplot(ribbonframe[ribbonframe$type=="Dose",])+geom_line(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(model,jall,type),color=model),size=2,alpha=0.5)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(model,jall,type),fill=model),alpha=0.2)+
  facet_wrap(~jall,scales="free")+
  scale_fill_manual(name="",values=c("#339966","#000066"))+
  scale_color_manual(name="",values=c("#339966","#000066"))+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(name="Dose")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))

table(top15frameparam$facecontact[top15frameparam$model=="Discrete"])

table(top15frameparam$facecontact[top15frameparam$model=="Discrete" & top15frameparam$j==4])

#(top15frameparam)+geom_histogram(aes(SF,y=..density..),alpha=0.5,fill="grey",color="black")+
#  #geom_density(data=top15frameparam,aes(fome1conc),fill="grey",alpha=0.3,color="black")+
#  geom_histogram(data=paramsaveall,aes(SF,y=..density..),alpha=0.3,fill="blue",color="black")+theme_pubr()+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
#geom_density(data=paramsaveall,aes(fome2conc),alpha=0.3,fill="blue",color="black")

windows()
ggarrange(A,B,C,D,common.legend=TRUE)

windows()
E




