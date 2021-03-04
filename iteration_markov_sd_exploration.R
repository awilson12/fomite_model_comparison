#exploring effect of duration on SD (multiplication of matrix)

iter<-5000

#setting seed after checking iterations
set.seed(30)

timesteps<-c(0.1,0.01,0.001)

for (z in 1:length(timesteps)){
  #run code for defining parameters------------------------------------------------------------------------------------------
  timestep<-timesteps[z]
  
  source('defining parameters.R')
  
  duration<-10
  
  source('Markov_models_v2.R')
  
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
                                hands=hands,model=rep("Markov",length(fome1total)),timestep=timesteps[z])
  
  if(z==1){
    frametotal<-frame.save.markov
  }else{
    frametotal<-rbind(frametotal,frame.save.markov)
  }

} #end of timestep loop


#pull mean and sd per time step
for (i in 2:(duration+1)){
  for (j in 1:4){
    if(i==2 & j==1){
      #means for doses
      mean.step1<-c(0,mean(frametotal$dose[frametotal$timestep==0.1 & frametotal$timeall==i-1 & frametotal$j.save==j]))
      sd.step1<-c(0,sd(frametotal$dose[frametotal$timestep==0.1 & frametotal$timeall==i-1 & frametotal$j.save==j]))
      
      mean.step2<-c(0,mean(frametotal$dose[frametotal$timestep==0.01 & frametotal$timeall==i-1 & frametotal$j.save==j]))
      sd.step2<-c(0,sd(frametotal$dose[frametotal$timestep==0.01 & frametotal$timeall==i-1 & frametotal$j.save==j]))
      
      mean.step3<-c(0,mean(frametotal$dose[frametotal$timestep==0.001 & frametotal$timeall==i-1 & frametotal$j.save==j]))
      sd.step3<-c(0,sd(frametotal$dose[frametotal$timestep==0.001 & frametotal$timeall==i-1 & frametotal$j.save==j]))
      
      #all
      j.all<-rep(j,2)
      time<-c(0,i-1)
    }else{
      
      #means for doses
      mean.step1temp<-mean(frametotal$dose[frametotal$timestep==0.1 & frametotal$timeall==i-1 & frametotal$j.save==j])
      sd.step1temp<-sd(frametotal$dose[frametotal$timestep==0.1 & frametotal$timeall==i-1 & frametotal$j.save==j])
      
      mean.step2temp<-mean(frametotal$dose[frametotal$timestep==0.01 & frametotal$timeall==i-1 & frametotal$j.save==j])
      sd.step2temp<-sd(frametotal$dose[frametotal$timestep==0.01 & frametotal$timeall==i-1 & frametotal$j.save==j])
      
      mean.step3temp<-mean(frametotal$dose[frametotal$timestep==0.001 & frametotal$timeall==i-1 & frametotal$j.save==j])
      sd.step3temp<-sd(frametotal$dose[frametotal$timestep==0.001 & frametotal$timeall==i-1 & frametotal$j.save==j])
      
      #all
      j.alltemp<-j
      timetemp<-i-1
      
      if(i==2 & j!=1){
        #means for doses
        mean.step1temp<-c(0,mean.step1temp)
        sd.step1temp<-c(0,sd.step1temp)
        
        mean.step2temp<-c(0,mean.step2temp)
        sd.step2temp<-c(0,sd.step2temp)
        
        mean.step3temp<-c(0,mean.step3temp)
        sd.step3temp<-c(0,sd.step3temp)
        
        
        j.alltemp<-rep(j.alltemp,2)
        timetemp<-rep(timetemp,2)
      }
      
      #append
      
      #means for hands
      mean.step1<-c(mean.step1,mean.step1temp)
      sd.step1<-c(sd.step1,sd.step1temp)
      
      mean.step2<-c(mean.step2,mean.step2temp)
      sd.step2<-c(sd.step2,sd.step2temp)
      
      mean.step3<-c(mean.step3,mean.step3temp)
      sd.step3<-c(sd.step3,sd.step3temp)
      
      #all
      j.all<-c(j.all,j.alltemp)
      time<-c(time,timetemp)
    
    }
    
  } #end of j loop
  
  
  
}#end of i loop

means<-c(mean.step1,mean.step2,mean.step3)
sds<-c(sd.step1,sd.step2,sd.step3)
timestep<-rep(c(rep("0.1",length(mean.step1)),rep("0.01",length(mean.step2)),rep("0.001",length(mean.step3))))
jall<-rep(j.all,3)
timeall<-rep(time,3)

ribbonframe2<-data.frame(means,sds,timestep,jall,timeall)

windows()
ggplot(ribbonframe2)+geom_line(aes(x=timeall,y=means,group=interaction(timestep,jall),color=timestep),size=1)+
  geom_point(aes(x=timeall,y=means,group=interaction(timestep,jall),color=timestep),size=2)+
  geom_ribbon(aes(x=timeall,ymax=means+sds,ymin=means-sds,group=interaction(timestep,jall),fill=timestep),alpha=0.2)+
  facet_wrap(~jall,scales="free")+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="")+
  scale_x_continuous(name="Time (minutes)",limits=c(0,10))+
  scale_y_continuous(name="Dose")+
  theme_pubr()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),strip.text=element_text(size=16))
