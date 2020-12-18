
save.list<-list()

for (a in 1:iter){
  
  source('rates.R')
  source('transitions.R')
  source('defining_probabilities.R')
  
  for (j in 1:4){
    
  columns=length(1:(duration*(1/timestep)))
  sim.mat<-matrix(nrow=5,ncol=columns) #convert min to one-thousandth of a minute
  
  sim.mat[1,1]<-100 #starting conc on fome 1
  sim.mat[2,1]<-5 #starting conc on fome 2
  sim.mat[3:5,1]<-0
  
    for (i in 2:(duration/timestep)){
      
      if (j==1 | j==3){
        if (handhygsave[i]=="yes"){
        #then we use new transitional probability matrix where no transitions occur excep from hands to inactivation
        #due to a hand wash event and inactivation on fomites for the fraction of time in which hands were washed, 1 timestep
          sim.mat[1,i]<-sim.mat[1,i-1]-sim.mat[1,i-1]*sum(P.1.b.total[2:4])
          sim.mat[2,i]<-sim.mat[2,i-1]-sim.mat[2,i-1]*sum(P.2.b.total[2:4])
          sim.mat[3,i]<-sim.mat[3,i-1]-sim.mat[3,i-1]*P.3.b.total[4]
          sim.mat[4,i]<-sim.mat[1,i-1]*P.1.b.total[4]+sim.mat[2,i-1]*P.2.b.total[4]+sim.mat[3,i-1]*P.3.b.total[4]
          sim.mat[5,i]<-sim.mat[5,i-1]
          
        
        }else{
          sim.mat[1,i]<-sim.mat[1,i-1]-sim.mat[1,i-1]*sum(P.1.total[2:4])+sim.mat[3,i-1]*P.3.total[1]
          sim.mat[2,i]<-sim.mat[2,i-1]-sim.mat[2,i-1]*sum(P.2.total[1],P.2.total[3:4])+sim.mat[3,i-1]*P.3.total[2]
          sim.mat[3,i]<-sim.mat[3,i-1]-sim.mat[3,i-1]*sum(P.3.total[1:2],P.3.total[4])+sim.mat[1,i-1]*P.1.total[3]+sim.mat[2,i-1]*P.2.total[3]
          sim.mat[4,i]<-sim.mat[1,i-1]*P.1.total[4]+sim.mat[2,i-1]*P.2.total[4]+sim.mat[3,i-1]*P.3.total[4]
          sim.mat[5,i]<-sim.mat[5,i-1]+sim.mat[3,i-1]*P.3.total[5]
        }
      }else{
        sim.mat[1,i]<-sim.mat[1,i-1]-sim.mat[1,i-1]*sum(P.1.total[2:4])+sim.mat[3,i-1]*P.3.total[1]
        sim.mat[2,i]<-sim.mat[2,i-1]-sim.mat[2,i-1]*sum(P.2.total[1],P.2.total[3:4])+sim.mat[3,i-1]*P.3.total[2]
        sim.mat[3,i]<-sim.mat[3,i-1]-sim.mat[3,i-1]*sum(P.3.total[1:2],P.3.total[4])+sim.mat[1,i-1]*P.1.total[3]+sim.mat[2,i-1]*P.2.total[3]
        sim.mat[4,i]<-sim.mat[1,i-1]*P.1.total[4]+sim.mat[2,i-1]*P.2.total[4]+sim.mat[3,i-1]*P.3.total[4]
        sim.mat[5,i]<-sim.mat[5,i-1]+sim.mat[3,i-1]*P.3.total[5]
      }
    
    }#end of time loop
  
    
    if (j==1){
      frame<-data.frame(state1=sim.mat[1,],state2=sim.mat[2,],state3=sim.mat[3,],state4=sim.mat[4,],dose=sim.mat[5,],
                        time=c(1:(duration/timestep)),a=a,j=j)
      max.dose.markov=data.frame(maxdose=max(sim.mat[5,]),a=a,j=j)
    }else{
      frametemp<-data.frame(state1=sim.mat[1,],state2=sim.mat[2,],state3=sim.mat[3,],state4=sim.mat[4,],dose=sim.mat[5,],
                            time=c(1:(duration/timestep)),a=a,j=j)
      max.dose.markov.temp=data.frame(maxdose=max(sim.mat[5,]),a=a,j=j)
      frame<-rbind(frame,frametemp)
      max.dose.markov=rbind(max.dose.markov,max.dose.markov.temp)
    }
    
  
  }#end of 4 model type loop
  save.list[[a]]<-frame
  if(a==1){
    max.dose.markov.all=max.dose.markov
  }else{
    max.dose.markov.all=rbind(max.dose.markov.all,max.dose.markov)
  }
}#end of iter loop


