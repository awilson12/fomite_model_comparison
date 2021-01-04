
save.list<-list()

for (a in 1:iter){
  
  #using parameters to inform rates
  source('rates.R')
  
  #using rates to define transitional probabilities
  source('defining_probabilities.R')
  
  for (j in 1:4){
  
  #number of columns for number of timesteps
  columns=length(1:(duration*(1/timestep)))
  #where row in sim.mat is the state and the column is the amount of virus in that state for that timestep
  sim.mat<-matrix(nrow=5,ncol=columns) #convert min to one-thousandth of a minute
  
  sim.mat[1,1]<-100*SA.fome.1[a] #starting # of viruses on fome 1, 100 per cm2
  sim.mat[2,1]<-5*SA.fome.2[a] #starting # of viruses on fome 2, 5 per cm2
  sim.mat[3:5,1]<-0 #others start with zero
  
    for (i in 2:(duration/timestep)){
      
      if (j==1 | j==3){
        #for models 1 and 3 where hand hygiene moments are treated separately at specific times in the model...
        
        #and if this is a hand hygiene moment...
        if (handhygsave[i]=="yes"){
          
        #then we use new transitional probability matrix where no transitions occur excep from hands to inactivation
        #due to a hand wash event and inactivation on fomites for the fraction of time in which hands were washed, 1 timestep
          
          sim.mat[1,i]<-sim.mat[1,i-1]-sim.mat[1,i-1]*sum(P.1.b.total[2:4])
          sim.mat[2,i]<-sim.mat[2,i-1]-sim.mat[2,i-1]*sum(P.2.b.total[2:4])
          sim.mat[3,i]<-sim.mat[3,i-1]-sim.mat[3,i-1]*P.3.b.total[4]
          sim.mat[4,i]<-sim.mat[1,i-1]*P.1.b.total[4]+sim.mat[2,i-1]*P.2.b.total[4]+sim.mat[3,i-1]*P.3.b.total[4]
          sim.mat[5,i]<-sim.mat[5,i-1]
          
        
        }else{
          #otherwise, we use the original matrix for Markov models 1 and 3 that do not account for transitions from hands to loss
          #other than what is lost due to inactivation
          sim.mat[1,i]<-sim.mat[1,i-1]-sim.mat[1,i-1]*sum(P.1.total[2:4])+sim.mat[3,i-1]*P.3.total[1]
          sim.mat[2,i]<-sim.mat[2,i-1]-sim.mat[2,i-1]*sum(P.2.total[1],P.2.total[3:4])+sim.mat[3,i-1]*P.3.total[2]
          sim.mat[3,i]<-sim.mat[3,i-1]-sim.mat[3,i-1]*sum(P.3.total[1:2],P.3.total[4])+sim.mat[1,i-1]*P.1.total[3]+sim.mat[2,i-1]*P.2.total[3]
          sim.mat[4,i]<-sim.mat[1,i-1]*P.1.total[4]+sim.mat[2,i-1]*P.2.total[4]+sim.mat[3,i-1]*P.3.total[4]
          sim.mat[5,i]<-sim.mat[5,i-1]+sim.mat[3,i-1]*P.3.total[5]
        }
      }else{
        #if not models 1 and 3, then losses due to inactivation and hand washing are lumped
        sim.mat[1,i]<-sim.mat[1,i-1]-sim.mat[1,i-1]*sum(P.1.total[2:4])+sim.mat[3,i-1]*P.3.total[1]
        sim.mat[2,i]<-sim.mat[2,i-1]-sim.mat[2,i-1]*sum(P.2.total[1],P.2.total[3:4])+sim.mat[3,i-1]*P.3.total[2]
        sim.mat[3,i]<-sim.mat[3,i-1]-sim.mat[3,i-1]*sum(P.3.total[1:2],P.3.total[4])+sim.mat[1,i-1]*P.1.total[3]+sim.mat[2,i-1]*P.2.total[3]
        sim.mat[4,i]<-sim.mat[1,i-1]*P.1.total[4]+sim.mat[2,i-1]*P.2.total[4]+sim.mat[3,i-1]*P.3.total[4]
        sim.mat[5,i]<-sim.mat[5,i-1]+sim.mat[3,i-1]*P.3.total[5]
      }
    
    }#end of time loop
  
    #saving states, iteration number, model number, and doses
    
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


