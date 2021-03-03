
markovfunc<-function(iter=iter){
  
  save.list.temp<-list()
  
  for (a in 1:iter){
    a<<-a
    
    for (j in 1:4){
      
      j<<-j
      
      #using parameters to inform rates
      source('rates.R')
      
      #using rates to define transitional probabilities
      source('defining_probabilities.R')
      
      #number of columns for number of timesteps
      columns=length(1:(duration*(1/timestep)+1))
      #where row in sim.mat is the state and the column is the amount of virus in that state for that timestep
      sim.mat<-matrix(nrow=5,ncol=columns) #convert min to one-thousandth of a minute
      
      sim.mat[1,1]<-100*SA.fome.1[a] #starting # of viruses on fome 1, 100 per cm2
      sim.mat[2,1]<-5*SA.fome.2[a] #starting # of viruses on fome 2, 5 per cm2
      sim.mat[3:5,1]<-0 #others start with zero
      
      initial.states<-c(sim.mat[,1])
      
      Ptemp<-P.all
      
      for (i in 1:(duration/timestep)){
        
        if (j==1 | j==3){
          #for models 1 and 3 where hand hygiene moments are treated separately at specific times in the model...
          
          #and if this is a hand hygiene moment...
          if (handhygsave[i]=="yes"){
            
            Ptemp<-Ptemp %*% P.all.b
            
          }else{
            
            Ptemp<-Ptemp %*% P.all
            
          }
        }else{
          
          #models 2 and 4
          
          Ptemp<-Ptemp %*% P.all
        }
        
        #calculating concentrations for current timestep
        
        sim.mat[,i+1]<-initial.states %*% Ptemp
        
      }#end of time loop
      
      #saving states, iteration number, model number, and doses
      
      if (j==1){
        frame<-data.frame(state1=sim.mat[1,]/SA.fome.1[a],state2=sim.mat[2,]/SA.fome.2[a],state3=sim.mat[3,]/(A.hand[a]*2),state4=sim.mat[4,],dose=sim.mat[5,],
                          time=c(1:(duration/timestep),duration/timestep+1),a=rep(a,length(sim.mat[1,])),j=rep(j,length(sim.mat[1,])))
        max.dose.markov=data.frame(maxdose=max(sim.mat[5,]),a=a,j=j)
      }else{
        frametemp<-data.frame(state1=sim.mat[1,]/SA.fome.1[a],state2=sim.mat[2,]/SA.fome.2[a],state3=sim.mat[3,]/(A.hand[a]*2),state4=sim.mat[4,],dose=sim.mat[5,],
                              time=c(1:(duration/timestep),duration/timestep+1),a=rep(a,length(sim.mat[1,])),j=rep(j,length(sim.mat[1,])))
        max.dose.markov.temp=data.frame(maxdose=max(sim.mat[5,]),a=a,j=j)
        frame<-rbind(frame,frametemp)
        max.dose.markov=rbind(max.dose.markov,max.dose.markov.temp)
      }
      
      
    }#end of 4 model type loop
    
    save.list.temp[[a]]<-frame
    if(a==1){
      max.dose.markov.all=max.dose.markov
    }else{
      max.dose.markov.all=rbind(max.dose.markov.all,max.dose.markov)
    }
  }#end of iter loop
  
  save.list<<-save.list.temp
  max.dose.markov.all<<-max.dose.markov.all
  
}

markovfunc(iter=iter)
