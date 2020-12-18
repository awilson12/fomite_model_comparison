states<-c("fome1","fome2","hands","inactivation")

#state 1 = fome 1
#state 2 = fome 2
#state 3 = hands
#state 4 = inactivation
#state 5 = mouth (dose)

non.fome.specific<-S.H[a]*TE.SH[a]*A.hand[a]
non.fome.specific.2<-0.5*S.H[a]*TE.HS[a]

if(j==1|j==2){
  
  lambda.1.3<-(1/SA.fome.1[a])*non.fome.specific*fome1.markov12
  lambda.2.3<-(1/SA.fome.2[a])*non.fome.specific*fome2.markov12
  
  lambda.3.1<-non.fome.specific.2*fome1.markov12
  lambda.3.2<-non.fome.specific.2*fome2.markov12
  
}else{
  
  lambda.1.3<-(1/SA.fome.1[a])*non.fome.specific*fome1.markov34
  lambda.2.3<-(1/SA.fome.2[a])*non.fome.specific*fome2.markov34
  
  lambda.3.1<-non.fome.specific.2*fome1.markov34
  lambda.3.2<-non.fome.specific.2*fome2.markov34
  
}

lambda.1.4<-inactiv.fome[a]
lambda.2.4<-inactiv.fome[a]

lambda.3.4.b<-(1/10^handsan[a])

if(j==1 | j==3){
  #hand hygiene not included in loss
  lambda.3.4<-inactiv.hands[a]
}else{
  #hand hygiene included in loss
  #loss due to hand hygiene
  handhygieneloss<-(1/10^handsan[a])*handsan.markov
  lambda.3.4<-(1/10^handsan[a])*handsan.markov+inactiv.hands[a]
}

lambda.3.5<-0.5*S.F[a]*TE.HF[a]*H.face

handsan.markov.moments<-c(5,10,15,20) #same time as hand hygiene moments in discrete event model
handhygiene<-c(1:duration)
handhygiene[handsan.markov.moments]<-"yes"

for (i in 1:duration){
  #we're assuming 1 min time steps in discrete model, so we assume the same here even though that would be a 
  #very long hand hygiene event
  
    if(i==1){
      handhygsave<-rep(handhygiene[i],1/timestep)
    }else{
      handhygsavetemp<-rep(handhygiene[i],1/timestep)
      handhygsave<-c(handhygsave,handhygsavetemp)
    }
}


