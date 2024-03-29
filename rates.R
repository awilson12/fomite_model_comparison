states<-c("fome1","fome2","hands","inactivation")

#state 1 = fome 1
#state 2 = fome 2
#state 3 = hands
#state 4 = inactivation
#state 5 = mouth (dose)

#this part of the rate equation for fomite-to-hand transfer applies regardless of fomite identity
non.fome.specific<-S.H[a]*TE.SH[a]*A.hand[a]

#this part of hte rate equation for hand-to-fomite transfer applies regardless of fomite identity
non.fome.specific.2<-0.5*S.H[a]*TE.HS[a]

if(j==1|j==2){
  
  #for Markov models 1 and 2 (symmetrical fome contacts) (convert to per time step)
  
  lambda.1.3<-(1/SA.fome.1[a])*non.fome.specific*fome1.markov12*timestep
  lambda.2.3<-(1/SA.fome.2[a])*non.fome.specific*fome2.markov12*timestep
  
  lambda.3.1<-non.fome.specific.2*fome1.markov12*timestep
  lambda.3.2<-non.fome.specific.2*fome2.markov12*timestep
  
}else{
  
  #for Markov models 3 and 4 (asymmetrical fome contacts)
  
  lambda.1.3<-(1/SA.fome.1[a])*non.fome.specific*fome1.markov34*timestep
  lambda.2.3<-(1/SA.fome.2[a])*non.fome.specific*fome2.markov34*timestep
  
  lambda.3.1<-non.fome.specific.2*fome1.markov34*timestep
  lambda.3.2<-non.fome.specific.2*fome2.markov34*timestep
  
}

#inactivation rates for fomites 1 and 2
lambda.1.4<-inactiv.fome[a]*timestep
lambda.2.4<-inactiv.fome[a]*timestep

#loss due to hand washing
k.handsan.event=log(10^handsan[a])*timestep
lambda.3.4.b<-k.handsan.event+(inactiv.hands[a]*timestep)

if(j==1 | j==3){
  #hand hygiene not included in loss
  lambda.3.4<-inactiv.hands[a]*timestep
  
}else{
  #hand hygiene included in loss
  #loss due to hand hygiene
  #handhygieneloss<-(1/10^handsan[a])*handsan.markov*timestep
  #calculate first order decay rate
  k.handsan=log(10^(handsan[a]*4))/duration
  lambda.3.4<-(k.handsan+inactiv.hands[a])*timestep
}

#loss from hands due to dose moment (hand-to-mouth contact)
lambda.3.5<-0.5*S.F[a]*TE.HF[a]*H.face*timestep

#timing of hand hygiene moments
handsan.markov.moments<-c(6,11,16,21) #same time as hand hygiene moments in discrete event model
#making a vector where "yes" corresponds to hand hygiene moment
handhygiene<-c(1:duration)
handhygiene[handsan.markov.moments]<-"yes"

#converting this to units of time used in the Markov model so that the "event" lasts a whole minute
#to be consistent with the discrete event models' timing assumptions

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


