require(truncdist)
require(triangle)

#TE hand->surface
TE.HS<-rtrunc(iter,"lnorm",-2.1,1.4,a=0,b=1)

#TE surface->hand
TE.SH<-rtrunc(iter,"lnorm",-2.1,1.4,a=0,b=1)

#TE hand-->face
TE.HF<-rtrunc(iter,"norm",mean=0.3390,sd=0.1318,a=0,b=1)

#surface areas of fomites
SA.fome.1<-runif(iter,150,250) 
SA.fome.2<-runif(iter,150,250)  

fome1.conc<-runif(iter,0,100)
fome2.conc<-100-fome1.conc

#fraction of hand surface area for hand-to-surf contact
S.H<-runif(iter,0.008,0.25)

#fraction of hand used for hand-to-face contact
S.F<-runif(iter,0.008,0.012)
  
#total hand surface area
A.hand<-runif(iter,445,535)

#inactivation on fomites (in per hr so x 1/60 to conver to per min)
inactiv.fome<-runif(iter,0.0048,0.013)*(1/60)

#inactivation on hands (gloved) (in per hr so x 1/60 to conver to per min)
inactiv.hands<-runif(iter,0.61,1.7)*(1/60)

#hand sanitizer efficacy
handsan<-rtrunc(iter,"norm",mean=1.06,sd=0.54,a=0,b=1.89)

#-------Markov model specific parameters--------------------------

#duration (# of events in discrete model translating to events within half hour)
duration<-21

#frequency of contacts with fomes
fome1.markov12<-8/duration #8 contacts per duration in min
fome2.markov12<-8/duration
handsan.markov<-4/duration #4 hand washes in duration in min

fome1.markov34<-12/duration #12 contacts per duration in min
fome2.markov34<-4/duration #4 contacts per duration in min

#frequency of hand-to-face contacts
H.face<-1/duration #1 contact per duration in min
