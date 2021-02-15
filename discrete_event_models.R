#discrete event approach


#-----event definitions--------------
#1=fomite 1 contact
#2=fomite 2 contact
#3=hand hygiene event
#4=hand-to-face contact (dose moment)

#discrete event model sequences

for(a in 1:iter){
  
  #designate timing of face contact
  facecontact<-sample(c(1:20),1)
  
  #these event patterns correspond to figures in current manuscript draft
  
  #symmetrical models
  discrete.1<-rep(c(1,2,1,2,3),4)
  discrete.2<-rep(c(rep(1,4),3,rep(2,4),3),2)
  
  #asymmetrical models
  discrete.3<-c(rep(c(1,1,1,1,3),3),rep(2,4),3)
  discrete.4<-rep(c(1,1,1,2,3),4)
  
  #timing of hand-to-mouth contact
  
  if(facecontact!=1 & facecontact!=20){
    #the first position correspondes to time zero, or 0 event
    #this is followed by the events before the timing of the hand-to-face contact
    #followed by the events that follow the hand-to-face contact
    discrete.1<-c(0,discrete.1[1:(facecontact-1)],4,discrete.1[facecontact:20])
    discrete.2<-c(0,discrete.2[1:(facecontact-1)],4,discrete.2[facecontact:20])
    discrete.3<-c(0,discrete.3[1:(facecontact-1)],4,discrete.3[facecontact:20])
    discrete.4<-c(0,discrete.4[1:(facecontact-1)],4,discrete.4[facecontact:20])
  }else if (facecontact==1){
    #if the hand-to-face contact happens first, we place it before the other events
    discrete.1<-c(0,4,discrete.1)
    discrete.2<-c(0,4,discrete.2)
    discrete.3<-c(0,4,discrete.3)
    discrete.4<-c(0,4,discrete.4)
    #print('yes')
  }else{
    #if the hand-to-face contact happens last, we place it after the other events
    discrete.1<-c(0,discrete.1,4)
    discrete.2<-c(0,discrete.2,4)
    discrete.3<-c(0,discrete.3,4)
    discrete.4<-c(0,discrete.4,4)
  }
  
  #matrix where the row corresponds to the discrete event model and the column
  #corresponds to the event # where we track concentration changes and doses per event
  matrix<-matrix(nrow=4,ncol=length(discrete.1))
  matrix[1,]<-discrete.1
  matrix[2,]<-discrete.2
  matrix[3,]<-discrete.3
  matrix[4,]<-discrete.4
  
  for (j in 1:4){
    
    #Use the right or left hand? Randomly sampling for all events
    hand<-sample(c("R","L"),duration+1,replace=TRUE)
    
    #initializing vectors for storing concentrations on right and left hands, fomites, and dose over the events
    handR<-rep(0,duration+1)
    handL<-rep(0,duration+1)
    fome1<-rep(NA,duration+1)
    fome1[1]<-100 #100/cm2
    fome2<-rep(NA,duration+1)
    fome2[1]<-5 #5/cm2
    dose<-rep(NA,duration+1)
    dose[1]<-0
    
    #"event" is the series of events for discrete model j
    event<-matrix[j,]
    
    for (i in 2:(duration+1)){
      #each time step is a minute, so inactivation rates are in min^-1
      
      if (event[i]==1){
        
        #for a hand-to-fome 1 contact....
        
        #conc on fome 2 = its previous conc accounting for loss due to inactivation
        fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])
        #we're tracking cumulative dose, so no change in dose during this event
        dose[i]<-dose[i-1]
        
        if(hand[i]=="R"){
          #if they use the right hand...
          
          #conc on fome 1 changed based on what is transfered from handR and what's lost due to inactivation and transfer to hand
          fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.1[a]))*((TE.SH[a]*fome1[i-1]*exp(-inactiv.fome[a])-TE.HS[a]*handR[i-1]*exp(-inactiv.hands[a])))
          #conc on handR changed based on what is transferred from fomite 1 and what's lost due to inactivation and transfer from hand
          handR[i]<-handR[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handR[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome1[i-1]*exp(-inactiv.fome[a])
          
          #conc on left hand only changed by inactivation
          handL[i]<-handL[i-1]*exp(inactiv.hands[a])
          
        }else{
          #if use left hand, same pattern as above but with focus on left hand as opposed to right hand
          
          fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.1[a]))*((TE.SH[a]*fome1[i-1]*inactiv.fome[a]-TE.HS[a]*handL[i-1]*exp(-inactiv.hands[a])))
          handL[i]<-handL[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handL[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome1[i-1]*exp(-inactiv.fome[a])
          
          handR[i]<-handR[i-1]*exp(inactiv.hands[a])
        }
        
      }else if (event[i]==2){
        #if it's a contact with fome 2, same pattern as for fome 1
        
        fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])
        dose[i]<-dose[i-1]
        
        if(hand[i]=="R"){
          
          fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.2[a]))*((TE.SH[a]*fome2[i-1]*inactiv.fome[a]-TE.HS[a]*handR[i-1]*exp(-inactiv.hands[a])))
          handR[i]<-handR[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handR[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome2[i-1]*exp(-inactiv.fome[a])
          
          handL[i]<-handL[i-1]*exp(inactiv.hands[a])
          
        }else{
          
          fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.2[a]))*(TE.SH[a]*fome2[i-1]*inactiv.fome[a]-TE.HS[a]*handL[i-1]*exp(-inactiv.hands[a]))
          handL[i]<-handL[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handL[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome2[i-1]*exp(-inactiv.fome[a])
          
          handR[i]<-handR[i-1]*exp(inactiv.hands[a])
          
        }
        
      }else if (event[i]==3){
        #if it's a hand hygiene moment...
        
        dose[i]<-dose[i-1]
        
        #concentration on hands changes by anticipated reduction due to hand washing
        # !!!! Question here.. should we be accounting for reduction of concentration after accounting for inactivation on hands?
        handR[i]<-handR[i-1]/(10^handsan[a])
        handL[i]<-handL[i-1]/(10^handsan[a])
        
        #fomite concentrations reduced by inactivation
        fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])
        fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])
      }else{
        #hand to mouth contact
        
        #fome concentrations only changed by inactivation
        fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])
        fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])
        
        #hand conc and dose calculated based on which hand is used for the hand-to-face contact
        
        if(hand[i]=="R"){
        
        dose[i]<-dose[i-1]+TE.HF[a]*S.F[a]*A.hand[a]*handR[i-1]*exp(-inactiv.hands[a])
        handR[i]<-(1-TE.HF[a]*S.F[a])*handR[i-1]*exp(-inactiv.hands[a])
        handL[i]<-handL[i-1]*exp(-inactiv.hands[a])
        
        }else{
        dose[i]<-dose[i-1]+TE.HF[a]*S.F[a]*A.hand[a]*handL[i-1]*exp(-inactiv.hands[a])
        handL[i]<-(1-TE.HF[a]*S.F[a])*handL[i-1]*exp(-inactiv.hands[a])
        handR[i]<-handR[i-1]*exp(-inactiv.hands[a])
        
        }
      }
    } #end of simulation loop (i)
    
    #framecheck<-data.frame(handR=handR,handL=handL,fome1=fome1,event=event,hand=hand)
    #View(framecheck)
    
    #saving outputs
    
    if (j==1){
      dosetotal=dose
      handRtotal=handR
      handLtotal=handL
      fome1total=fome1
      fome2total=fome2
      timeall=c(0,1:duration)
      a.save=rep(a,(duration+1))
      j.save=rep(j,(duration+1))
      Tehandsurf=rep(TE.HS[a],duration)
      Tesurfhand=rep(TE.SH[a],duration)
      totalhand=rep(A.hand[a],duration)
      SAfome1=rep(SA.fome.1[a],duration)
      SAfome2=rep(SA.fome.2[a],duration)
      kfome=rep(inactiv.fome[a],duration)
      khand=rep(inactiv.hands[a],duration)
      eventtotal=event
    }else{
      dosetemp=dose
      Tehandsurftemp=rep(TE.HS[a],duration)
      Tesurfhandtemp=rep(TE.SH[a],duration)
      totalhandtemp=rep(A.hand[a],duration)
      SAfome1temp=rep(SA.fome.1[a],duration)
      SAfome2temp=rep(SA.fome.2[a],duration)
      kfometemp=rep(inactiv.fome[a],duration)
      khandtemp=rep(inactiv.hands[a],duration)
      a.savetemp=rep(a,(duration+1))
      j.savetemp=rep(j,(duration+1))
      eventtemp=event
      
      eventtotal<-c(eventtotal,eventtemp)
      dosetotal<-c(dosetotal,dosetemp)
      handRtotal<-c(handRtotal,handR)
      handLtotal<-c(handLtotal,handL)
      fome1total<-c(fome1total,fome1)
      fome2total<-c(fome2total,fome2)
      timeall<-c(timeall,c(0,1:duration))
      Tehandsurf<-c(Tehandsurf,Tehandsurftemp)
      Tesurfhand<-c(Tesurfhand,Tesurfhandtemp)
      totalhand<-c(totalhand,totalhandtemp)
      SAfome1<-c(SAfome1,SAfome1temp)
      SAfome2<-c(SAfome2,SAfome2temp)
      kfome<-c(kfome,kfometemp)
      khand<-c(khand,khandtemp)
      a.save<-c(a.save,a.savetemp)
      j.save<-c(j.save,j.savetemp)
    }
    
  } #end of discrete model (all 4 done) loop (j)
  
  frame<-data.frame(handRtotal=handRtotal,handLtotal=handLtotal,fome1total=fome1total,fome2total=fome2total,
                    timeall=timeall,a.save=a.save,j.save=j.save,dose=dose,eventtotal=eventtotal)
  param<-data.frame(Tehandsurf=Tehandsurf,Tesurfhand=Tesurfhand,totalhand=totalhand,
                    SAfome1=SAfome1,SAfome2=SAfome2,kfome=kfome,khand=khand)
  
  #saving overall larger frame for later plotting
  
  if(a==1){
    framesave<-frame
    paramsave<-param
  }else{
    framesave<-rbind(framesave,frame)
    paramsave<-rbind(paramsave,param)
  }
  
} #end of iteration loop (a)



