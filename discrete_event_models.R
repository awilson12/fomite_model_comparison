#discrete event approach


#-----event definitions--------------
#1=fomite 1 contact
#2=fomite 2 contact
#3=hand hygiene event


#discrete event model sequences

#symmetrical models
discrete.1<-rep(c(1,2,1,2,3),4)
discrete.2<-rep(c(rep(1,4),3,rep(2,4),3),2)

#asymmetrical models
discrete.3<-c(rep(c(1,1,1,1,3),3),rep(2,4),3)
discrete.4<-rep(c(1,1,1,2,3),4)

matrix<-matrix(nrow=4,ncol=length(discrete.1))
matrix[1,]<-discrete.1
matrix[2,]<-discrete.2
matrix[3,]<-discrete.3
matrix[4,]<-discrete.4

for(a in 1:iter){
  
  for (j in 1:4){
    hand<-sample(c("R","L"),duration,replace=TRUE)
    handR<-rep(0,duration)
    handL<-rep(0,duration)
    fome1<-rep(NA,duration)
    fome1[1]<-100
    fome2<-rep(NA,duration)
    fome2[1]<-5
    event<-matrix[j,]
    
    for (i in 2:duration){
      #each time step is a minute, so inactivation rates are in min^-1
      
      if (event[i]==1){
        
        fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])
        
        if(hand[i]=="R"){
          
          fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.1[a]))*(TE.SH[a]*fome1[i-1]*inactiv.fome[a]-TE.HS[a]*handR[i-1]*exp(-inactiv.hands[a]))
          handR[i]<-handR[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handR[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome1[i-1]*exp(-inactiv.fome[a])
          
          handL[i]<-handL[i-1]*exp(inactiv.hands[a])
          
        }else{
          
          fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.1[a]))*(TE.SH[a]*fome1[i-1]*inactiv.fome[a]-TE.HS[a]*handL[i-1]*exp(-inactiv.hands[a]))
          handL[i]<-handL[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handL[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome1[i-1]*exp(-inactiv.fome[a])
          
          handR[i]<-handR[i-1]*exp(inactiv.hands[a])
        }
        
      }else if (event[i]==2){
        
        fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])
        
        if(hand[i]=="R"){
          
          fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.2[a]))*(TE.SH[a]*fome2[i-1]*inactiv.fome[a]-TE.HS[a]*handR[i-1]*exp(-inactiv.hands[a]))
          handR[i]<-handR[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handR[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome2[i-1]*exp(-inactiv.fome[a])
          
          handL[i]<-handL[i-1]*exp(inactiv.hands[a])
          
        }else{
          
          fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])-(S.H[a]*(A.hand[a]/SA.fome.2[a]))*(TE.SH[a]*fome2[i-1]*inactiv.fome[a]-TE.HS[a]*handL[i-1]*exp(-inactiv.hands[a]))
          handL[i]<-handL[i-1]*exp(-inactiv.hands[a])-TE.HS[a]*S.H[a]*handL[i-1]*exp(-inactiv.hands[a])+TE.SH[a]*S.H[a]*fome2[i-1]*exp(-inactiv.fome[a])
          
          handR[i]<-handR[i-1]*exp(inactiv.hands[a])
          
        }
        
      }else{
        handR[i]<-handR[i-1]/(10^handsan[a])
        handL[i]<-handL[i-1]/(10^handsan[a])
        
        fome1[i]<-fome1[i-1]*exp(-inactiv.fome[a])
        fome2[i]<-fome2[i-1]*exp(-inactiv.fome[a])
      }
    } #end of simulation loop (i)
    
    #framecheck<-data.frame(handR=handR,handL=handL,fome1=fome1,event=event,hand=hand)
    #View(framecheck)
    
    if (j==1){
      handRtotal=handR
      handLtotal=handL
      fome1total=fome1
      fome2total=fome2
      timeall=c(1:20)
      a.save=rep(a,duration)
      j.save=rep(j,duration)
      Tehandsurf=rep(TE.HS[a],duration)
      Tesurfhand=rep(TE.SH[a],duration)
      totalhand=rep(A.hand[a],duration)
      SAfome1=rep(SA.fome.1[a],duration)
      SAfome2=rep(SA.fome.2[a],duration)
      kfome=rep(inactiv.fome[a],duration)
      khand=rep(inactiv.hands[a],duration)
    }else{
      
      Tehandsurftemp=rep(TE.HS[a],duration)
      Tesurfhandtemp=rep(TE.SH[a],duration)
      totalhandtemp=rep(A.hand[a],duration)
      SAfome1temp=rep(SA.fome.1[a],duration)
      SAfome2temp=rep(SA.fome.2[a],duration)
      kfometemp=rep(inactiv.fome[a],duration)
      khandtemp=rep(inactiv.hands[a],duration)
      a.savetemp=rep(a,duration)
      j.savetemp=rep(j,duration)
      
      
      handRtotal<-c(handRtotal,handR)
      handLtotal<-c(handLtotal,handL)
      fome1total<-c(fome1total,fome1)
      fome2total<-c(fome2total,fome2)
      timeall<-c(timeall,c(1:20))
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
    
    frame<-data.frame(handRtotal=handRtotal,handLtotal=handLtotal,fome1total=fome1total,fome2total=fome2total,
                      timeall=timeall,Tehandsurf=Tehandsurf,Tesurfhand=Tesurfhand,totalhand=totalhand,
                      SAfome1=SAfome1,SAfome2=SAfome2,kfome=kfome,khand=khand,a.save=a.save,j.save=j.save)
    
  } #end of discrete model (all 4 done) loop (j)
  
  if(a==1){
    framesave<-frame
  }else{
    framesave<-rbind(framesave,frame)
  }
  
} #end of iteration loop (a)



