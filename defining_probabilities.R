
#Rows correspond to transitional probabilities from the state that corresponds to row number to the state that corresponds to the column number

#here since all transitions are in units of mi^-1, delta T = 1 min

#Wherever the probability of staying in the same state is not equal to 1, the probabilities are calculated in the P.i.i, P.i, and P.i.total lines, where
#i is the row of interest

# ROW 1------------------------------------------------------------------------------------

#State 1: Fomite 1---------------------------------------

lambdas.1<-c(0,lambda.1.3,lambda.1.4,0)

lambda.1.T<-sum(lambdas.1)

P.1.1<-exp(-lambda.1.T)

P.1<-(1-P.1.1)*(lambdas.1/lambda.1.T)

P.1.total<-c(P.1.1,P.1)

#when hand sanitizer used... (for models that have separate transitional probability matrices for the timed hand hygiene moments)
lambdas.1.b<-c(0,0,lambda.1.4,0)

lambda.1.b.T<-sum(lambdas.1.b)

P.1.1.b<-exp(-lambda.1.b.T)

P.1.b<-(1-P.1.1.b)*(lambdas.1.b/lambda.1.b.T)

P.1.b.total<-c(P.1.1.b,P.1.b)

#State 2: Fomite 2---------------------------------------

lambdas.2<-c(lambda.2.3,lambda.2.4,0)

lambda.2.T<-sum(lambdas.2)

P.2.2<-exp(-lambda.2.T)

P.2<-(1-P.2.2)*(lambdas.2/lambda.2.T)

P.2.total<-c(0,P.2.2,P.2)

#when hand sanitizer used... (for models that have separate transitional probability matrices for the timed hand hygiene moments)
lambdas.2.b<-c(0,lambda.2.4,0)

lambda.2.b.T<-sum(lambdas.2.b)

P.2.2.b<-exp(-lambda.2.b.T)

P.2.b<-(1-P.2.2.b)*(lambdas.2.b/lambda.2.b.T)

P.2.b.total<-c(0,P.2.2.b,P.2.b)

#State 3: Hands----------------------------------------

lambdas.3<-c(lambda.3.1,lambda.3.2,lambda.3.4,lambda.3.5)

lambda.3.T<-sum(lambdas.3)

P.3.3<-exp(-lambda.3.T)

P.3<-(1-P.3.3)*(lambdas.3/lambda.3.T)

P.3.total<-c(P.3[1:2],P.3.3,P.3[3:4])

#when hand sanitizer used...
lambdas.3.b<-c(0,0,lambda.3.4.b)

lambda.3.b.T<-sum(lambdas.3.b)

P.3.3.b<-exp(-lambda.3.b.T)

P.3.b<-(1-P.3.3.b)*(lambdas.3.b/lambda.3.b.T)

P.3.b.total<-c(P.3.b[1:2],P.3.3.b,P.3.b[3],0)

#State 4: Inactivation---------------------------------

P.4.total<-c(0,0,0,1,0) #absorbing state

#State 5: Mouth---------------------------------------

P.5.total<-c(0,0,0,0,1)

P.all<-matrix(nrow=5,ncol=5)
P.all.b<-matrix(nrow=5,ncol=5)

P.all[1,]<-P.1.total
P.all[2,]<-P.2.total
P.all[3,]<-P.3.total
P.all[4,]<-P.4.total
P.all[5,]<-P.5.total

P.all.b[1,]<-P.1.b.total
P.all.b[2,]<-P.2.b.total
P.all.b[3,]<-P.3.b.total
P.all.b[4,]<-P.4.total
P.all.b[5,]<-P.5.total



