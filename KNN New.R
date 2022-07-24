rm(list=ls())
Data=data.frame(iris)
#head(Data)
n=nrow(Data);k=5
D=c(1:n)
Datap=Data[,-5];rownames(Data)=D
class=c(rep(1,50),rep(2,50),rep(3,50))
Train.Count=floor(n*0.8);Test.Count=(n-Train.Count)
index=sample(1:n,Train.Count,replace=FALSE);head(index)
Train.Data=Datap[index,];Test.Data=Datap[-index,];Train.Class=class[index];Test.Class=class[-index]
Entire.Data=rbind(Train.Data,Test.Data);Entire.Class=c(Train.Class,Test.Class);head(Entire.Class)
Class.Matrix=matrix(nrow=n,ncol=k);Class.Predict=c()

###Trivial distance function in case of categorical variables
# dfunc=function(x,y){
#   if(x==y){return(0)}
#   else(return(1))
# }

###Function for calculating distance between two observation based on above dist function
# dobsfunc=function(i,j,m=3){
#   count=0
#   for(k in 1:m){
#     count=count+dfunc(Entire.Data[i,k],Entire.Data[j,k])}
#   return(count)
# }
#  

Dmat=as.matrix(dist(Entire.Data,diag=T,upper=T));dim(Dmat)

# In Categorical case our Dmat matrix will be
# for(i in 1:n){
#  for(j in 1:n){
#    Dmat[i,j]=dobsfunc(i,j)
#  }
# }


Train.Dist.Matrix=Dmat[1:Train.Count,1:Train.Count];dim(Train.Dist.Matrix)   ###Dist Matrix for Train Data
Test.Dist.Matrix=Dmat[(Train.Count+1):n,1:Train.Count];dim(Test.Dist.Matrix) ###Dist Matrix of Test Data from Training Observations
Dist.Matrix=Dmat[1:n,1:Train.Count]


kmat=matrix(nrow=n,ncol=Train.Count)
for(i in 1:n){
  if(i<=Train.Count){
    kmat[i,]=sort(Dist.Matrix[i,],index.return=TRUE)$ix
  }
  kmat[i,]=sort(Dist.Matrix[i,],index.return=TRUE)$ix
}

for(i in 1:Train.Count){
  for(j in 1:k){
    Class.Matrix[i,j]=Entire.Class[kmat[i,(j+1)]] 
  }
}
for(i in (Train.Count+1):n){
  for(j in 1:k){
    Class.Matrix[i,j]=Entire.Class[kmat[i,j]]
  }
}

my_mode=function(x){
 u=unique(x)
 t=tabulate(match(x,u))
 u[t==max(t)]
}

for(i in 1:n){
Class.Predict[i]=my_mode(Class.Matrix[i,])
}

Train.Predict=Class.Predict[1:Train.Count]
Test.Predict=Class.Predict[(Train.Count+1):n]

table(Train.Class,Train.Predict);table(Train.Class)
table(Test.Class,Test.Predict);table(Test.Class)    
length(which(Train.Predict!=Train.Class))/Train.Count  #Missclassification Probability for Training Data
length(which(Test.Predict!=Test.Class))/Test.Count   ####Missclassification Probability for Test Data

