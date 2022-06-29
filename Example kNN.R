rm(list=ls())

head(iris)
normalize=function(x){    ###For Scaling Data
  return((x-min(x))/(max(x)-min(x)))
}

iris=as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
D=as.matrix(c(1:150));head(D)
Data=as.matrix(cbind(D,iris[,-5]));rownames(Data)<-c(1:150);head(Data)
n=nrow(Data);n

Datap=as.matrix(iris[,-5]);rownames(Datap)<-c(1:150);head(Datap)
class=as.matrix(c(rep(1,50),rep(2,50),rep(3,50)))

index=sample(2,n,replace=T,prob=c(0.7,0.3))
test.data=Data[index==2,];head(test.data)
test.data.class=class[index==2]
test.datap=Datap[index==2,]
t.D=D[index==2]

Data=Data[index==1,];rownames(Data)<-c(1:150)[index==1];maindata=Data
Datap=Datap[index==1,];rownames(Datap)<-c(1:150)[index==1];mainDatap=Datap
class=class[index==1];mainclass=class
D=D[index==1]
n=nrow(Data);n
#index=rep(1,150)
Dmat=matrix(nrow=n,ncol=n)   ###distance Matrix based on above function 
colnames(Dmat)=c(1:150)[index==1]
rownames(Dmat)<-c(1:150)[index==1]


###Trivial distance function
dfunc=function(x,y){
  return(dist(c(x,y)))
}

###Function for calculating distance between two observation based on above dist function
dobsfunc=function(i,j,m=3){
  count=0
  for(k in 1:m){
    count=count+dfunc(Datap[i,k],Datap[j,k])}
  return(count)
}




euc_dist <- function(x1, x2){
  return(sqrt(sum((x1 - x2)^2)))
}

for(i in 1:n){
  for(j in 1:n){
    Dmat[i,j]=euc_dist(Datap[i,],Datap[j,])
  }
};head(Dmat)

k=12 ###kNN

kmat=matrix(nrow=n,ncol=k) ###kNN matrix
i=1;j=1
for(i in 1:n){
  for(j in 1:k){
    kmat[i,j]= D[sort(Dmat[i,],index.return=T)$ix[j]]
  }
  if(length(which(Dmat[i,]==0))>1){
    temp=kmat[i,][-which(kmat[i,]==D[i])]
    kmat[i,]=append(temp,D[i],0)
  }
};head(kmat)

indexmatrix=matrix(nrow=n,ncol=k)  ###Indexmatrix
for(i in 1:n){
  for(j in 1:k){
    indexmatrix[i,j]= which(Data[,1]==kmat[i,j])
  }
}
Q=list()   ###Class of kNN observations in list

for(i in 1:k){
  Q[[i]]=class[indexmatrix[,i]]
}

Qprime=Q[[1]]
for(i in 2:k){
  Qprime=cbind(Qprime,Q[[i]])
}
Qprime ###Class of kNN observations in Matrix

###Function for calculating mode of each row of Matrix of Class of kNN observations

my_mode = function(x) {                     # Create mode function 
  unique_x = unique(x)
  tabulate_x = tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

trainpre=c()  ###Vector for testing prediction
for(i in 1:n){
  trainpre[i]=as.numeric(my_mode(Qprime[i,]))
}
table(class,trainpre)
which(class!=trainpre) ###Miss Classification

###########
###Now, Testing part
ntest=nrow(test.data) ###Number of test Observations
DTest=matrix(nrow=ntest,ncol=n)
Datap=rbind(Datap,test.datap);head(Datap)
nrow(Datap)

for(i in 1:n){
  for(j in 1:ntest){
    DTest[j,i]=euc_dist(Datap[i,],Datap[n+j,])
    
  }
};head(DTest)

DG=matrix(nrow=ntest,ncol=k)
for(j in 1:ntest){
  DG[j,]=Data[,1][sort(DTest[j,],index.return=T)$ix[1:k]]  # kNN of test (Row-wise)
};head(DG)

ind=matrix(nrow=ntest,ncol=k)   ###Index vector to store class of kNN

for(i in 1:ntest){
  for(j in 1:k){
    ind[i,j]=class[which(Data[,1]==DG[i,j])]
  }
  
};head(ind)

predicted.class=c()
for(i in 1:ntest){
  predicted.class[i]=my_mode(ind[i,])
};table(predicted.class)  ###Class of test observations.
which(predicted.class!=test.data.class)   ###Miss classification

table(test.data.class)  ###Actual class of test data

KNN=knn(train=mainDatap,test=test.datap,cl=mainclass,k=3)
table(test.data.class,predicted.class,KNN)
table(KNN)
####################





### KNN Regression
rm(list=ls())
library(ISLR)
Data=as.matrix(Portfolio);k=3;n=nrow(Data);n
obs=c(1:100)

X=Data[,1];Y=Data[,2];Datap=Y

index=sample(2,n,replace=T,prob=c(0.7,0.3))
newX=X[which(index==2)];newY=Y[which(index==2)];nnew=length(newX)

X=Data[which(index==1),1];Y=Data[which(index==1),2];Datap=Y;n=length(X)

Dmat=as.matrix(dist(X,upper=T,diag=T));head(Dmat)

kmat=matrix(nrow=n,ncol=k) ###kNN matrix

for(i in 1:n){
  for(j in 1:k){
    kmat[i,j]=obs[sort(Dmat[i,],index.return=T)$ix[j]]
  }
  if(length(which(Dmat[i,]==0))>1){
    temp=kmat[i,][-which(kmat[i,]==obs[i])]
    kmat[i,]=append(temp,obs[i],0)
  }
};head(kmat);head(Dmat)

Datap=c(X,newX);head(Datap)
nprime=length(Datap)
G=matrix(nrow=nnew,ncol=k);Yall=G
for(i in 1:nnew){
  G[i,]=obs[sort(as.matrix(dist(Datap,diag=T,upper=T))[n+i,1:n],index.return=T)$ix[1:k]]   ###kNN of test observation
  
};head(G)

for(i in 1:nnew){
  for(j in 1:k){
    Yall[i,j]=Y[which(obs==G[i,j])]
  }
};Yall=as.matrix(Yall)   ###Y for corresponding KNN

Yhat=c()
for(i in 1:nnew){
  Yhat[i]=mean(as.numeric(Yall[i,]))
};Yhat

plot(newY-Yhat)
var(newY-Yhat)
mean(newY-Yhat)

