rm(list=ls())
###what if distance is 0 for two distint obs
Data=matrix(ncol=4,nrow=5);colnames(Data)=c("Fruit","Size","Taste","Seed");Data[,1]=c("Mango","Apple","Watermelon","Orange","Grape");obs=Data[,1]
Data[,2]=c("M","M","L","M","S");Data[,3]=c("Sweet","Sweet","Sweet","Sour","Sour");Data[,4]=c(1,2,2,2,0)
Data
class=c(1,1,2,2,1) ###group or class assigned
n=nrow(Data)   ###Number of Obserations
Datap=Data[,-1];Datap   ### only feature entries
cbind(Data[,1],class)
###Trivial distance function
dfunc=function(x,y){
  if(x==y){return(0)}
  else(return(1))
}

###Function for calculating distance between two observation based on above dist function
dobsfunc=function(i,j,m=3){
  count=0
  for(k in 1:m){
    count=count+dfunc(Datap[i,k],Datap[j,k])}
  return(count)
}

Dmat=matrix(nrow=n,ncol=n)   ###distance Matrix based on above function 
colnames(Dmat)=c("Mango","Apple","Watermelon","Orange","Grape")
rownames(Dmat)<-c("Mango","Apple","Watermelon","Orange","Grape")

for(i in 1:n){
  for(j in 1:n){
    Dmat[i,j]=dobsfunc(i,j)
  }
}
Dmat

k=3 ###kNN

kmat=matrix(nrow=n,ncol=k) ###kNN matrix

for(i in 1:n){
  for(j in 1:k){
    kmat[i,j]= Data[,1][sort(Dmat[i,],index.return=T)$ix[j]]
  }
  if(length(which(Dmat[i,]==0))>1){
    temp=kmat[i,][-which(kmat[i,]==obs[i])]
    kmat[i,]=append(temp,obs[i],0)
  }
}
kmat

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
mode=function(x){
  return( which(table(x)==max(table(x))))
}

my_mode = function(x) {                     # Create mode function 
  unique_x = unique(x)
  tabulate_x = tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

trainpre=c()  ###Vector for training prediction
for(i in 1:n){
  trainpre[i]=as.numeric(mode(Qprime[i,]))
}
cbind(class,trainpre)
which(class!=trainpre) ###Miss Classification

###########
###Now, Testing part
Guava=c("M","Sour",2);Guava
Banana=c("M","Sweet",0);Banana
ntest=2 ###Number of test Observations
DTest=matrix(nrow=ntest,ncol=n)

Datap=rbind(Datap,Guava,Banana);Datap

for(i in 1:n){
  for(j in 1:ntest){
    DTest[j,i]= dobsfunc(n+j,i)
  }
};DTest

DG=matrix(nrow=ntest,ncol=k)
for(j in 1:ntest){
  DG[j,]=Data[,1][sort(DTest[j,],index.return=T)$ix[1:k]]  # kNN of test (Row-wise)
};DG

ind=matrix(nrow=ntest,ncol=k)   ###Index vector to store class of kNN

for(i in 1:ntest){
  for(j in 1:k){
      ind[i,j]=class[which(Data[,1]==DG[i,j])]
  }
  
};ind

predicted.class=c()
for(i in 1:ntest){
  predicted.class[i]=mode(ind[i,])
};predicted.class   ###Class of test observations.




####################
### KNN Regression
rm(list=ls())
Data=matrix(nrow=5,ncol=2);k=3;n=5
Data[,1]=c(1,2,2,4,1);Data[,2]=c(2,3,4,9,3)
obs=c("O1","O2","O3","O4","O5")

X=Data[,1];Y=Data[,2];Datap=Y;n=length(X);k=3

Dmat=as.matrix(dist(X,upper=T,diag=T));Dmat

kmat=matrix(nrow=n,ncol=k) ###kNN matrix

for(i in 1:n){
  for(j in 1:k){
    kmat[i,j]=obs[sort(Dmat[i,],index.return=T)$ix[j]]
  }
  if(length(which(Dmat[i,]==0))>1){
    temp=kmat[i,][-which(kmat[i,]==obs[i])]
    kmat[i,]=append(temp,obs[i],0)
  }
};kmat;Dmat

newX=c(3,5)
nnew=length(newX)

Datap=c(X,newX);Datap
nprime=length(Datap)
G=matrix(nrow=nnew,ncol=k);Yall=G
for(i in 1:nnew){
G[i,]=obs[sort(as.matrix(dist(Datap,diag=T,upper=T))[n+i,1:n],index.return=T)$ix[1:k]]   ###kNN of test observation

};G

for(i in 1:nnew){
  for(j in 1:k){
    Yall[i,j]=Y[which(obs==G[i,j])]
  }
};Yall   ###Y for correspondin KNN

Yhat=c()
for(i in 1:nnew){
  Yhat[i]=mean(as.numeric(Yall[i,]))
};Yhat


