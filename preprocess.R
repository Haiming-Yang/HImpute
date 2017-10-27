tag<-x[1,]
i=1
k=0
while(i<=864){
  if(tag[i]!="NF"&&tag[i]!="NP"&&tag[i]!="TH"&&tag[i]!="PEP"){
    
    m=i-k
    x<-x[,-c(m)]
    k=k+1
  }
  i=i+1
}

a<-c()
i=1
while(i<=22525){
 xxx<-x[,i]
 fg<-sum(xxx==0)
 if(fg>=404){
   a<-c(a,i)
 }
   
  i=i+1
}

i=1
while(i<=3){
  k=1
  while(k<=405){
    if(xx[i,k]==0){
      xx[i,k]<-0
    
    }
    k=k+1
  }
  i=i+1
}