fakt<-function(x){
  sayi<-x
  faktoriyel<-1
  while (sayi>0) {
    faktoriyel=faktoriyel*sayi
    sayi=sayi-1
  }
  return(faktoriyel)
}

kombinasyon<-function(n,r){
  sonuc<- fakt(n)/(fakt(r)* fakt(n-r))
  return(sonuc)
}







fisher_test<-function(data){
  A<-data[1,1]
  B<-data[1,2]
  C<-data[2,1]
  D<-data[2,2]
  
  N<-A+B+C+D
  
  p<-(kombinasyon(A+B,A)*kombinasyon(C+D,C))/kombinasyon(N,A+C)
  cat("Ilk adimda hesaplanan Olasilik:",p,"\n")
  
  if((sort(c(A,B,C,D))[1])<5){
    p1=0
    if(A==(sort(c(A,B,C,D))[1])){
      while(A>0){
        A=A-1
        D=D-1
        B=B+1
        C=C+1
        
        p2<-(kombinasyon(A+B,A)*kombinasyon(C+D,C))/kombinasyon(N,A+C)
        p1=p1+p2
        
        cat("Bu adimda hesaplanan olasilik:",p2,"\t","Toplam Olasilik ",p+p1,"\n")
      }
    }else if(B==(sort(c(A,B,C,D))[1])){
      while(B>0){
        B=B-1
        C=C-1
        A=A+1
        D=D+1
        
        p2<-(kombinasyon((A+B),A)*kombinasyon((C+D),C))/kombinasyon(N,(A+C))
        p1=p1+p2
        
        cat("Bu adimda hesaplanan olasilik:",p2,"\t","Toplam Olasilik ",p+p1,"\n")
      }
    }else if(C==(sort(c(A,B,C,D))[1])){
      while(C>0){
        C=C-1
        B=B-1
        A=A+1
        D=D+1
        
        p2<-(kombinasyon(A+B,A)*kombinasyon(C+D,C))/kombinasyon(N,A+C)
        p1=p1+p2
        
        cat("Bu adimda hesaplanan olasilik:",p2,"\t","Toplam Olasilik ",p+p1,"\n")
      }
    }else if(D==(sort(c(A,B,C,D))[1])){
      while(D>0){
        D=D-1
        A=A-1
        B=B+1
        C=C+1
        
        p2<-(kombinasyon(A+B,A)*kombinasyon(C+D,C))/kombinasyon(N,A+C)
        p1=p1+p2
        
        cat("Bu adimda hesaplanan olasilik:",p2,"\t","Toplam Olasilik ",p+p1,"\n")
      }
    }
  }
  return(p+p1)
  
}