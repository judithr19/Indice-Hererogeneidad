#___________________________________________________________#
# Índice de Diversidad Heterogénea: Una medida de variabilidad y disparidad
#___________________________________________________________#

#********************************#
#### INDICES ####
#********************************#

#* INDICE PARA R^2
#* P-vector observado
#* Q-Vector esperado
Indice<-function(P,Q)
{ 
    vec0<-rep(0,length(P))
    if(sum(as.numeric(P==vec0))>1||sum(as.numeric(Q==vec0))>1)
    {return(0)
   }else{
  if(sum(P)!=0) P<-P/sum(P) # normalizado
  if(sum(Q)!=0) Q<-Q/sum(Q) # normalizado
  areaA=min(P[1],Q[1])*abs(P[2]-Q[2])
  areaB=abs(P[1]-Q[1])*abs(P[2]-Q[2])#/2
  areaC=min(P[2],Q[2])*abs(P[1]-Q[1])
  I=(areaA+areaB+areaC)
  return(I)
    }
}

#********************************#
#* INDICE PARA R^n
#* A-vector observado
#* B-Vector esperado
IndiceAn<-function(A,B)
{
  l<-length(A)
  combinaciones <- combinations(l,2)
  dim<-dim(combinaciones)[1]
  suma<-0
  for(i in 1:dim)
  {
    vi<-combinaciones[i,1]
    vj<-combinaciones[i,2]
    As<-c(A[vi],A[vj])
    Bs<-c(B[vi],B[vj])
    suma<-suma+Indice(As,Bs)
  }
  return(suma/(dim))
}

#********************************#
#* Distancia Euclideana
#* A-vector observado
#* B-Vector esperado
eucli<-function(A,B)
{ 
  if(sum(A) !=0)A<-A/sum(A)
  if(sum(B) !=0)B<-B/sum(B)
  suma<-0
  for(i in 1:length(A))
  { suma<-suma+(A[i]-B[i])^2
  }
  return(suma^(1/2))
}

#************************************
#### SIMULACIONES ####
#************************************

# dimensión n
dev.new(4,4)
par(mfrow=c(3,2))
muestras<-50
medidas<-c("Euclidiana","Índice","Gini")
colores<-c("blue","red","black")

for(n in 2:7)
{
  Tabla<-matrix(0,nrow=muestras,ncol=3)
  for(i in 1:muestras){
    A<-sample(1:20,n,replace  = T)
    B<-rep(1,n)
    Tabla[i,1]<-eucli(A,B)
    Tabla[i,2]<-IndiceAn(A,B)
    Tabla[i,3]<- Gini(A)
  }
  colnames(Tabla)<-medidas
  Tabla<-Tabla[order(Tabla[,2],decreasing = T),]
  matplot(Tabla,type="l",xlab="Muestra",ylab="Índice",
          col=colores,ylim=c(0,1),main=paste("valor n=",n))
  legend("topright",legend=medidas,lty=1,col=colores,cex=1)
}

