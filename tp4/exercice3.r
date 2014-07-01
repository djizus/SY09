# Exercice 3 : DOnnées réelles

data(Pima.tr)
data(Pima.te)

Xapp<-as.matrix(Pima.tr[,1:7])
yapp<-rep(0,dim(Pima.tr)[1])
yapp[Pima.tr[,8]=="No"]<-1
yapp[Pima.tr[,8]=="Yes"]<-2
yapp<-as.factor(yapp)

zapp<-0

for (i in 1:length(yapp)){
  if(yapp[i]==1){
    zapp[i]=1
  } else {
    zapp[i]=0
  }
}


Xtst<-as.matrix(Pima.te[,1:7])
ytst<-rep(0,dim(Pima.te)[1])
ytst[Pima.te[,8]=="No"]<-1
ytst[Pima.te[,8]=="Yes"]<-2
ytst<-as.factor(ytst)

lda<-lda(Xapp,yapp)
qda<-qda(Xapp,yapp)
lrl<-logreg(Xapp,zapp)

model1<-predict(lda,Xtst)
model2<-predict(qda,Xtst)
model3<-logeva(Xtst,lrl$beta)

#Calcul des erreurs
error_model1 <- model1$class==ytst
error_model2 <- model2$class==ytst
error_model3 <- model3$deci==ytst

error <- function(X){
  n<-length(X)
  error = 0
  for (i in 1:n) {
    if (X[i]==FALSE) {
      error = error + 1
    }
    
  }
  return (error/n*100)
}

error_model1 <- error(error_model1)
error_model2 <- error(error_model2)
error_model3 <- error(error_model3)
	