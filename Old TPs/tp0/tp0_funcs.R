prodtrans<-function(X)
{
	struct <- NULL
	struct$champ1 <- X
	struct$champ2 <- t(X)
	struct$champ3 <- t(X)* X
	struct
	#X * t(X)
}

centre<-function(X)
{
	struct <- NULL
	struct$champ1 <- X
	struct$champ2 <- apply(X,2,mean)	
	for(i in 1:dim(X)[2]) {	
		X[,i] <- X[,i] - struct$champ2[i]
    	}	
	struct$champ3 <- X
	struct
}

centrerep<-function(X)
{
	struct <- NULL
	struct$champ1 <- X
	struct$champ2 <- apply(X,2,mean)	
	struct$champ3 <- rep(struct$champ2,dim(X)[1])
	struct$champ4 <- matrix(struct$champ3,nrow=dim(X)[1],byrow=T)
	struct$champ5 <- X - struct$champ4
	struct
}

hist.factor<-function(X,Y){
	inter<-seq(min(X),max(X),by=(max(X)-min(X))/10)
	h<-hist(plot=F,X[Y==levels(Y)[1]],breaks=inter)$count
	for (m in 2:nlevels(Y))
	h<-rbind(h,hist(plot=F,X[Y==levels(Y)[m]],breaks=inter)$count)
	barplot(h,space=0,legend=levels(Y),col=palette())
}
