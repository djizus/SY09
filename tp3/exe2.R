#On sait que  f11 et f12 suivent des lois normales d'espérance -1 et -1 
#or ces deux lois sont conditionellement indépendantes
#on en déduit donc que f1, produit de ces 2 variable suit une loi normale d'espérance 
#E(f11)*E(f12)=-1
#Comme X et Y sont des variables aleatoires discretes independantes Cov(X1, X2)=0#cov(f11, f12)=0
#==> matrice diagonale

var(X1+X2) = var(X1)+ var(X2)+ 2Cov(X1, X2) = var(X1)+ var(X2)
#avec var(X) = E([X-E(X)][X-E(X)]t)

#On sait que  f21 et f22 suivent des lois normales d'espérance 1 et 1 
#or ces deux lois sont conditionellement indépendantes
#on en déduit donc que f2, produit de ces 2 variable suit une loi normale d'espérance 
#E(f11)*E(f12)=1
#en outre, la covariance cov(f11, f12)=0
#==> matrice diagonale

pi = 1/2
n1 = 10
n2 = 100
n3 = 1000
n4 = 10000
n5 = 100000

mu1 <- matrix(c(1,0))
mu2 <- matrix(c(1,0))

Id <- matrix(c(1,0,0,1),2,2)
Sigma1 <- 1 * Id 
Sigma2 <- 1 * Id 

simul2 <- function (n,pi,mu1,mu2,Sigma_1,Sigma_2)
{
	samples <- sample(0:1, n, replace=TRUE)
	resultat <- matrix(nrow=n, ncol=3)
	for(k in 1:n)
	{
		if(samples[k] <= pi)
		{
			resultat[k,c(1)] <- 1
			resultat[k, c(2,3)] <- mvrnorm(1, mu1, Sigma_1)
		}
		else		
		{
			resultat[k,c(1)] <- 2
			resultat[k, c(2,3)] <- mvrnorm(1, mu2, Sigma_2)
		}
	}
resultat
}

res1 <- simul2(n1,pi,mu1,mu2,Sigma1,Sigma2)
res2 <- simul2(n2,pi,mu1,mu2,Sigma1,Sigma2)
res3 <- simul2(n3,pi,mu1,mu2,Sigma1,Sigma2)
res4 <- simul2(n3,pi,mu1,mu2,Sigma1,Sigma2)
res5 <- simul2(n3,pi,mu1,mu2,Sigma1,Sigma2)


plot(res1[,c(2,3)], col=c("blue","red")[res1[,c(1)]])
plot(res2[,c(2,3)], col=c("blue","red")[res1[,c(1)]])
plot(res3[,c(2,3)], col=c("blue","red")[res1[,c(1)]])
plot(res4[,c(2,3)], col=c("blue","red")[res1[,c(1)]])
plot(res5[,c(2,3)], col=c("blue","red")[res1[,c(1)]])