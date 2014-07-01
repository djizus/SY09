library(MASS)

n <- 600
pi <- 1/2
mu1 <- matrix(c(0,0))
mu2 <- matrix(c(10,0))
Id <- matrix(c(1,0,0,1),2,2)
Sigma1 <- 1 * Id 
Sigma2 <- 1 * Id 

Sigma3 <- 1 * Id 
Sigma4 <- 5 * Id

Sigma5 <- 1 * Id 
Sigma6 <- 9 * Id

Sigma7 <- 5 * Id 
Sigma8 <- 5 * Id

Sigma9 <- 9 * Id 
Sigma10 <- 9 * Id

#mvrnorm nous renvoie un sample qui est un vecteur composé de 2 valeurs
#il faut donc qu'on enregistre les 2, donc on fait une matrice avec 
#ce vecteur + une colonne pour dire sur quelle loi on est

simul <- function (n,pi,mu1,mu2,Sigma_1,Sigma_2)
{
	#on tire 600 nombres entre 0 et 1
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

resultat1 <- simul(n,pi,mu1,mu2,Sigma1,Sigma2)
resultat2 <- simul(n,pi,mu1,mu2,Sigma3,Sigma4)
resultat3 <- simul(n,pi,mu1,mu2,Sigma5,Sigma6)
resultat4 <- simul(n,pi,mu1,mu2,Sigma7,Sigma8)
resultat5 <- simul(n,pi,mu1,mu2,Sigma9,Sigma10))

plot(resultat1[,c(2,3)], col=c("blue","red")[resultat1[,c(1)]])
plot(resultat2[,c(2,3)], col=c("blue","red")[resultat1[,c(1)]])
plot(resultat3[,c(2,3)], col=c("blue","red")[resultat1[,c(1)]])
plot(resultat4[,c(2,3)], col=c("blue","red")[resultat1[,c(1)]])
plot(resultat5[,c(2,3)], col=c("blue","red")[resultat1[,c(1)]])
#boxplot(resultat1[,c(2,3)], col=c("blue","red")[resultat1[,c(1)]])



png(file = "plots/plot_simul_1.png")
plot(resultat1[resultat1[ ,1] == 1, c(2, 3)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(resultat1[resultat1[ ,1] == 2, c(2, 3)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 1, a2 = 1")
dev.off()

png(file = "plots/plot_simul_2.png")
plot(resultat2[resultat2[ ,1] == 1, c(2, 3)], col = c("blue"), ylim=c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(resultat2[resultat2[ ,1] == 2, c(2, 3)], col = c("red"), ylim=c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 1, a2 = 5")
dev.off()

png(file = "plots/plot_simul_3.png")
plot(resultat3[resultat3[ ,1] == 1, c(2, 3)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(resultat3[resultat3[ ,1] == 2, c(2, 3)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 1, a2 = 9")
dev.off()

png(file = "plots/plot_simul_4.png")
plot(resultat4[resultat4[ ,1] == 1, c(2, 3)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(resultat4[resultat4[ ,1] == 2, c(2, 3)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 5, a2 = 5")
dev.off()

png(file = "plots/plot_simul_5.png")
plot(resultat5[resultat5[ ,1] == 1, c(2, 3)], col = c("blue"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "")
par(new = TRUE)
plot(resultat5[resultat5[ ,1] == 2, c(2, 3)], col = c("red"), ylim = c(-10, 10), xlim = c(-8, 18), xlab = "", ylab = "", main = "n = 600, pi = 1/2, a1 = 9, a2 = 9")
dev.off()

