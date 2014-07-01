calculMu <- function(ech)
{
mu = matrix(c(0,0,0,0),2,2)
tt1 =0
tt2 =0

for (i in 1:nrow(ech[1:(nrow(ech)),]))
{
	if(ech[i,1] == 1){
	mu[1] = mu[1] + ech[i,2]
	mu[2] = mu[2] + ech[i,3]
	tt1 = tt1+1
	}else{
	mu[3] = mu[3] + ech[i,2]
	mu[4] = mu[4] + ech[i,3]
	tt2 = tt2+1
	}
}
#loi 1
mu[1] = mu[1] / tt1
mu[2] = mu[2] / tt1

#loi 2
mu[3] = mu[3] / tt2
mu[4] = mu[4] / tt2

mu
}

#retourne la classe retenue par le classifieur
#euclidien pour l’observation x.

regleEuclidienne <- function(x,mu1,mu2) 
{

if((abs(mu2[1]- x[1])) > (abs(mu1[1]) - x[1])){
return (1);
} else {
return (2);
}
}

#retourne la probabilité d’erreur estimée sur l’échantillon ech. 
#Utiliser dans cette fonction la commande apply de la manière suivante :
#classement=apply(ech,1,regle,mu1=mu1,mu2=mu2)

errEstimee <- function(ech,regle,mu1,mu2)
{
err = 0
classement=apply(ech,1,regle,mu1=mu1,mu2=mu2)
for(i in 1:nrow(ech)) {
	if(classement[i] != ech[i,1]){
		err = err+1
	}
}
err = err/nrow(ech) * 100
err
}

test = errEstimee(resultat1[(n*pi +1):n,], regleEuclidienne, )

errMoyenne <- function(n,pi,mu1,mu2,sigma1,sigma2,regle,intervalle){
result = matrix(c(0,0,0,0),1,4)
for (i in 1:10) {
	ech = simul(n,pi,mu1,mu2,sigma1,sigma2)
	esti = calculMu(ech[1:(n*pi),])
	e = erreurEstimee(ech[(n*pi +1):n,], regle, esti[,1], esti[,2])/100
	result[1] = result[1]+e
	result[2] = e^2 /10

}
#moyenne
result[1] = result[1] / 10

#variance
result[2] = result[2] - result[1]^2

#ecart-type
e = 10/(10-1) * result[2]

#intervalle
result[3] = result[1] - intervalle *e /sqrt(10)
result[4] = result[1] - intervalle *e /sqrt(10)

result

}

intervalle = 0.05
pi1 = errMoyenne(n,pi,mu1,mu2,Sigma1,Sigma2,regleEuclidienne,intervalle)

