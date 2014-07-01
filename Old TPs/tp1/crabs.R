initcrabs<-function()
{
library(MASS)
data(crabs)
A <- crabs[,4:8]
A <- scale(A, center = TRUE, scale = FALSE)
S <- (1/200)*t(A)%*%A
S

#n <- dim(A)[1]
#S <- A-matrix(1,n,1)%*%apply(A,2,mean)
#S <- S/matrix(1,n,1)%*%apply(S,2,sd)
#S <- scale(A, center = TRUE, scale = FALSE)
#(1/n)*t(S)%*%S
}

#cor(crabsquant)
#crabsquant1 <- crabsquant[,2:5]
#crabsquant1 <- crabsquant1/crabsquant[,1]

#FL frontal lobe size (mm). 
#RW rear width (mm). 
#CL carapace length (mm). 
#CW carapace width (mm). 
#BD body depth (mm). 


#plot(crabsquant, col=crabs$sp)
#plot(crabsquant, col=crabs$sex)


splitsexandsp<-function(crabs)
{
struct <- NULL
struct$sexf <- crabs[crabs$sex == "F",]
struct$sexm <- crabs[crabs$sex == "M",]
struct$especeb <- crabs[crabs$sp == "B",]
struct$especeo <- crabs[crabs$sp == "O",]
struct
#graph qui compare les carac de chaque sex
#pareil mais pour chaque espèce
}

test <- function()
{
A <- matrix(c(3,4,3,1,4,3,2,3,6,2,1,4), nrow = 4, byrow=T)
A <- scale(A, center = TRUE, scale = FALSE)
S <- (1/4)*t(A)%*%A
#eigen(X) = diagonalise une matrice X
eigen(S)
v <- matrix(c(2,1,0.5))
diag(v) #trace de v
#P1 <- 
#P2 <- 
#P3 <- 
#P4 <- 
}
