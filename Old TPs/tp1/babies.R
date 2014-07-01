inittp<-function()
{
setwd("Z:/R/tp1")
babies<-read.table("babies23.txt",header=T)
babies<-babies[c(7,5,8,10,12,13,21,11)]
names(babies)<-c("bwt","gestation","parity","age","height","weight","smoke","education")

#Remplacer les codes des données non disponibles par NA (not available)

babies[babies$bwt == 999, 1] <- NA
babies[babies$gestation == 999, 2] <- NA
babies[babies$age == 99, 4] <- NA
babies[babies$height == 99, 5] <- NA
babies[babies$weight == 999, 6] <- NA
babies[babies$smoke == 9, 7] <- NA
babies[babies$education == 9, 8] <- NA

#Enfin, déclarer les variables quantitatives comme facteurs

babies$smoke<-factor(c("NonSmoking","Smoking","NonSmoking","NonSmoking")[babies$smoke+1])
babies$education<-factor(babies$education,ordered=T)

#Les variables disponibles deviennent alors :
#1. bwt : le poids de naissance (birth weight) en onces,
#2. gestation, la durée de la gestation en jours,
#3. parity : le nombre de grossesses précédentes,
#4. age : l’âge de la mère à la fin de la grossesse,
#5. height : la taille de la mère en pouces,
#6. weight : le poids de la mère en livres,
#7. smoke : est-ce-que la mère fume ? (0 : never ; 1 : smokes now; 2 : until current pregnancy ; 3 :
#once did, not now),
#8. ed : le niveau d’éducation de la mère (0 : less than 8th grade ; 1 : 8th to 12th grade 
#- did not graduate ; 2 : High School graduate, no other schooling ; 3 : High School + trade ;

babies

}

splitsmoke<-function(babies)
{
struct <- NULL
struct$smoking <- babies[babies$smoke == "Smoking",]
struct$nonsmoking <- babies[babies$smoke == "NonSmoking",]

#struct$bwtsmoke <- rnorm(struct$smoking$bwt)
#struct$bwtnonsmoke <- rnorm(struct$nonsmoking$bwt)

struct$bwtsmoke <- mean(struct$smoking$bwt, na.rm=T)
struct$bwtnonsmoke <- mean(struct$nonsmoking$bwt, na.rm=T)

#struct$gestsmoke <- rnorm(struct$smoking$gestation)
#struct$gestnonsmoke <- rnorm(struct$nonsmoking$gestation)

struct$gestsmoke <- mean(struct$smoking$gestation, na.rm=T)
struct$gestnonsmoke <- mean(struct$nonsmoking$gestation, na.rm=T)

struct$educsmoke <- mean(struct$smoking$education, na.rm=T)
struct$educnonsmoke <- mean(struct$nonsmoking$education, na.rm=T)

struct

#summary(res$nonsmoking)
#summary(res$smoking)
#boxplot(res$smoking$bwt, res$nonsmoking$bwt)
#boxplot(res$smoking$gestation, res$nonsmoking$gestation)

}

educationsmoke<-function(babies)
{

struct <- NULL
struct$education0 <- babies[babies$education == 0,]
struct$education1 <- babies[babies$education == 1,]
struct$education2 <- babies[babies$education == 2,]
struct$education3 <- babies[babies$education == 3,]
struct$education4 <- babies[babies$education == 4,]
struct$education5 <- babies[babies$education == 5,]
struct$education67 <- babies[(babies$education == 7 | babies$education == 6),]

struct

}
