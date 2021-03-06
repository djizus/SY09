# UV : SY09 - TP01
# 1) Statistique descriptive
# Exercice 1 : Donn�es babies
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice1.R

setwd("C:/Users/sy09p039/Desktop/sy09_td01");
babies = read.table("babies23.txt", header = T)
babies = babies[c(7, 5, 8, 10, 12, 13, 21, 11)]
names(babies) = c("bwt", "gestation", "parity", "age", "height", "weight", "smoke", "education")

babies[babies$bwt == 999, 1] = NA
babies[babies$gestation == 999, 2] = NA
babies[babies$age == 99, 4] = NA
babies[babies$height == 99, 5] = NA
babies[babies$weight == 999, 6] = NA
babies[babies$smoke == 9, 7] = NA
babies[babies$education == 9, 8] = NA

babies$smoke = factor(c("NonSmoking", "Smoking", "NonSmoking", "NonSmoking")[babies$smoke+1])
babies$education = factor(babies$education, ordered = T)

# SELECT * FROM babies WHERE babies.smoke == 'Smoking';
# babies[babies$smoke == 'Smoking',]

# Question 1
# SELECT bwt FROM babies WHERE smoke == 'Smoking';
a = babies$bwt[babies$smoke == 'Smoking']
b = babies$bwt[babies$smoke == 'NonSmoking']

png(file = "img/boxplot_bwt_smoke.png")
boxplot(a, b, main = "Comparaison du poids des b�b�s � la naissance",  col = c("white", "red"), names = c("Fumeuses", "Non Fumeuses"), ylab = "bwt", notch = TRUE)
dev.off()

# Question 2
# SELECT gestation FROM babies WHERE smoke == 'Smoking'
c = babies$gestation[babies$smoke == 'Smoking']
d = babies$gestation[babies$smoke == 'NonSmoking']

png(file = "img/boxplot_gestation_smoke.png")
boxplot(c, d, main = "Comparaison du temps de gestation des m�res", col = c("white", "red"), names = c("Fumeuses", "Non Fumeuses"), ylab = "gestation", notch = TRUE)
dev.off()

# Question 3
NonSmoke = summary(babies$education[babies$smoke == 'NonSmoking'])
Smoke = summary(babies$education[babies$smoke == 'Smoking'])

png(file = "img/barplot_etude_smoke.png")
barplot(rbind(NonSmoke, Smoke), space = 0,legend = levels('Smoke'), main = "Comparaison du niveau d'�tudes des m�res", col = c('white','red'), xlab = "niveau d'�tude", ylab = "nombre d'individus", legend.text = T)
dev.off()
