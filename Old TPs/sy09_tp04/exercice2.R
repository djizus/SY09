# UV : SY09 - TP04 : Analyses discriminante quadratique et lin�aire
# Exercice 2 : Analyse discriminante sur les donn�es Crabes
# Auteurs : Bertrand Bon - Antoine Hars
# Fichier : exercice2.R

library(MASS)

# 2.

couleur = c("blue", "red")
len = 50

crabs$FL1 = crabs$FL / (crabs$FL + crabs$RW + crabs$CL + crabs$CW + crabs$BD)
crabs$RW1 = crabs$RW / (crabs$FL + crabs$RW + crabs$CL + crabs$CW + crabs$BD)
D = crabs[ ,c(9, 10, 2)]

# Analyse discriminante lin�aire.
D.lda <- lda(D[ ,1:2], D$sex)

# Analyse discriminante quadratique.
D.qda <- qda(D[ ,1:2], D$sex)

x1p = seq(min(D$FL1), max(D$FL1), length = len)
x2p = seq(min(D$RW1), max(D$RW1), length = len)

grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

# Fronti�re de d�cision pour la lda
ly = predict(D.lda, grille)
lyp = ly$post[ ,1] - ly$post[ ,2]

# Fronti�re de d�cision de la qda
qy = predict(D.qda, grille)
qyp = qy$post[ ,1] - qy$post[ ,2]

png(file = "plots/exo2_analyse1.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Donnees Crabs selon le sexe")
contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'green')
contour(x1p, x2p, matrix(qyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'magenta')
dev.off()

# Calcul de l'erreur empirique lda sur l'ensemble d'apprentissage
n = dim(D)[1]
ly <- predict(D.lda, D[ ,1:2])
erreur.lda <- sum(ly$class != D$sex) / n

# Calcul de l'erreur empirique qda sur l'ensemble d'apprentissage
qy <- predict(D.qda, D[ ,1:2])
erreur.qda <- sum(qy$class != D$sex) / n

# 3.
# Fonction de cr�ation des �chantillons d'apprentissage et de test (2/3 au hasard).
ech_crabs1 <- function (data, n) {

	rand = sample(0:2, n, replace = TRUE)
	
	for (k in 1:n) {
		if (rand[k] == 2) {
			rand[k] = 0
		}
	}

	result = cbind(data[ ,1:3], rand)
	return (result)
}

repet_esti <- function (D, n, ft_ech) {

	result = matrix(c(0, 0, 0, 0), 4, 4, dimnames = list(c("a.lda", "a.qda", "t.lda", "t.qda"), c("1", "2", "3", "4")))
	
	# R�p�tition du processus de s�lection d'exemples et d'estimation d'erreur
	for (i in 1:4) {
		r = ft_ech(D, n)
		# 0 = �chantillon de test
		# 1 = �chantillon d'apprentissage

		# Estimation d'erreur sur l'�chantillon d'apprentissage
		a.lda = lda(r[r[ ,4] == 0, 1:2], r[r[ ,4] == 0, ]$sex)
		a.qda = qda(r[r[ ,4] == 0, 1:2], r[r[ ,4] == 0, ]$sex)

		x1p = seq(min(r[r[ ,4] == 0, ]$FL1), max(r[r[ ,4] == 0, ]$FL1), length = len)
		x2p = seq(min(r[r[ ,4] == 0, ]$RW1), max(r[r[ ,4] == 0, ]$RW1), length = len)

		grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))
		ly = predict(a.lda, grille)
		qy = predict(a.qda, grille)

		nn = dim(r[r[ ,4] == 0, ])[1]
		# Calcul de l'erreur empirique lda sur l'ensemble d'apprentissage
		ly <- predict(a.lda, r[r[ ,4] == 0, 1:2])
		
		a.lda_erreur <- sum(ly$class != r[r[ ,4] == 0, ]$sex) / nn

		# Calcul de l'erreur empirique qda sur l'ensemble d'apprentissage
		qy <- predict(a.qda, r[r[ ,4] == 0, 1:2])
		a.qda_erreur <- sum(qy$class != r[r[ ,4] == 0, ]$sex) / nn
		grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

		# Estimation d'erreur sur l'�chantillon de test
		t.lda = lda(r[r[ ,4] == 1, 1:2], r[r[ ,4] == 1, ]$sex)
		t.qda = qda(r[r[ ,4] == 1, 1:2], r[r[ ,4] == 1, ]$sex)

		x3p = seq(min(r[r[ ,4] == 1, ]$FL1), max(r[r[ ,4] == 1, ]$FL1), length = len)
		x4p = seq(min(r[r[ ,4] == 1, ]$RW1), max(r[r[ ,4] == 1, ]$RW1), length = len)

		grille = data.frame(expand.grid(FL1 = x3p, RW1 = x4p))
		ly = predict(t.lda, grille)
		qy = predict(t.qda, grille)
	
		nn = dim(r[r[ ,4] == 1, ])[1]
		# Calcul de l'erreur empirique lda sur l'ensemble d'apprentissage
		ly <- predict(t.lda, r[r[ ,4] == 1, 1:2])
		t.lda_erreur <- sum(ly$class != r[r[ ,4] == 1, ]$sex) / nn

		# Calcul de l'erreur empirique qda sur l'ensemble d'apprentissage
		qy <- predict(t.qda, r[r[ ,4] == 1, 1:2])
		t.qda_erreur <- sum(qy$class != r[r[ ,4] == 1, ]$sex) / nn
		
		result[1, i] = a.lda_erreur
		result[2, i] = a.qda_erreur
		result[3, i] = t.lda_erreur
		result[4, i] = t.qda_erreur

	}
#	dev.off()
	return(result)
}
question3 = repet_esti(D, n, ech_crabs1)


# 4.

# 1/2 au hasard
ech_crabs2 <- function (data, n) {

	rand = sample(0:1, n, replace = TRUE)
	result = cbind(data[ ,1:3], rand)
	return (result)
}
question4_1 = repet_esti(D, n, ech_crabs2)

# 4/5 au hasard
ech_crabs3 <- function (data, n) {

	rand = sample(0:5, n, replace = TRUE)
	
	for (k in 1:n) {
		if (rand[k] == 2) {
			rand[k] = 0
		}
		if (rand[k] == 3) {
			rand[k] = 0
		}
		if (rand[k] == 4) {
			rand[k] = 0
		}
		if (rand[k] == 5) {
			rand[k] = 0
		}
	}

	result = cbind(data[ ,1:3], rand)
	return (result)
}
question4_2 = repet_esti(D, n, ech_crabs3)




png(file = "plots/exo2_a_10.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Echantillon d'apprentissage 1")

for (i in 1:4) {
	r = ech_crabs1(D, n)

	# Estimation d'erreur sur l'�chantillon d'apprentissage
	a.lda = lda(r[r[ ,4] == 0, 1:2], r[r[ ,4] == 0, ]$sex)
	
	grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

	# Fronti�re de d�cision pour la lda
	ly = predict(a.lda, grille)
	lyp = ly$post[ ,1] - ly$post[ ,2]

	x1p = seq(min(r[r[ ,4] == 0, ]$FL1), max(r[r[ ,4] == 0, ]$FL1), length = len)
	x2p = seq(min(r[r[ ,4] == 0, ]$RW1), max(r[r[ ,4] == 0, ]$RW1), length = len)
	
	contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'green')

}
dev.off()

png(file = "plots/exo2_a_11.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Echantillon de test 1")

for (i in 1:4) {
	r = ech_crabs1(D, n)

	# Estimation d'erreur sur l'�chantillon d'apprentissage
	a.lda = lda(r[r[ ,4] == 1, 1:2], r[r[ ,4] == 1, ]$sex)
	
	grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

	# Fronti�re de d�cision pour la lda
	ly = predict(a.lda, grille)
	lyp = ly$post[ ,1] - ly$post[ ,2]

	x1p = seq(min(r[r[ ,4] == 1, ]$FL1), max(r[r[ ,4] == 1, ]$FL1), length = len)
	x2p = seq(min(r[r[ ,4] == 1, ]$RW1), max(r[r[ ,4] == 1, ]$RW1), length = len)
	
	contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'magenta')

}
dev.off()

png(file = "plots/exo2_a_20.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Echantillon d'apprentissage 2")

for (i in 1:4) {
	r = ech_crabs2(D, n)

	# Estimation d'erreur sur l'�chantillon d'apprentissage
	a.lda = lda(r[r[ ,4] == 0, 1:2], r[r[ ,4] == 0, ]$sex)
	
	grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

	# Fronti�re de d�cision pour la lda
	ly = predict(a.lda, grille)
	lyp = ly$post[ ,1] - ly$post[ ,2]

	x1p = seq(min(r[r[ ,4] == 0, ]$FL1), max(r[r[ ,4] == 0, ]$FL1), length = len)
	x2p = seq(min(r[r[ ,4] == 0, ]$RW1), max(r[r[ ,4] == 0, ]$RW1), length = len)
	
	contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'green')

}
dev.off()

png(file = "plots/exo2_a_21.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Echantillon de test 2")

for (i in 1:4) {
	r = ech_crabs2(D, n)

	# Estimation d'erreur sur l'�chantillon d'apprentissage
	a.lda = lda(r[r[ ,4] == 1, 1:2], r[r[ ,4] == 1, ]$sex)
	
	grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

	# Fronti�re de d�cision pour la lda
	ly = predict(a.lda, grille)
	lyp = ly$post[ ,1] - ly$post[ ,2]

	x1p = seq(min(r[r[ ,4] == 1, ]$FL1), max(r[r[ ,4] == 1, ]$FL1), length = len)
	x2p = seq(min(r[r[ ,4] == 1, ]$RW1), max(r[r[ ,4] == 1, ]$RW1), length = len)
	
	contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'magenta')

}
dev.off()

png(file = "plots/exo2_a_30.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Echantillon d'apprentissage 3")

for (i in 1:4) {
	r = ech_crabs3(D, n)

	# Estimation d'erreur sur l'�chantillon d'apprentissage
	a.lda = lda(r[r[ ,4] == 0, 1:2], r[r[ ,4] == 0, ]$sex)
	
	grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

	# Fronti�re de d�cision pour la lda
	ly = predict(a.lda, grille)
	lyp = ly$post[ ,1] - ly$post[ ,2]

	x1p = seq(min(r[r[ ,4] == 0, ]$FL1), max(r[r[ ,4] == 0, ]$FL1), length = len)
	x2p = seq(min(r[r[ ,4] == 0, ]$RW1), max(r[r[ ,4] == 0, ]$RW1), length = len)
	
	contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'green')

}
dev.off()

png(file = "plots/exo2_a_31.png")
plot(D[ ,1:2], col = couleur[D$sex], pch = 20, main = "Echantillon de test 3")

for (i in 1:4) {
	r = ech_crabs3(D, n)

	# Estimation d'erreur sur l'�chantillon d'apprentissage
	a.lda = lda(r[r[ ,4] == 1, 1:2], r[r[ ,4] == 1, ]$sex)
	
	grille = data.frame(expand.grid(FL1 = x1p, RW1 = x2p))

	# Fronti�re de d�cision pour la lda
	ly = predict(a.lda, grille)
	lyp = ly$post[ ,1] - ly$post[ ,2]

	x1p = seq(min(r[r[ ,4] == 1, ]$FL1), max(r[r[ ,4] == 1, ]$FL1), length = len)
	x2p = seq(min(r[r[ ,4] == 1, ]$RW1), max(r[r[ ,4] == 1, ]$RW1), length = len)
	
	contour(x1p, x2p, matrix(lyp, len), add = TRUE, levels = 0, drawlabels = FALSE, col = 'magenta')

}
dev.off()


# ANNEXE.
# crabs[1, ] donne la ligne 1
# et crabs[-1, ] nous donne la compl�mentaire de la ligne 1 (les lignes 2 � 200)

# point sur la fonction contour :
# tracer les courbes de niveau d'une fonction
# on a 2 classes en pr�sence avec une fonction discriminante pour chacune des classes
# quand on veut repr�senter une courbe de niveau des ces fonctions :
# on peut les tracer sous R.
# On prend une grille de repr�sentation (division de l'espace de repr�sentation) -> espace en 2 dimensions
# pour chaque point de la grille, on calcule la valeur prise par la fonction
# la fonction contour va regarder les points qui sont autour d'une valeur donn�e et va interpoler ces points pour les relier.
# Fonction surface pour faire la m�me chose en 3D.

# r[r[ ,4] == 0, 1:2]
# r[r[ ,4] == 1, 1:2]
# la dimension de l'�chantillon 1 dim(r[r[ ,4] == 1, 1:2])[1]
# la dimension de l'�chantillon 0 dim(r[r[ ,4] == 0, 1:2])[1]



