#Load images

#images <- read.table("http://www.loria.fr/~bougrain/pub/databases/train-images.gz", sep =" "); #Ne fonctionne pas

images <- read.table("C:/Users/jretterer/Desktop/data/train-images.txt", sep = " ");

#Load labels
labels <- read.table("C:/Users/jretterer/Desktop/data/train-labels.txt");

#Normalsier entre -1 et 1 les valeurs des niveaux de gris
imagesNorm <- (images[,1:784]-128)/128

#Tester avec la première image
images1 <- imagesNorm[1,]

#T(X)
tImages1 <- t(images1)

#Random values
random <- runif(784, -1, 1) 

#Multiplication de vecteurs
res1 <- tImages1 * random
res1

