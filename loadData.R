#Load images

#images <- read.table(url("http://www.loria.fr/~bougrain/pub/databases/train-labels.gz")); #Ne fonctionne pas

images <- read.table("C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz");

#Load labels
labels <- read.table("C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz");


#Normalsier entre -1 et 1 les valeurs des niveaux de gris
imagesNorm <- (images[,1:784]-128)/128;

#Tester avec la première image
images1 <- imagesNorm[1,];

#T(X)
input <- t(imagesNorm);

#Initialize weights with random values
weights <- runif(784, -2, 2);

#Seuil
seuil <- 0.3;

#Activation
a <- colSums(input * weights) - seuil;

#output
#fonction sigmoide f(a) = 1 / (1 + exp(-a)) f'(a) = f(a) * (1 - f(a))
output <- 1 / (1 + exp(-a));

#error entropie relative ou distance de Kullback-Leibler
#adapter labels 1 si 0 et 0 si [1-9] f(x) = a*ln(x/a) f'(x) = a²/x ? pas sur
error <- labels * ln(output / labels)

#error quadratique
#f(x) = 1/2 * sum((a - x) * (a - x)) f'(x) = -(a - x)

#coeff d'apprentissage
alpha <- 0.25;

#deltaW
delta <- - alpha * (labels - output) * input

#maj weights
weights <- weights + delta



