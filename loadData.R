#Load images

#images <- read.table(url("http://www.loria.fr/~bougrain/pub/databases/train-labels.gz")); #Ne fonctionne pas

images <- read.table("C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz");

#Load labels
labels <- read.table("C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz");


#Normalsier entre -1 et 1 les valeurs des niveaux de gris
imagesNorm <- (images[,1:784]-128)/128;

#Tester avec la première image
#images1 <- imagesNorm[1,];

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

###########################
###########################

#algo
#Dit si le nombre manuscrit est un zero ou non

#functions
sigmoide <- function( x ) { #f(a) = 1 / (1 + exp(-a)) f'(a) = f(a) * (1 - f(a))
  return (1 / (1 + exp(-x)))
}

sigmoidePrime <- function( x ) {
  return (sigmoide(a) * (1 - sigmoide(a)))
}

errorQuadra <- function( t, y ) {
  return( 1/2 * sum((t - y) * (t - y)))
}

errorQuadraPrime <- function( t, y ) {
  return( - (t - y))
}

#Load results
labels <- read.table("C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz");
#Remplace les valeurs différentes de 0 par 0 et les 0 par 1
labels0 <- replace(labels$V1, which(labels$V1 == 1), 2);
labels0 <- replace(labels0, which(labels0 == 0), 1);
labels0 <- replace(labels0, which(labels0 != 1), 0);

#Load input
images <- read.table("C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz");
#Normalsier entre -1 et 1 les valeurs des niveaux de gris
imagesNorm <- (images[,1:784]-128)/128;
#T(X)
input <- t(imagesNorm);

#Initialize weights with random values
weights <- runif(784, -2, 2);

#Seuil
seuil <- 0.3;

#coeff d'apprentissage
alpha <- 0.25;

#Nb itérations
N <- 500;
for ( i in 1:N ) { 
  for ( j in 1:60000) {
    entries <- input[,j];
    a <- sum(entries * weights) - seuil;
    output <- sigmoide(a);
    error <- errorQuadra(labels0[j], output);
    deltaW <- - alpha * sigmoidePrime(output) * errorQuadraPrime(labels0[j], output) * entries;
    weights <- weights + deltaW;
    seuil <- seuil + (- alpha * sigmoidePrime(output) * errorQuadraPrime(labels0[j], output));
  }
  #a <- colSums(input * weights) - seuil;
  #output <- sigmoide(a);
  #error <- errorQuadra(labels0, output) / 60000;
  #deltaW <- - alpha * sigmoidePrime(output) * errorQuadraPrime(labels0, output) * input;
  #weights <- weights + deltaW;
  #seuil <- seuil + - alpha * sigmoidePrime(output) * errorQuadraPrime(labels0, output)
}

res <- sigmoide(rowSums(t(input) * weights) - seuil);
res <- round(res, 2);
res

labels0

