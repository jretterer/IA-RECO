###########################
###########################

#algo
#Dit si le nombre manuscrit est un zero ou non

#functions
sigmoide <- function( x ) { #f(a) = 1 / (1 + exp(-a)) f'(a) = f(a) * (1 - f(a))
  return (1 / (1 + exp(-x)))
}

sigmoidePrime <- function( x ) {
  return (sigmoide(x) * (1 - sigmoide(x)))
}

errorQuadra <- function( t, y ) {
  return( 1/2 * sum((t - y) * (t - y)))
}

errorQuadraPrime <- function( t, y ) {
  return( - (t - y))
}

setLabel <- function( labels, x ) {
  if (x == 1) {
    label <- replace(labels, which(labels != 1), 0);
  }
  else if (x == 0) {
    label <- replace(labels, which(labels == 1), 2);
    label <- replace(label, which(label == 0), 1);
    label <- replace(label, which(label != 1), 0);
  }
  else {
    label <- replace(labels, which(labels != x), 0);
    label <- replace(label, which(label == x), 1);
  }
  return(label);
}

truePositives <- function ( t, y ) {
  
}

#Load results
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz"
labels <- read.table("C:/Users/jretterer/Desktop/data/train-labels.txt");
#Remplace les valeurs différentes de x par 0 et les x par 1
lab <- 1;
label <- setLabel(labels[[1]], lab);

#Load input
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz"
images <- read.table("C:/Users/jretterer/Desktop/data/train-images.txt");
#Normalsier entre -1 et 1 les valeurs des niveaux de gris
imagesNorm <- (images[,1:784] - 128) / 128;

imagesNorm <- (images[,1:784]) / 256;
#T(X)
input <- t(imagesNorm);

#Initialize weights with random values
eights <- runif(784, -2, 2);

#threshhold
threshhold <- 0.3;

#coeff d'apprentissage
alpha <- 0.25;

#Nb itérations
N <- 1;
nEntries <- 500;
for ( i in 1:N ) {
  print(i)
#   for ( j in 1:nEntries) {
#     print(j)
#     entries <- input[,j];
#     a <- sum(entries * weights) - threshhold;
#     print(a)
#     output <- sigmoide(a);
#     error <- errorQuadra(label[j], output);
#     b <- - alpha * sigmoidePrime(a) * errorQuadraPrime(label[j], output);
#     deltaW <- b * entries;
#     weights <- weights + deltaW;
#     threshhold <- threshhold + b;
#   }
  a <- colSums(input * weights) - threshhold;
  output <- sigmoide(a);
  error <- mean(errorQuadra(label, output));
  b <- - alpha * sigmoidePrime(a) * errorQuadraPrime(label, output);
  deltaW <- rowMeans(b * input);
  weights <- weights + deltaW;
  threshhold <- threshhold + mean(b);
}w

res <- sigmoide(colSums(input * weights) - threshhold);
res <- round(res, 2);
head(res,20)
head(label,20)
res

labels0