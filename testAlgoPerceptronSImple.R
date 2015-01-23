#test algo perceptron simple

#############################
#Testing  ###################
#############################

#Normalsier entre -1 et 1 les valeurs des niveaux de gris
imagesNorm <- (images[,1:784]) / 255;
#T(X)
input <- t(imagesNorm);

#Initialize weights with random values
w <- runif(784, -1, 1);

#threshhold
th <- 0.3;

#coeff d'apprentissage
alpha <- 0.25;

#Nb itérations
N <- 500;
nEntries <- 2000;
for ( i in 1:N ) {
  print(i)
  for ( j in 1:nEntries ) {
    entries <- input[,j];
    a <- sum(entries * w) - th;
    if (a > 0) {
      x <- 1;
    }
    else {
      x <- -1;
    }
    if (label[j] != x) {
      w <- w + alpha * (label[j] - x) * entries;
    }
    
    #     output <- sigmoide(a);
    #     error <- errorQuadra(label[j], output);
    #     if (abs(label[j] - output) > 0.20 ) {
    #       b <- - (alpha * sigmoidePrime(a) * errorQuadraPrime(label[j], output));
    #       dW <- b * entries;
    #       w <- w + dW;
    #       th <- th + b;
    #     }
  }
  #   a <- colSums(input * weights) - threshhold;
  #   output <- sigmoide(a);
  #   error <- mean(errorQuadra(label, output));
  #   b <- - alpha * sigmoidePrime(a) * errorQuadraPrime(label, output);
  #   deltaW <- rowMeans(b * input);
  #   weights <- weights + deltaW;
  #   threshhold <- threshhold + mean(b);
}

res <- colSums(input * w) - th;
res <- replace(res, which(res > 0), 1);
res <- replace(res, which(res <= 0), 0);
res <- round(res, 2);
head(res,20)
head(label,20)
res

labels0

stats <- results(label[1:60000], res[1:60000])
errorRate(stats)
stats

#test functions
f <- function(a=a,x=0) {
  print(x)
}
f()
f(2,1)
f(x=2)
