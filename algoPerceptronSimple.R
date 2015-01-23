###########################
###########################

#algo

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

results <- function ( t, y ) {
  tp <- 0;
  tn <- 0;
  fp <- 0;
  fn <- 0;
  for (i in 1:length(t)) {
    if (y[i] == t[i] & y[i] == 1) {
      tp <- tp + 1;
    }
    else if (y[i] == t[i] & y[i] == 0) {
      tn <- tn + 1;
    }
    else if (y[i] != t[i] & y[i] == 1) {
      fp <- fp + 1;
    }
    else {
      fn <- fn + 1;
    }
  }
  
  return(c(tp, tn, fp, fn));
}

errorRate <- function (x) {
  return(1 - (x[1] + x[2]) / sum(x));
}

perceptronSimple <- function(dataTrain = dataTrain, labelTrain = labelTrain, lab = 0, th = 0.3, alpha = 0.25, nIters = 500, nSamples = 2000) {
  #Normalsier entre -1 et 1 les valeurs des niveaux de gris
  imagesNorm <- (dataTrain[,1:784]) / 255;
  #T(X)
  input <- t(imagesNorm);
  
  label <- setLabel(labelTrain[[1]], lab);
  
  #Initialize weights with random values
  w <- runif(784, -1, 1);
  
  for ( i in 1:nIters ) {
    print(i)
    for ( j in 1:nSamples ) {
      entries <- input[,j];
      a <- sum(entries * w) - th;
      if (a > 0) {
        x <- 1;
      }
      else {
        x <- -1;
      }
      if (label[j] != x) {
        w <- w + alpha * (label[j] - x) * entries; #erreur : (label[j] - x)
      }
    }
  }
  
  res <- colSums(input * w) - th;
  res <- replace(res, which(res > 0), 1);
  res <- replace(res, which(res <= 0), 0);
  return(res);
}

############################
#Perceptron simple
############################

#Load labels
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz"
#"C:/Users/jretterer/Desktop/data/train-labels.txt"
labels <- read.table(file=file.choose());

#Load images
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz"
#"C:/Users/jretterer/Desktop/data/train-images.txt"
images <-  read.table(file=file.choose());

#perceptron simple
lab <- 1;
res <- perceptronSimple(images, labels, lab);
label <- setLabel(labels[[1]], lab);
stats <- results(label[1:60000], res[1:60000])
errorRate(stats)
stats
stats[1]/(stats[3]+stats[1])#precision
stats[1]/(stats[4]+stats[1])#rappel
#return(c(tp, tn, fp, fn));

