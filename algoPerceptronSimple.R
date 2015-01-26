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
  return((1 - (x[1] + x[2]) / sum(x))*100);
}

precision <- function(x) {
  return((x[1]/(x[3]+x[1]))*100);
}

rappel <- function(x) {
  return((x[1]/(x[4]+x[1]))*100);
}

#Perceptron Simple avec fonction linéaire
perceptronSimple <- function(dataTrain = dataTrain, 
                             labelTrain = labelTrain, 
                             alpha = 0.25, 
                             nIters = 500, 
                             nSamples = 2000,
                             th = 0.3) {
  #Normalsier entre -1 et 1 les valeurs des niveaux de gris
  input <- t((dataTrain[,1:784]) / 255);
  
  label <- labelTrain * 2 - 1; #label = 1 si chiffre recherché, label = 0 sinon
  
  print(paste("Alpha = ", alpha, sep = ""))
  print(paste("NIters = ", nIters, sep = ""))
  print(paste("NSamples = ", nSamples, sep = ""))
  print(paste("Th = ", th, sep = ""))
  
  
  #Initialize weights and threshhold with random values
  w <- runif(784, -1, 1);
  #th <- runif(1, -1, 1);
  
  for ( i in 1:nIters ) {
    if (i %% 100 == 0) { #Affiche uniquement les multiple de 100 (donc 100/200/300/400/500 si 500 nIters)
      print(i)
    }
    for ( j in 1:nSamples ) {
      entries <- input[,j];
      a <- sum(entries * w) - th;
      if (a > 0) {
        x <- 1;
      }
      else {
        x <- -1;
      }
      if (labelTrain[j] != x) {
        #Weights Update
        w <- w + alpha * (labelTrain[j] - x) * entries; #erreur : (label[j] - x)
        #ThreshHold Update
        th <- th + alpha * (labelTrain[j] - x);
      }
    }
  }
  
  res <- colSums(input * w) - th;
  res <- (res + 1) / 2;
  res <- replace(res, which(res > 0), 1);
  res <- replace(res, which(res <= 0), 0);
  return(res);
}

#Perceptron Simple avec fonction sigmoide
perceptronSimpleSigmoide <- function(dataTrain = dataTrain, 
                                     labelTrain = labelTrain, 
                                     alpha = 0.25, 
                                     nIters = 500, 
                                     nSamples = 2000,
                                     th = 0.3,
                                     validation = 0.7) {
  #Normalsier entre -1 et 1 les valeurs des niveaux de gris
  input <- t((dataTrain[,1:784]) / 255);
  
  label <- labelTrain; #label = 1 si chiffre recherché, label = 0 sinon
  
  #Initialize weights and threshhold with random values
  w <- runif(784, -1, 1);
  debut <- runif(1, 1, nrow(dataTrain)-nSamples);
  
  for ( i in 1:nIters ) {
    if (i %% 100 == 0) {
      print(i)
    }
    for ( j in debut:nSamples ) {
    #for ( j in 1:nSamples ) {
      entries <- input[,j];
      a <- sum(entries * w) - th;
      x <- sigmoide(a);
      if (labelTrain[j] != x) {
        #Weights Update
        w <- w + alpha * (labelTrain[j] - x) * entries;
        #ThreshHold Update
        th <- th + alpha * (labelTrain[j] - x);
      }
    }
  }
  
  res <- colSums(input * w) - th;
  res <- replace(res, which(res > validation), 1);
  res <- replace(res, which(res <= validation), 0);
  return(list("res" = res,"w" = w));
}


#10 chiffres 10 perceptrons
reseau <- function(images = images, 
                   labels = labels, 
                   alpha = 0.25, 
                   nIters = 500, 
                   nSamples = 2000,
                   th = 0.3,
                   validation = 0.7) {
  
  print(paste("Alpha = ", alpha, sep = ""))
  print(paste("NIters = ", nIters, sep = ""))
  print(paste("NSamples = ", nSamples, sep = ""))
  print(paste("Th = ", th, sep = ""))
  
  wres <- c(); 
  
  for ( i in 0:9 ) {
    label <- setLabel(labels[[1]], i);
    result <- perceptronSimpleSigmoide(images, label,alpha,nIters,nSamples,th,validation);
    stats2 <- results(label, result$res);
    wres <- matrix(c(wres,result$w),10,784);
    cat(sprintf("Nombre %s : erreur %s°/., precision %s°/.,rappel %s°/.\n",i,errorRate(stats2),precision(stats2),rappel(stats2)))
  }
  
  return(wres);
}

#ecrit le fichier correspondant
ecrireFichier <- function(images = images, 
                          wres = wres,
                          nameFichier=nameFichier) {

  fileConn<-file(nameFichier);
  input <- t((images[,1:784]) / 255);
  res<-c();
  for ( i in 1:ncol(input) ) {
      entries <- input[,i];
      j <- 1;
      maxx <- 0;
      nbx <- 0;
      while ( j < 11 ) {
         a <- sum(entries * wres[j,]);
         x <- sigmoide(a);
         if(x > maxx){
           maxx <- x;
           nbx <- j - 1;
         }
        j <- j + 1;
       }
      res<-c(res,paste("",nbx));
  }
  writeLines(res, fileConn);
  close(fileConn);
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

#Perceptron

#lab <- 1;
#label <- setLabel(labels[[1]], lab);

#res1 <- perceptronSimple(images, label);
#stats1 <- results(label, res1);
#stats1
#errorRate(stats1)
#precision(stats1)
#rappel(stats1)

#res2$res <- perceptronSimpleSigmoide(images, label);
#stats2 <- results(label, res2$res);
#stats2
#errorRate(stats2)
#precision(stats2)
#rappel(stats2)

wres <- reseau(images, labels,nIters = 500,nSamples = 2000);

imagestest <-  read.table(file=file.choose());

#/Users/xaviereyl/Documents/RProject/test-labels.gz
ecrireFichier(imagestest, wres,"/Users/xaviereyl/Documents/RProject/test-labels.gz");

# a voir le random + function ecrireFichier a corriger...



