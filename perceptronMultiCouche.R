#Perceptron Simple avec fonction sigmoide
PMC <- function(dataTrain = dataTrain,
                labelTrain = labelTrain,
                alpha = 0.25, 
                nIters = 500, 
                nSamples = 2000,
                th = 0.3,
                validation = 0.7,
                nNeurones = 4) {
  #Normalsier entre -1 et 1 les valeurs des niveaux de gris
  input <- t((dataTrain[,1:784]) / 255);
  
  label <- labelTrain; #label = 1 si chiffre recherché, label = 0 sinon
  
  #Initialize weights and threshold with random values in the hidden layer
  w1 <- matrix(,784,nNeurones);
  for ( i in 1:nNeurones ) {
    w1[,i] <- runif((784), -1, 1);
  }
  th1 <- th;
  
  #Initialize weights and threshold with random values in the output layer
  w2 <- matrix(,784,10);
  for ( i in 1:10 ) {
    w2[,i] <- runif((784), -1, 1);
  }
  th2 <- th;
  
  #w <- runif(784, -1, 1);
  #debut <- runif(1, 1, nrow(dataTrain)-nSamples);
  
  for ( i in 1:nIters ) {
    if (i %% 100 == 0) {
      print(i)
    }
    for ( j in 1:nSamples ) {
      #for ( j in 1:nSamples ) {
      entries <- input[,j];
      for ( k in 1:nNeurones ) {
        
      }
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


#Load labels
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz"
#"C:/Users/jretterer/Desktop/data/train-labels.txt"
labels <- read.table(file=file.choose());

#Load images
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz"
#"C:/Users/jretterer/Desktop/data/train-images.txt"
images <-  read.table(file=file.choose());

w1 <- matrix(,784,4);
for ( i in 1:4 ) {
  w1[,i] <- runif((784), -1, 1);
}

entries <- images[1,];

a <- sum(entries * w1) - 0.3