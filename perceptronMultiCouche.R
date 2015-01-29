
checkLab <- function(label, i, x) {
  if (label[i,] == x) {
    return(1);
  }
  else {
    return(0);
  }
}

#Perceptron Simple avec fonction sigmoide
train <- function(dataTrain = dataTrain,
                labelTrain = labelTrain,
                alpha = 0.25, 
                nIters = 500, 
                nSamples = 2000,
                th = 0.3,
                nNeurones = 4) {
  #Normalsier entre -1 et 1 les valeurs des niveaux de gris
  input <- t((dataTrain[,1:784]) / 255);
  
  #label <- labelTrain; #label = 1 si chiffre recherché, label = 0 sinon
  
  #Initialize weights and threshold with random values in the hidden layer
  w1 <- matrix(,784,nNeurones);
  dw1 <- w1;
  for ( i in 1:nNeurones ) {
    w1[,i] <- runif((784), -1, 1);
  }
  th1 <- rep(th, nNeurones);
  
  #Initialize weights and threshold with random values in the output layer
  w2 <- matrix(,nNeurones,10);
  dw2 <- w2;
  for ( i in 1:10 ) {
    w2[,i] <- runif((nNeurones), -1, 1);
  }
  th2 <- rep(th, 10); 
  
  #w <- runif(784, -1, 1);
  #debut <- runif(1, 1, nrow(dataTrain)-nSamples);
  
  for ( i in 1:nIters ) {
#     if (i %% 100 == 0) {
#       print(i)
#     }
    print(i)
    for ( j in 1:nSamples ) {
      x <- input[,j];
      
      a1 <- colSums(w1 * x) - th1;  
      #Fonction sigmoide
      y1 <- 1 / (1 + exp(-a1));
      
      #Fonction sigmoide
      a2 <- colSums(w2 * y1) - th2;
      y2 <- 1 / (1 + exp(-a2));
      
      #Couche de sortie
      #Pour chaque neurone en sortie
      #dW2 <- alpha * (t - y2) * y2 * (1 - y2) * y1;
      for ( k in 1:10 ) {
        #print(y2[[k]])
        dw2[,k] <- alpha * (checkLab(labels,j,k - 1) - y2[[k]]) * y2[[k]] * (1 - y2[[k]]) * y1;
        th2[k] <- th2[k] + alpha * (checkLab(labels,j,k - 1) - y2[[k]]) * y2[[k]] * (1 - y2[[k]]);
      }
      
      #Couche cachée
      #Pour chaque neurone de la couche cachée
      #dW1 <- - alpha * y1 * (1 - y1) * x * sum(-(t - y2) * w2);
      for ( k in 1:nNeurones ) {
        output <- rep(0, 10);
        for (l in 1:10) {
          output[l] <- checkLab(labels,j,l - 1); 
        }
        dw1[,k] <- - alpha * y1[[k]] * (1 - y1[[k]]) * x * sum(-(output - y2) * w2[k,]);
        th1[k] <- th1[k] - alpha * y1[[k]] * (1 - y1[[k]]) * 1 * sum(-(output - y2) * w2[k,]);
      }
      #Weights Update
      w1 <- w1 + dw1;
      w2 <- w2 + dw2;
    }
  }
  
#   res <- colSums(input * w) - th;
#   res <- replace(res, which(res > validation), 1);
#   res <- replace(res, which(res <= validation), 0);
  return(list("w1" = w1,"th1" = th1, "w2" = w2,"th2" = th2));
}

test <- function (dataTest = dataTest,
                  res = res, #Contient le résultat de la fonction train() soit une liste avec les poids et seuils
                  labelTest = 0) {
  w1 <- res$w1;
  th1 <- res$th1;
  w2 <- res$w2;
  th2 <- res$th2;
  
  input <- t((dataTest[,1:784]) / 255);
  
  len <- nrow(dataTest);
  
  y <- matrix(, len, 10);
  res <- rep(0, len);
  
  for (i in 1:len) {
    x <- input[,i];
    
    a1 <- colSums(w1 * x) - th1;  
    y1 <- 1 / (1 + exp(-a1));
    
    a2 <- colSums(w2 * y1) - th2;
    y[i,] <- 1 / (1 + exp(-a2));
    res[i] <- which.max(y[i,]) - 1;
  }
  
  if (labelTest == 0) { #Si on ne connais pas les labels
    return(list("res" = res, "stats" = 0));
  }
  else { #Si on veut évaluer le modèle
    c <- 0;
    for (i in 1:len) {
      if (res[i] == labelTest[i,]) {
        c <- c + 1;
      }
    }
    stats <- c / len;
    return(list("res" = res, "stats" = stats));
  }
}


#Load labels
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz"
#"C:/Users/jretterer/Desktop/data/train-labels.txt"
labels <- read.table(file=file.choose());

#Load images
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz"
#"C:/Users/jretterer/Desktop/data/train-images.txt"
images <-  read.table(file=file.choose());

res <- train(images, labels, nIters = 500);

res2 <- test(images[1:400,], res, labels[1:400,]);

res2$stats

# alpha <- 0.3;
# 
# w1 <- matrix(,784,4);
# dW1 <- w1;
# for ( i in 1:4 ) {
#   w1[,i] <- runif((784), -1, 1);
# }
# th1 <- rep(0.3, 4);
# 
# #Initialize weights and threshold with random values in the output layer
# w2 <- matrix(,4,10);
# dW2 <- w2;
# for ( i in 1:10 ) {
#   w2[,i] <- runif((4), -1, 1);
# }
# th2 <- rep(0.3, 10);

# input <- t((images[,1:784]) / 255);
#x <- input[,1];

# a1 <- colSums(w1 * x) - th1;
# 
# y1 <- 1 / (1 + exp(-a1));
#   
# a2 <- colSums(w2 * y1) - th2;
#   
# y2 <- 1 / (1 + exp(-a2));

#Couche de sortie
#Pour chaque neurone en sortie
#dW2 <- alpha * (t - y2) * y2 * (1 - y2) * y1;
# for ( i in 1:10 ) {
#   print(y2[[i]])
#   dW2[,i] <- alpha * (t(labels,1,i - 1) - y2[[i]]) * y2[[i]] * (1 - y2[[i]]) * y1;
# }

#Couche cachée
#Pour chaque neurone de la couche cachée
#dW1 <- - alpha * y1 * (1 - y1) * x * sum(-(t - y2) * w2);
# for ( i in 1:4 ) {
#   output <- rep(0, 10);
#   for (j in 1:10) {
#     output[j] <- t(labels,1,j - 1); 
#   }
#   dW1[,i] <- - alpha * y1[[i]] * (1 - y1[[i]]) * x * sum(-(output - y2) * w2[i,]);
# }
