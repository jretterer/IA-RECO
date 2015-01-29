
checkLab <- function(label, i, x) {
  if (label[i,] == x) {
    return(1);
  }
  else {
    return(0);
  }
}

#compare 2 labels
compare <- function( labels, labels_res ) {
  nbcorrect <- 0;
  for (i in 1:nrow(labels)) {
    if (labels[i,] == labels_res[i,]) {
      nbcorrect <- nbcorrect + 1;
    }
  }
  return(nbcorrect);
}

#Perceptron Simple avec fonction sigmoide
train <- function(dataTrain = dataTrain,
                labelTrain = labelTrain,
                alpha = 0.25, 
                nIters = 500, 
                nSamples = 2000,
                nNeurones = 4) {
  #Normalsier entre -1 et 1 les valeurs des niveaux de gris
  input <- t((dataTrain[,1:784]) / 255);
  
  #label <- labelTrain; #label = 1 si chiffre recherché, label = 0 sinon
  
  #Initialize weights and threshold with random values in the hidden layer
  w1 <- matrix(,784,nNeurones);
  dw1 <- w1;
  for ( i in 1:nNeurones ) {
    w1[,i] <- runif(784, -1, 1);
  }
  th1 <- runif(nNeurones, -1, 1);
  
  #Initialize weights and threshold with random values in the output layer
  w2 <- matrix(,nNeurones,10);
  dw2 <- w2;
  for ( i in 1:10 ) {
    w2[,i] <- runif((nNeurones), -1, 1);
  }
  th2 <- runif(10, -1, 1);
  
  for ( i in 1:nIters ) {
#     if (i %% 100 == 0) {
#       print(i)
#     }
    print(i)
    print(th2)
    print(th1)
    for ( j in 1:nSamples ) {
      x <- input[,j];
      
      a1 <- colSums(w1 * x) - th1;  
      #Fonction sigmoide
      y1 <- 1 / (1 + exp(-a1));
      
      #Fonction sigmoide
      a2 <- colSums(w2 * y1) - th2;
      y2 <- 1 / (1 + exp(-a2));
   
      
      output <- rep(0, 10);
      #Couche de sortie
      #Pour chaque neurone en sortie
      #dW2 <- alpha * (t - y2) * y2 * (1 - y2) * y1;
      for ( k in 1:10 ) {
#         if (a2[[k]] >= 0) {
#           y2[[k]] <- 1;
#         }
#         else {
#           y2[[k]] <- 0;
#         }
        
        output[k] <- checkLab(labels,j,k - 1); 
        #print(y2[[k]])
        #Avec fonction sigmoide
        dw2[,k] <- alpha * (output[k] - y2[[k]]) * y2[[k]] * (1 - y2[[k]]) * y1;
        th2[k] <- th2[k] + alpha * (output[k] - y2[[k]]) * y2[[k]] * (1 - y2[[k]]);
        #Avec fonction linéaire
#         dw2[,k] <- alpha * (output[k] - y2[[k]]) * y1;
#         th2[k] <- th2[k] + alpha * (output[k] - y2[[k]]);
      }
      
      
      #Couche cachée
      #Pour chaque neurone de la couche cachée
      #dW1 <- - alpha * y1 * (1 - y1) * x * sum(-(t - y2) * w2);
      for ( k in 1:nNeurones ) {
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
                  labelTest = NULL) {
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
#     for ( k in 1:10 ) {
#       if (a2[[k]] >= 0) {
#         y[i,k] <- 1;
#       }
#       else {
#         y[i,k] <- 0;
#       }
#     }
  }
  
  if (is.null(labelTest)) { #Si on ne connais pas les labels
    return(list("res" = res, "stats" = 0));
  }  
  else { #Si on veut évaluer le modèle
    c <- 0;
    for (i in 1:len) {
      if (res[i] == labelTest[i]) {
        c <- c + 1;
      }
    }
    stats <- c / len;
    return(list("res" = res, "stats" = stats, "y" = y));
  }
}

#ecrit le fichier correspondant
ecrireFichier <- function(label = label, 
                          nameFichier=nameFichier) {
  
  fileConn<-file(nameFichier);
  res<-c();
  for ( i in 1:length(label) ) {    
    res<-c(res,paste("",label[i]));
  }
  writeLines(res, fileConn);
  close(fileConn);
}

#Load labels
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-labels.gz"
#"C:/Users/jretterer/Desktop/data/train-labels.txt"
labels <- read.table(file=file.choose());

#Load images
#path "C:/Users/Utilisateur/Desktop/IAData/ia/train-images.gz"
#"C:/Users/jretterer/Desktop/data/train-images.txt"
images <-  read.table(file=file.choose());

res1 <- train(images, labels, nIters = 100, nSamples = 10000, nNeurones = 11);

#comparaison sur 400
res2 <- test(dataTest = images[10000:10400,], res = res1, labelTest = labels[10000:10400,]);
res2$stats
res2$res
res2$y

#comparaison sur tout le train
res3 <- test(dataTest = images[0:60000,], res = res1, labelTest = labels[0:60000,]);
res3$stats
ecrireFichier(res3$res,"/Users/xaviereyl/Documents/RProject/train-labels-multi.gz");#reecrit pour vérifier
labels_rendu <- read.table(file=file.choose());
#et on compare
nombre <- compare(labels_rendu, labels);
cat(sprintf(" %s nombres justes sur %s\n",nombre,nrow(labels)))

#ecriture du fichier test_label
imagestest <-  read.table(file=file.choose());
res4 <- test(dataTest = imagestest[0:10000,], res = res1);
ecrireFichier(res4$res,"/Users/xaviereyl/Documents/RProject/test-labels-multi.gz");


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
