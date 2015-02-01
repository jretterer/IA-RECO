
checkLab <- function(label, i, x) {
  if (label[i,] == x) {
    return(1);
  }
  else {
    return(0);
  }
}

#Perceptron multicouches avec fonction sigmoide
train <- function(dataTrain = dataTrain,
                labelTrain = labelTrain,
                alpha = 0.25, 
                nIters = 500, 
                nSamples = 2000,
                nNeurones = 4) {
  #Normalsier entre 0 et 1 les valeurs des niveaux de gris
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
  
  err <- rep(0, nIters);
  labels2 <- matrix(,nSamples,10);
  for ( i in 1:nSamples ) {
    for ( j in 1:10 ) {
      if (labels[i,] == (j - 1)) {
        labels2[i,j] <- 1;
      }
      else labels2[i,j] <- 0;
    }
  }
  y <- matrix(,nSamples,10);
  
  for ( i in 1:nIters ) {
    print(i)
    e <- 0;
    for ( j in 1:nSamples ) {
      x <- input[,j];
      
      a1 <- colSums(w1 * x) - th1;  
      #Fonction sigmoide
      y1 <- 1 / (1 + exp(-a1));
      
      #Fonction sigmoide
      a2 <- colSums(w2 * y1) - th2;
      y2 <- 1 / (1 + exp(-a2));
      
      y[j,] <- y2;
   
      output <- rep(0, 10);
      #Couche de sortie
      #Pour chaque neurone en sortie
      #dW2 <- alpha * (t - y2) * y2 * (1 - y2) * y1;
      for ( k in 1:10 ) {      
        output[k] <- checkLab(labels,j,k - 1); 
        #Avec fonction sigmoide
        dw2[,k] <- alpha * (output[k] - y2[[k]]) * y2[[k]] * (1 - y2[[k]]) * y1;
        th2[k] <- th2[k] + alpha * (output[k] - y2[[k]]) * y2[[k]] * (1 - y2[[k]]);
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
      
      e <- e + 1/2 * sum((labels2[j,] - y[j,]) * (labels2[j,] - y[j,]));
    }
    err[i] <- e;
  }

  return(list("w1" = w1,"th1" = th1, "w2" = w2,"th2" = th2, "err" = err));
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
    return(list("res" = res, "stats" = stats));
  }
}

#ecrit le fichier correspondant
ecrireFichier <- function(label = label, 
                          nameFichier=nameFichier) {
  
  fileConn<-file(nameFichier);
  res<-c();
  for ( i in 1:length(label) ) {    
    res<-c(res,paste(label[i],"", sep = ""));
  }
  writeLines(res, fileConn);
  close(fileConn);
}

################################################
#1-Charger les fonctions dans l'environnement R#
################################################

######################
#2-Charger les labels#
######################
labels <- read.table(file=file.choose());

######################
#3-Charger les images#
######################
images <-  read.table(file=file.choose());

###################
#4-Train le modèle#
###################
res1 <- train(images, labels, nIters = 100, nSamples = 10000, nNeurones = 11);

####################
#5-Tester le modèle#
####################
res2 <- test(dataTest = images[10000:60000,], res = res1, labelTest = labels[10000:60000,]);
res2$stats
res2$res

#######################################################
#6-Générer les labels correspondant aux images de test#
#######################################################
#Charger le fichier test-images
imagestest <-  read.table(file=file.choose());
#Tester le modèle calculé précédemment
res3 <- test(dataTest = imagestest, res = res1);
#Ecrire le résultat dans un fichier
ecrireFichier(res3$res,"test-labels.gz");
