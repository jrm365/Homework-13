library(dplyr)
library(caret)
library(nnet)

show_number <- function(m, i, oriented=T)
{
  im <- matrix(mtrain[i,], byrow=T, nrow=28)
  
  if (oriented) {
    im_orient <- matrix(0, nrow=28, ncol=28)
    for (i in 1:28)
      im_orient[i,] <- rev(im[,i])
    
    im <- im_orient
  }
  image(im)
}


# get the training datasets
if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header=F) %>% as.matrix
  train_classification <- mtrain[,1] #y values
  output_vector <- rep(NA, length(train_classification))
  
  for (i in 1:length(train_classification)) {
    c_number <- train_classification[i]
    if (c_number==3) {
       output_vector[i] <- 1
    } else {
      output_vector[i] <- 0 
    }
  }
  
  y <- factor(output_vector, levels=c(0,1))
  y <- y[1:1000]
  # for caret, y variable should be a factor
  # see line 54 in caret_intro_2d.R
  mtrain <- mtrain[,-1]/256 # x matrix
  
  colnames(mtrain) <- 1:(28^2)
  x <- mtrain[1:1000,]
  
  #colnames(mtrain) <- NULL
  #rownames(mtrain) <- NULL
}

# look at a sample
show_number(mtrain, 1)
