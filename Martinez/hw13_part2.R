tuning_df <- data.frame(size=7:12, decay=0)

fitControl <- trainControl(method="none")

fitControl <- trainControl(##10-fold CV
  method = "repeatedcv",
  number = 2,
  ## repeated ten times
  repeats = 2)

t_out <- caret::train(x=x, y=y, method="nnet",
                      trControl = fitControl,
                      tuneGrid=tuning_df, maxit=100000, MaxNWts=100000)

prediction_errors <- function(x, y)
{
  true_y <- y
  pred_y <- predict(t_out, x)
  
  n_samples <- nrow(x)
  error <- sum(true_y != pred_y)/n_samples
  return (error)
}

prediction_errors(x,y)
