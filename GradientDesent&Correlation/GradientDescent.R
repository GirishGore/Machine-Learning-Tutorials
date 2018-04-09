attach(mtcars)
plot(disp, mpg, col = "blue", pch = 13)
model <- lm(mpg ~ disp, data = mtcars)
summary(model)
y_preds <- predict(model)
abline(model)

errors <- unname((mpg - y_preds) ^ 2)
sum(errors) / length(mpg)

m <- runif(1, 0, 30)
length(disp)

gradientDescent <- function(x, y, learning_rate, threshold, max_iter) {
  
  plot(x, y, col = "blue", pch = 5)
  ## Initialize the slope and intercept.
  m <- runif(1, 0, 1)
  c <- runif(1, 25, 30)
  
  ## Linear Equation given
  ypred <- m * x + c
  
  ## Getting Number of records
  n = length(x)
  
  ## Mean Standard Error = Sum of squared errors / Total Number
  MSE <- sum((y - ypred) ^ 2) / n
  
  ## Flag to denote convergence and iteration counter
  converged = FALSE
  iterations = 0
  
  
  while(converged == FALSE) {
    ## Implement the gradient descent algorithm
    m <- m - learning_rate * ((1 / n) * (sum((ypred - y) * x)))
    c <- c - learning_rate * ((1 / n) * (sum(ypred - y)))
    
    ## Update Ypred 
    ypred <- m * x + c
    MSE_new <- sum((y - ypred) ^ 2) / n
    if(MSE - MSE_new <= threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = TRUE
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
}

# Run the function 
gradientDescent(disp, mpg, 0.0000293, 0.001, 2500000)
coef(model)

