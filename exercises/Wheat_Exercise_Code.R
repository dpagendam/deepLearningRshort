library(remotes)
remotes::install_github("dpagendam/deepLearningRshort")
library(deepLearningRshort)
#For those using Colab run: install.packages("keras")
data("wheat")


dim(trainData_X)
dim(testData_X)



rescaleCols <- function(rowX, colMins, colMaxs)
{
  r <- (rowX - colMins)/(colMaxs - colMins)
  r[is.nan(r)] <- 0
  return(r)
}



colMinsX <- apply(trainData_X, 2, min)
colMaxsX <- apply(trainData_X, 2, max)




trainData_X_scaled <- t(apply(trainData_X, 1, rescaleCols, 
                              colMinsX, colMaxsX))
testData_X_scaled <- t(apply(testData_X, 1, rescaleCols, 
                             colMinsX, colMaxsX))
                             
                             
                             
                             
trainData_Y = matrix(trainData_Y[, "wheatTotalWeight"], ncol = 1)
testData_Y = matrix(testData_Y[, "wheatTotalWeight"], ncol = 1)




library(keras)
model <- keras_model_sequential()
model %>% layer_dense(units = 64, activation = "relu", 
          input_shape = ncol(trainData_X_scaled)) %>% 
layer_dense(units = 64, activation = "relu") %>%
layer_dense(units = 64, activation = "relu") %>% 
layer_dense(units = 1)



model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(learning_rate = 0.0001)
)



history <- model %>% fit(
  x = trainData_X_scaled, y = trainData_Y,
  epochs = 30, batch_size = 32, 
  validation_data = list(testData_X_scaled, testData_Y)
)




plot(history)





pred_test <- predict(model, testData_X_scaled)
plot(testData_Y, pred_test, xlab = " True Plant Biomass (kg/ha)",
     ylab = " Predicted Plant Biomass (kg/ha)", pch = 20, 
     col = adjustcolor("black", 0.2))
abline(0, 1, col = "red")





negLL_logNormal <- function(y_true, y_pred)
{
  K <- backend()
  # Set up muMask and sigma Mask as 2 x 1 matrices.
  muMask <- K$constant(matrix(c(1, 0), 2, 1), shape = c(2, 1))
  sigmaMask <- K$constant(matrix(c(0, 1), 2, 1), shape = c(2, 1))
  
  # Extract the first and second columns
  mu <- K$dot(y_pred, muMask)
  sigma <- K$exp(K$dot(y_pred, sigmaMask))
  
  # Use mu and sigma as parameters describing log-normal distributions
  logLike <- -1*(K$log(y_true) + K$log(sigma)) -
  0.5*K$log(2*pi) -
  K$square(K$log(y_true) - mu)/(2*K$square(sigma))
  -1*(K$sum(logLike, axis = 1L))
}






model <- keras_model_sequential()
model %>% layer_dense(units = 64, activation = "relu", 
          input_shape = ncol(trainData_X_scaled)) %>% 
layer_dense(units = 64, activation = "relu") %>%
layer_dense(units = 64, activation = "relu") %>% 
layer_dense(units = 2)





model %>% compile(
  loss = negLL_logNormal,
  optimizer = optimizer_rmsprop(learning_rate = 0.0001)
)




history <- model %>% fit(
  x = trainData_X_scaled, y = trainData_Y,
  epochs = 50, batch_size = 32, 
  validation_data = list(testData_X_scaled, testData_Y),
  callbacks = list(callback_reduce_lr_on_plateau(monitor = "val_loss",
              factor = 0.25, patience = 5))
)





plot(history)





yhat <- predict(model, testData_X_scaled)
mu <- yhat[, 1]
sigma <- exp(yhat[, 2])
pred_mean <- exp(mu + 0.5*sigma^2)




plot(testData_Y, pred_mean, xlab = " True Plant Biomass (kg/ha)",
     ylab = " Predicted Plant Biomass (kg/ha)", xlim = c(0, 3000),
     ylim = c(0, 3000), pch = 20, col = adjustcolor("black", 0.2))
abline(0, 1, col = "red")




lower95 <- qlnorm(0.025, meanlog = mu, sdlog = sigma)
upper95 <- qlnorm(0.975, meanlog = mu, sdlog = sigma)
coverage95 <- sum(testData_Y > lower95 & 
                    testData_Y < upper95)/length(testData_Y)
print(coverage95)




lower50 <- qlnorm(0.25, meanlog = mu, sdlog = sigma)
upper50 <- qlnorm(0.75, meanlog = mu, sdlog = sigma)
coverage50 <- sum(testData_Y > lower50 & 
                    testData_Y < upper50)/length(testData_Y)
print(coverage50)




xgrid <- seq(0, 2000, 10)
plotIndex = 123
yieldDist <- dlnorm(xgrid, meanlog = mu[plotIndex],
                    sdlog = sigma[plotIndex])
plot(xgrid, yieldDist, ty = "l", 
     main = paste0("Val. Sample ", plotIndex), xlab = "Biomass (kg/ha)",
     ylab = "Probability Density")
points(x = testData_Y[plotIndex], y = 0, col = "red")




plotIndex = 321
yieldDist <- dlnorm(xgrid, meanlog = mu[plotIndex], 
                    sdlog = sigma[plotIndex])
plot(xgrid, yieldDist, ty = "l", 
     main = paste0("Val. Sample ", plotIndex), xlab = "Biomass (kg/ha)",
     ylab = "Probability Density")
points(x = testData_Y[plotIndex], y = 0, col = "red")




