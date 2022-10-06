
install.packages("raster", repos = "http://cran.us.r-project.org", 
                 quiet = TRUE)
library(raster)
remotes::install_github("dpagendam/deepLearningRshort")
library(deepLearningRshort)
#For those using Colab run: install.packages("keras")
library(keras)




data(drought)



plot(anomalyRasterList[[1]], main = paste0("Sea Surface Temperature Anomaly (", dateGrid[1, 1], "/", dateGrid[1, 2], ")"),
  xlab = "Longitude", ylab = "Latitude", zlim = c(-5, 5))
  
  
  


batchSize <- 32
forecastMonthsAhead <- 3
timestepsPerSample <- 24
trainingInds <- 1:1300
testInds <- 1301:1434





numComponents <- 100
EOFList <- rasterToEOFs(anomalyRasterList[trainingInds], 
           numComponents = numComponents, plot = FALSE)
v.train <- EOFList[["rasterEOFs"]][["v.dim.red"]]




EOFList <- rasterToEOFs(anomalyRasterList[trainingInds], numComponents = numComponents, plot = TRUE)




validPixels <- EOFList[["raster.validPixels"]]
eof1 <- EOFsToRaster(matrix(EOFList$rasterEOFs$EOFs[, 1], ncol = 1),
        matrix(rep(1, 1), nrow = 1), c(33, 84),
        validPixels)[[1]]
extent(eof1) <- extent(anomalyRasterList[[1]])
plot(eof1, main = "1st EOF", xlab = "Longitude", ylab = "Latitude")




eof2 <- EOFsToRaster(matrix(EOFList$rasterEOFs$EOFs[, 2], ncol = 1),
        matrix(rep(1, 1), nrow = 1), c(33, 84),
        validPixels)[[1]]
extent(eof2) <- extent(anomalyRasterList[[1]])
plot(eof2, main = "2nd EOF", xlab = "Longitude", ylab = "Latitude")




eof3 <- EOFsToRaster(matrix(EOFList$rasterEOFs$EOFs[, 3], ncol = 1),
        matrix(rep(1, 1), nrow = 1), c(33, 84),
        validPixels)[[1]]
extent(eof3) <- extent(anomalyRasterList[[1]])
plot(eof3, main = "3rd EOF", xlab = "Longitude", ylab = "Latitude")




eof100 <- EOFsToRaster(matrix(EOFList$rasterEOFs$EOFs[, 100], ncol = 1),
        matrix(rep(1, 1), nrow = 1), c(33, 84),
        validPixels)[[1]]
extent(eof100) <- extent(anomalyRasterList[[1]])
plot(eof100, main = "100th EOF", xlab = "Longitude", ylab = "Latitude")





testSample <- 1434
X <- EOFList$rasterEOFs$EOFs
r1 <- anomalyRasterList[[testSample]]
validPixels <- EOFList[["raster.validPixels"]]
Y <- getValues(r1)
Y <- Y[validPixels]
lm1 <- lm(Y~X)
intercept <- coefficients(lm1)[1]
alpha <- coefficients(lm1)[1]
beta <- coefficients(lm1)[2:(numComponents + 1)]
r2 <- alpha + EOFsToRaster(X, matrix(beta, nrow = 1),
      c(33, 84), validPixels)[[1]]
extent(r2) <- extent(r1)






par(mfrow = c(1, 2))
plot(r1, xlab = "Longitude", ylab = "Latitude", zlim = c(-5, 5))
plot(r2, xlab = "Longitude", ylab = "Latitude", zlim = c(-5, 5))





v.test <- proj.raster.EOFs(anomalyRasterList[testInds],
         EOFList[["rasterEOFs"]][["EOFs"]],
         EOFList[["raster.validPixels"]])
         
         
         
         
par(mfrow = c(3, 1), mar = c(4,4,1,1))
plot(trainingInds, v.train[, 1], ty = "l", xlim = c(0, 1434),
     xlab = "Months", ylab = "Variable 1")
lines(testInds, v.test[, 1], col = "blue")
legend("topleft", legend = c("train", "test"), horiz = TRUE,
       box.lwd = 0, col = c("black", "blue"), lty = 1)
plot(trainingInds, v.train[, 2], ty = "l", xlim = c(0, 1434),
     xlab = "Months", ylab = "Variable 2")
lines(testInds, v.test[, 2], col = "blue")
legend("topleft", legend = c("train", "test"), horiz = TRUE,
       box.lwd = 0, col = c("black", "blue"), lty = 1)
plot(trainingInds, v.train[, 3], ty = "l", xlim = c(0, 1434),
     xlab = "Months", ylab = "Variable 3")
lines(testInds, v.test[, 3], col = "blue")
legend("topleft", legend = c("train", "test"), horiz = TRUE,
       box.lwd = 0, col = c("black", "blue"), lty = 1)
       
       
       
       
par(mfrow = c(3, 1), mar = c(4,4,1,1))
plot(trainingInds, v.train[, 1], ty = "l", xlim = c(0, 1434),
     xlab = "Months", ylab = "Variable 1")
lines(testInds, v.test[, 1], col = "blue")
legend("topleft", legend = c("train", "test"), horiz = TRUE,
       box.lwd = 0, col = c("black", "blue"), lty = 1)
plot(trainingInds, v.train[, 2], ty = "l", xlim = c(0, 1434),
     xlab = "Months", ylab = "Variable 2")
lines(testInds, v.test[, 2], col = "blue")
legend("topleft", legend = c("train", "test"), horiz = TRUE,
       box.lwd = 0, col = c("black", "blue"), lty = 1)
plot(trainingInds, v.train[, 3], ty = "l", xlim = c(0, 1434),
     xlab = "Months", ylab = "Variable 3")
lines(testInds, v.test[, 3], col = "blue")
legend("topleft", legend = c("train", "test"), horiz = TRUE,
       box.lwd = 0, col = c("black", "blue"), lty = 1)
       
       
       
       

v.combined <- rbind(v.train, v.test)





v.scaling.train <- scaleCols.pos(v.combined[trainingInds, ])
v.train.scaled <- v.scaling.train[["X.scaled"]]
v.scaling.test <- scaleCols.pos(v.combined[testInds, ],
                  colMaxsX = v.scaling.train[["colMaxsX"]],
                  colMinsX = v.scaling.train[["colMinsX"]])
v.test.scaled <- v.scaling.test[["X.scaled"]]

v.scaled <- rbind(v.train.scaled, v.test.scaled)





numDims <- ncol(v.scaled)
tensorData <- tensorfyData.rnn(v.scaled, forecastMonthsAhead,
              timestepsPerSample, indicesX = 1:numDims,
              indicesY = 1:numComponents, indicesTrain = trainingInds,
              indicesTest = testInds)
str(tensorData)





Y.train.inds <- tensorData$y.train.tsInds
Y.test.inds <- tensorData$y.test.tsInds
Y.train.rnn_MDB <- rainfallAnomaly[Y.train.inds, 3]
Y.test.rnn_MDB <- rainfallAnomaly[Y.test.inds, 3]
Y.train.min <- min(Y.train.rnn_MDB)
Y.train.max <- max(Y.train.rnn_MDB)

Y.train.rnn_MDB <- (Y.train.rnn_MDB - Y.train.min)/
                    (Y.train.max - Y.train.min)
Y.test.rnn_MDB <- (Y.test.rnn_MDB - Y.train.min)/
                    (Y.train.max - Y.train.min)
                    
                    
                    

tensorData[["Y.train.rnn"]] <- Y.train.rnn_MDB
tensorData[["Y.test.rnn"]] <- Y.test.rnn_MDB





X.rnn.train <- tensorData[["X.train.rnn"]]
X.rnn.test <- tensorData[["X.test.rnn"]]

Y.rnn.train <- tensorData[["Y.train.rnn"]]
Y.rnn.test <- tensorData[["Y.test.rnn"]]





Gaussian_logLikelihood <- function(y_true, y_pred)
{
  K <- backend()
  muMask <- K$constant(c(1, 0), shape = c(2, 1))
  sigmaMask <- K$constant(c(0, 1), shape = c(2, 1))

  sigma <- K$exp(K$dot(y_pred, sigmaMask))
  mu <- K$dot(y_pred, muMask)
  
  ll <- -0.5*K$square((mu - y_true)/(sigma)) - K$log(sigma)
  -1*K$sum(ll, axis = 1L)
}





library(keras)
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 128,
             input_shape = c(timestepsPerSample, numDims),
             return_sequences = FALSE,
             stateful = FALSE) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 2)
  
  
  



model %>% compile(loss = Gaussian_logLikelihood,
                  optimizer = optimizer_rmsprop())
                  
                  
                  


history <- model %>% fit(x = X.rnn.train, y = Y.rnn.train, 
           batch_size = batchSize, epochs = 200, shuffle = TRUE, 
           validation_data = list(X.rnn.test, Y.rnn.test),
           callbacks = list(callback_early_stopping(
           monitor = "val_loss", min_delta = 0, patience = 20),
           callback_model_checkpoint(filepath = "MDB_Gaussian.hd5",
           save_best_only = TRUE, save_weights_only = FALSE)))

bestModel <- load_model_hdf5(filepath = "MDB_Gaussian.hd5",
             custom_objects = list(Gaussian_logLikelihood = 
             Gaussian_logLikelihood))
             




lstmPredictions <- bestModel %>% predict(X.rnn.test)

mu <- Y.train.min + lstmPredictions[, 1]*(Y.train.max - Y.train.min)
sigma <- exp(lstmPredictions[, 2])*(Y.train.max - Y.train.min)
n <- length(mu)
upper95 <- mu + 1.96*sigma
lower95 <- mu - 1.96*sigma
upper50 <- mu + 0.674*sigma
lower50 <- mu - 0.674*sigma






plot(rainfallAnomaly[Y.test.inds, 3], ty = "l", 
     xlab = "Time (months)", 
     ylab = "MDB Rainfall Anomaly (mm)")
lines(mu, col = "blue")
polygon(x = c(1:n, rev(1:n), 1),
        y = c(lower95, rev(upper95), lower95[1]),
        col = fade("blue", 100), border = NA)
polygon(x = c(1:n, rev(1:n), 1),
        y = c(lower50, rev(upper50), lower50[1]),
        col = fade("blue", 100), border = NA)
        
        
        
        


plot(rainfallAnomaly[Y.test.inds, 3], ty = "l",
     xlab = "Time (months)",
     ylab = "MDB Rainfall Anomaly (mm)")
lines(mu, col = "blue")
polygon(x = c(1:n, rev(1:n), 1),
        y = c(lower95, rev(upper95), lower95[1]),
        col = fade("blue", 100), border = NA)
polygon(x = c(1:n, rev(1:n), 1),
        y = c(lower50, rev(upper50), lower50[1]),
        col = fade("blue", 100), border = NA)
        
        
        
        
n <- (length(Y.test.inds))
coverage50 <- length(which(rainfallAnomaly[Y.test.inds, 3]> lower50
              & rainfallAnomaly[Y.test.inds, 3] < upper50))/n
coverage95 <- length(which(rainfallAnomaly[Y.test.inds, 3] > lower95
              & rainfallAnomaly[Y.test.inds, 3] < upper95))/n
print(coverage50)
print(coverage95)


















  