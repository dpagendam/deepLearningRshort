if(Sys.info()['sysname'] == "Linux")
{
  #Install Linux dependencies for those using Google Colab
  system('sudo apt-get install -y libtiff5-dev', intern=TRUE)
  system('sudo apt-get install -y libfftw3-dev', intern=TRUE)
}
install.packages(c("pbapply", "keras", "raster"), 
                 repos = "http://cran.us.r-project.org", 
                 quiet = TRUE)
                 
                 
                 
                 
                 
remotes::install_github("dpagendam/deepLearningRshort")
install.packages("BiocManager", 
                 repos = "http://cran.us.r-project.org", 
                 quiet = TRUE)
BiocManager::install("EBImage")
library(pbapply)
library(keras)
library(raster)
library(deepLearningRshort)
library(EBImage)




width <- 50
height <- 50
grayScale <- FALSE
packageDataDir = system.file("extdata", package="deepLearningRshort")
trainData <- extract_feature(paste0(packageDataDir, "/carcinoma/train/"),
                             width, height, grayScale, TRUE)
testData <- extract_feature(paste0(packageDataDir, "/carcinoma/test/"), 
                            width, height, grayScale, TRUE)
                            
                            



numInputChannels <- 3
train_array <- t(trainData$X)
dim(train_array) <- c(width, height, numInputChannels, 
                      nrow(trainData$X))
train_array <- aperm(train_array, c(4,1,2,3))
  
test_array <- t(testData$X)
dim(test_array) <- c(width, height, numInputChannels, 
                     nrow(testData$X))
test_array <- aperm(test_array, c(4,1,2,3))




model <- keras_model_sequential()

model %>% layer_conv_2d(kernel_size = c(5,5), filters = 8,
          strides = 1, activation = "relu", padding = "same", 
          input_shape = c(width, height, numInputChannels),
          data_format="channels_last") %>%
layer_batch_normalization() %>%
layer_max_pooling_2d(pool_size = c(2,2), padding = "same") %>% 

layer_conv_2d(kernel_size = c(5,5), filters = 8,
          strides = 1, activation = "relu", padding = "same", 
          input_shape = c(width, height, numInputChannels),
          data_format="channels_last") %>%
layer_batch_normalization() %>%
layer_max_pooling_2d(pool_size = c(2,2), padding = "same")  %>%
layer_conv_2d(kernel_size = c(5,5), filters = 8,
          strides = 1, activation = "relu", padding = "same", 
          input_shape = c(width, height, numInputChannels),
          data_format="channels_last") %>%
layer_batch_normalization() %>%
layer_max_pooling_2d(pool_size = c(2,2), padding = "same") %>%
layer_flatten() %>%
layer_dense(units = 8, activation = "relu") %>% 
layer_dense(units = 8, activation = "relu") %>%
layer_dense(units = 8, activation = "relu") %>%
layer_dense(units = 1, activation = "sigmoid")



model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(learning_rate = 0.0001),
  metrics = c('accuracy')
)



history <- model %>% fit(
  x = train_array, y = as.numeric(trainData$y),
  epochs = 50, batch_size = 32, validation_data =
    list(test_array,as.numeric(testData$y))
)




plot(history)




model <- keras_model_sequential()

model %>% layer_conv_2d(kernel_size = c(5,5), filters = 8,
          strides = 1, activation = "relu", padding = "same", 
          input_shape = c(width, height, numInputChannels),
          data_format="channels_last") %>%
layer_batch_normalization() %>%
layer_max_pooling_2d(pool_size = c(2,2), padding = "same") %>% 

layer_conv_2d(kernel_size = c(5,5), filters = 8,
          strides = 1, activation = "relu", padding = "same", 
          input_shape = c(width, height, numInputChannels),
          data_format="channels_last") %>%
layer_batch_normalization() %>%
layer_max_pooling_2d(pool_size = c(2,2), padding = "same")  %>%
layer_conv_2d(kernel_size = c(5,5), filters = 8,
          strides = 1, activation = "relu", padding = "same", 
          input_shape = c(width, height, numInputChannels),
          data_format="channels_last") %>%
layer_batch_normalization() %>%
layer_max_pooling_2d(pool_size = c(2,2), padding = "same") %>%
layer_flatten() %>%
layer_dense(units = 8, activation = "relu") %>% 
layer_dense(units = 8, activation = "relu") %>%
layer_dense(units = 8, activation = "relu") %>%
layer_dense(units = 1, activation = "sigmoid")




model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(learning_rate = 0.0001),
  metrics = c('accuracy')
)




history <- model %>% fit(
  x = train_array, y = as.numeric(trainData$y),
  epochs = 100, batch_size = 32, validation_data =
    list(test_array,as.numeric(testData$y)), 
  callbacks = list(callback_early_stopping(
           monitor = "val_loss", min_delta = 0, patience = 20), 
           callback_model_checkpoint(filepath = "carcinoma.hd5",
           save_best_only = TRUE, save_weights_only = FALSE),
           callback_reduce_lr_on_plateau(monitor = "val_loss", 
          factor = 0.25, patience = 5))
)

bestModel <- load_model_hdf5(filepath = "carcinoma.hd5")



plot(history)




# We can obtain the outputs of the model (sigmoid activation)
probabilities <- predict(model, test_array)
# We obtain the predicted classes for the test/validation dataset
predictions <-  as.numeric(predict(model, test_array) %>% 
                             `>`(0.5) %>% k_cast("int32"))
truth <- testData$y
propCorrect <- sum(predictions == truth)/length(truth)
print(propCorrect)




random <- sample(1:nrow(testData$X), 16)
plot_preds <- predictions[random]
plot_probs <- as.vector(round(probabilities[random,], 2))
plot_truth <- truth[random]

par(mfrow = c(4, 4), mar = rep(0, 4))
for(i in 1:length(random)){
  if(grayScale)
  {
    image(t(apply(test_array[random[i],,,], 2, rev)),
          col = gray.colors(12), axes = F)
  }
  if(!grayScale)
  {
    image(t(apply(test_array[random[i],,,1], 2, rev)),
          col = fade(hcl.colors(12, "YlOrRd", rev = FALSE), 100), axes = F)
  }
    legend("top", legend = paste0("Pred: ", ifelse(plot_preds[i] == 0, "IDC Neg.", "IDC Pos.")),
         text.col = ifelse(plot_preds[i] == 0, 4, 2), bty = "n", text.font = 2)
  legend("center", legend = plot_probs[i], bty = "n", cex = 2, text.col = "black")
  legend("bottom", legend = paste0("Truth: ", ifelse(plot_truth[i] == 0, "IDC Neg.", "IDC Pos.")), text.col = ifelse(plot_truth[i] == 0, 4, 2), bty = "n", text.font = 2)
}











                            

                 
                 
