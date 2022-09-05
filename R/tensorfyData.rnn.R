#' @title Convert Data into Tensors Used by RNNs in Keras
#' @description \code{tensorfyData.rnn} is used to convert a matrix, whose columns are time series into data that can be ingested by an RNN model (e.g. LSTM or GRU) in Keras.
#' #'
#' @param ts is a matrix that contains time series as columns.
#' @param forecastMonthsAhead is an integer specifying the number of time steps ahead to forecast.
#' @param timestepsPerSample is the number of timesteps (history) to be included when training each forecast.
#' @param indicesX are indices for the columns of ts to be used as predictors.
#' @param indicesY are indices for the columns of ts to be used as the quantity we are trying to predict.
#' @param indicesTrain are the row indices of ts that are to be used for training.
#' @param indicesTest are the row indices of ts that are to be used for testing.

#'
#' @return
#' @export
#'
#' @examples
tensorfyData.rnn = function(ts, forecastMonthsAhead, timestepsPerSample, indicesX, indicesY, indicesTrain, indicesTest)
{
  train <- ts[indicesTrain, ]
  test <- ts[indicesTest, ]

  n.train = nrow(train) - timestepsPerSample - forecastMonthsAhead
  n.test = nrow(test) - timestepsPerSample - forecastMonthsAhead

  x.train.tsInds <- indicesTrain[timestepsPerSample:(n.train + timestepsPerSample - 1)]
  x.test.tsInds <- indicesTest[timestepsPerSample:(n.test + timestepsPerSample - 1)]
  y.train.tsInds <- indicesTrain[(timestepsPerSample + forecastMonthsAhead):(n.train + timestepsPerSample - 1 + forecastMonthsAhead)]
  y.test.tsInds <- indicesTest[(timestepsPerSample + forecastMonthsAhead):(n.test + timestepsPerSample - 1 + forecastMonthsAhead)]

  d_in = length(indicesX)
  d_out = length(indicesY)

  X.train.rnn <- array(0, c(n.train, timestepsPerSample, d_in))
  X.test.rnn <- array(0, c(n.test, timestepsPerSample, d_in))
  Y.train.rnn <- array(0, c(n.train, d_out))
  Y.test.rnn <- array(0, c(n.test, d_out))

  for(i in 1:n.train)
  {
    X.train.rnn[i, , ] <- train[i:(i + timestepsPerSample - 1), indicesX]
    Y.train.rnn[i, 1:length(indicesY)] <- train[(i + timestepsPerSample - 1 + forecastMonthsAhead), indicesY]
  }
  for(i in 1:n.test)
  {
    X.test.rnn[i, , ] <- test[i:(i + timestepsPerSample - 1), indicesX]
    Y.test.rnn[i, 1:length(indicesY)] <- test[(i + timestepsPerSample - 1 + forecastMonthsAhead), indicesY]
  }

  return(list(X.train.rnn = X.train.rnn, Y.train.rnn = Y.train.rnn, X.test.rnn = X.test.rnn, Y.test.rnn = Y.test.rnn, x.train.tsInds = x.train.tsInds, x.test.tsInds = x.test.tsInds, y.train.tsInds = y.train.tsInds, y.test.tsInds = y.test.tsInds))
}







