#' @title Construct a lag matrix for a time series.
#' @description \code{lagMatrix} creates a lagged version of a time series contained in a time series matrix X.
#' #'
#' @param X is a matrix. The columns of X represent time series.
#' @param lags is a scalar integer lag.
#' @param padding (default = 0) is the value used in a matrix where lagged values are beyond the limit of the observed time series.
#'
#' @return returns a matrix with the same dimension as X, but where the rows have been shifted in time to introduce a time lag.
#' @export
#'
#' @examples

lagMatrix <- function(X, lag, padding = 0)
{
  if(!any(class(X) == "matrix"))
  {
    stop("Argument X must be a matrix.")
  }
  if(!is.numeric(padding))
  {
    stop("Argument padding must be numeric.")
  }
  if(length(lag) > 1)
  {
    cat("length(lag) was greater than 1 - only using the first value. \n")
    lag <- lag[1]
  }

  lag <- round(lag)

  if(lag == 0)
  {
    return(X)
  }
  if(!is.numeric(lag))
  {
    stop("Argument lag must be numeric.")
  }
  if(lag > 0)
  {
    result <- rbind(matrix(padding, lag, ncol(X)), X[1:(nrow(X) - lag), ])
  }
  else
  {
    result <- rbind(X[(lag + 1):(nrow(X)), ], matrix(padding, lag, ncol(X)))
  }
  return(result)
}
