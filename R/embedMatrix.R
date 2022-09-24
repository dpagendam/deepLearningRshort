#' @title Construct an embedding matrix for a time series.
#' @description \code{embedMatrix} constructs a matrix of embedding vectors for a matrix whose columns represent time series.
#' #'
#' @param X is a matrix.
#' @param lags is a vector of integer lags.
#' @param padding is the value used in a matrix where lagged values are not known.
#'
#' @return returns a matrix like X that has additional columns corresponding to lagged versions of time series.
#' @export
#'
#' @examples

embedMatrix <- function(X, lags, padding)
{
  if(!any(class(X) == "matrix"))
  {
    stop("Argument X must be a matrix.")
  }
  if(!is.numeric(padding))
  {
    stop("Argument padding must be numeric.")
  }

  lags <- round(lags)

  if(any(!is.numeric(lags)))
  {
    stop("Argument lags must all be numeric.")
  }

  for(i in 1:length(lags))
  {
    if(i == 1)
    {
      thisLaggedMat <- lagMatrix(X, lags[i], padding)
    }
    else
    {
      thisLaggedMat <- cbind(thisLaggedMat, lagMatrix(X, lags[i], padding))
    }
  }
  return(thisLaggedMat)
}
