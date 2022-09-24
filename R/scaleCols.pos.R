#' @title Scale the columns of a matrix to [0, 1].
#' @description \code{scaleCols.pos} 
#' #'
#' @param X a matrix to scale
#' @param colMinsX is an optional vector of column minimum values to use.
#' @param colMaxsX is an optional vector of column maximum values to use.
#'
#' @return a list containing: (i) the scaled matrix; (ii) the column mins used for scaling; and (iii) the column maxs used for scaling
#' @export
#'
#' @examples 
scaleCols.pos <- function(X, colMinsX = NULL, colMaxsX = NULL)
{
  if(!any(class(X) == "matrix"))
  {
    stop("Argument X must be a matrix.")
  }
  if(is.null(colMinsX))
  {
    colMinsX <- apply(X, 2, min)
  }
  else if(length(colMinsX) != ncol(X))
  {
    stop("Argument colMinsX must have length equal to ncol(X).")
  }
  
  if(is.null(colMaxsX))
  {
    colMaxsX <- apply(X, 2, max)
  }
  else if(length(colMaxsX) != ncol(X))
  {
    stop("Argument colMaxsX must have length equal to ncol(X).")
  }
  
  badInds <- which(colMinsX == colMaxsX)
    
  scaleRow <- function(rowX)
  {
    scaled <- (rowX - colMinsX)/(colMaxsX - colMinsX)
  }
    
  replaceRow <- function(rowX)
  {
    rowX[badInds] <- 0.5
    return(rowX)
  }
    
  scaledX <- t(apply(X, 1, scaleRow))
  if(length(badInds) > 0)
  {
    scaleX_fixed <- t(apply(scaledX, 1, replaceRow))
  }
  else
  {
    scaleX_fixed <- scaledX
  }
  
    
  result <- list(X.scaled = scaleX_fixed, colMinsX = colMinsX, colMaxsX = colMaxsX, scalingType = "pos")
    
  return(result)
}
