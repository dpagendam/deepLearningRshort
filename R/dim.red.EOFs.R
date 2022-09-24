#' @title Perform Dimension Reduction to create EOFs.
#' @description \code{dim.red.EOFs} performs dimension reduction on a matrix using singular value decomposition to create a set of Empirical Orthogonal Functions (EOFs).
#' #'
#' @param X is a matrix, whose row vectors contain independent realisations of a stochastic process.  The number of columns in X represents the dimension of the process being observed.
#' @param numComponents is the number of dimensions to reduce X to and should be less than the number of columns in X.
#' @param percentageVariation is used to determine the number of EOFs to use if numComponents is not specified.  For example, specifying percentVariation = 0.95 will retain enough EOFs to account for 95% of the overall variation exibited in X.
#' @param plot is used to control whether or not to plot the percentage of variation explained as a function of the number of EOFs.
#'
#' @return the function returns a list containing two named objects: (i) v.dim.red is a matrix that contains coefficients that can be applied to the EOFs to (approximately) reconstruct each of the rows of X; and (ii) EOFs is a matrix that contains the EOFs in it's columns.  X is approximately equal to EOFs %*%  v.dim.red.
#' @export
#'
#' @examples
dim.red.EOFs <- function(X, numComponents = NULL, percentageVariation = NULL, plot = TRUE)
{
  if(!any(class(X) == "matrix"))
  {
    stop("Argument X must be a matrix.")
  }
  X.z <- scale(X)
  X.z[is.nan(X.z)] <- 0
  svd_X <- svd(t(X.z))
  if(!is.null(numComponents))
  {
    if(numComponents > ncol(X))
    {
      stop("numComponents must be less than or equal to the number of rows in X.")
    }
    U_trunc <- (svd_X$u)[, 1:numComponents]
    D_trunc <- diag(svd_X$d[1:numComponents])
    phi <- U_trunc %*% D_trunc
    v.dim.red <- svd_X$v[, 1:numComponents]
  }
  else if(!is.null(percentageVariation))
  {
    cs <- cumsum(svd_X$d^2)/(sum(svd_X$d^2))
    numComponents = (which(cs > percentageVariation))[1]
    U_trunc <- (svd_X$u)[, 1:numComponents]
    D_trunc <- diag(svd_X$d[1:numComponents])
    phi <- U_trunc %*% D_trunc
    v.dim.red <- svd_X$v[, 1:numComponents]
  }
  else
  {
    stop("One of the arguments numComponents or percentageVariation must be specified. Both were NULL.")
  }
  if(plot)
  {
    plot(cumsum(svd_X$d^2)/(sum(svd_X$d^2)), xlab = "Number of EOFs", ylab= "Proportion of Variance Explained")
  }
  result <- list(v.dim.red = v.dim.red, EOFs = phi)
  return(result)
}



