#' @title Project vectors onto a set of EOFs.
#' @description \code{proj.EOFs} projects a set of vectors contained in the columns of X onto an existing set of Empirical Orthogonal Functions (EOFs).
#' #'
#' @param X is a matrix, whose row vectors contain independent realisations of a stochastic process.  The number of columns in X represents the dimension of the process being observed.
#' @param EOFs is a matrix whose columns contain individual EOFs.
#'
#' @return a matrix containing coefficients that can be used to (approximately) recreate the vectors in X from the EOFs.
#' @export
#'
#' @examples
proj.EOFs <- function(X, EOFs)
{
  # Do some checks on dimensions etc
  if(!any(class(X) == "matrix"))
  {
    stop("Argument X must be a matrix.")
  }
  if(!any(class(EOFs) == "matrix"))
  {
    stop("Argument EOFs must be a matrix.")
  }
  if(nrow(EOFs) != ncol(X))
  {
    stop("Matrix X does not match the dimension of the EOFs.  nrow(EOFs) must equal ncol(X).")
  }
  X.z <- scale(X)
  X.z[is.nan(X.z)] <- 0

  projectRow <- function(Xrow)
  {
    solve((t(EOFs))%*%EOFs) %*% t(EOFs) %*% matrix(Xrow, ncol = 1)
  }

  v_svd <- t(apply(X.z, 1, projectRow))

  return(v_svd)
}
