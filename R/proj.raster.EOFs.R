#' @title Project rasters onto a set of EOFs.
#' @description \code{proj.raster.EOFs} projects a set of rasters contained in a list onto an existing set of Empirical Orthogonal Functions (EOFs).
#' #'
#' @param rasterList is a list containing raster objects.
#' @param EOFs is a matrix whose columns contain individual EOFs.
#' @param validPixels is a vector containing the indices in the raster that are valid pixels (i.e. have not been masked out).
#'
#' @return a matrix containing coefficients that can be used to (approximately) recreate the vectors in X from the EOFs.
#' @export
#'
#' @examples
proj.raster.EOFs <- function(rasterList, EOFs, validPixels)
{
  require(raster)
  if(class(rasterList) != "list")
  {
    stop("Argument rasterList must be a list.")
  }
  dims <- dim(rasterList[[1]])
  X <- matrix(NA, length(rasterList), prod(dims))
  for(i in 1:length(rasterList))
  {
    X[i, ] <- raster::getValues(rasterList[[i]])
  }
  X <- X[, validPixels]
  rasterprojection <- proj.EOFs(X, EOFs)
  return(rasterprojection)
}
