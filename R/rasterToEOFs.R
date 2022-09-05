#' @title Construct EOFs from a list of raster objects.
#' @description \code{rasterToEOFs} creates a set of Empirical Orthogonal Functions (EOFs) from a collection of raster objects contained in a list.
#' #'
#' @param rasterList is a list containing raster objects.
#' @param numComponents is the number of dimensions to reduce X to and should be less than the number of columns in X.
#' @param percentageVariation is used to determine the number of EOFs to use if numComponents is not specified.  For example, specifying percentVariation = 0.95 will retain enough EOFs to account for 95% of the overall variation exibited in X.
#' @param plot is used to control whether or not to plot the percentage of variation explained as a function of the number of EOFs.
#'
#' @return the function returns a list containing three named objects: (i) raster.dims is the dimensions of the raster; (ii) raster.validPixels is the indices of the rasters that are valid (i.e. not masked out); and (iii) rasterEOFs is a list that contains two named items.
#' rasterEOFs contains: (i) v.dim.red is a matrix that contains coefficients that can be applied to the EOFs to (approximately) reconstruct each of the rows of X; and (ii) EOFs is a matrix that contains the EOFs in it's columns.  X is approximately equal to EOFs %*%  v.dim.red.
#' @export
#'
#' @examples


rasterToEOFs <- function(rasterList, numComponents = NULL, percentageVariation = NULL, plot = TRUE)
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

  pixelIndsNoNAs <- 1:prod(dims)
  pixelIndsWithNAs <- c()
  for(i in 1:prod(dims))
  {
    if(any(is.na(X[, i])))
    {
      pixelIndsWithNAs <- c(pixelIndsWithNAs, i)
    }
  }
  pixelIndsNoNAs <- pixelIndsNoNAs[-pixelIndsWithNAs]
  X <- X[, pixelIndsNoNAs]
  #X[is.na(X)] <- mean(X, na.rm = TRUE)

  rasters.red <- dim.red.EOFs(X, numComponents, percentageVariation, plot = plot)
  result <- list(raster.dims = dims, raster.validPixels = pixelIndsNoNAs, rasterEOFs = rasters.red)
}
