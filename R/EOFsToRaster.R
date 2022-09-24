#' @title Construct Rasters from EOFs.
#' @description \code{EOFsToRaster} constructs one or more rasters from a set of Empirical Orthogonal Functions (EOFs).
#' #'
#' @param EOFs is a matrix, whose row vectors contain empirical orthogonal functions.
#' @param coefficients is a matrix containing coefficients to use to create rasters as row vectors.  The number of columns on EOFs must equl the number of columns in the coefficients matrix.
#' @param dims is a vector of length 2 containing the number of rows (first element) and number of columns (second element) of the raster.
#' @param validPixels contains indices for pixels that are masked out of the EOFs (i.e. the number of rows in the EOFs can be less than the number of pixels in the raster).
#'
#' @return a list containing raster objects (one for each row of the coefficients matrix).
#' @export
#'
#' @examples

EOFsToRaster <- function(EOFs, coefficients, dims, validPixels)
{
  library(raster)
  if(!any(class(EOFs) == "matrix"))
  {
    stop("Argument EOFs must be a matrix.")
  }
  if(!any(class(coefficients) == "matrix"))
  {
    stop("Argument coefficients must be a matrix.")
  }
  if(ncol(coefficients) != ncol(EOFs))
  {
    stop("ncol(coefficients) and ncol(EOFs) must be equal.")
  }
  dims <- round(dims)
  if(!all(is.numeric(dims)))
  {
    stop("Argument dims can only contain numeric values")
  }

  rasterList <- list()
  for(i in 1:nrow(coefficients))
  {
    rasterVals <- EOFs %*% matrix(coefficients[i, ], ncol = 1)
    rasterVector <- rep(NA, prod(dims))
    rasterVector[validPixels] <- rasterVals
    r <- raster(nrows = dims[1], ncols = dims[2])
    r <- setValues(r, rasterVector)
    rasterList[[i]] <- r
  }

  return(rasterList)
}
