#' @title Fade a colour.
#' @description \code{dim.red.EOFs} adds an alpha channel (transparency) to a given colour.
#' #'
#' @param colours is the name of a colour (character object).
#' @param alpha is a value between 0 and 255 that indicates the level of opacity.
#'
#' @return a new colour (character object).
#' @export
#'
#' @examples

fade <- function(colors,alpha)
{
  rgbcols <- col2rgb(colors)
  rgb(rgbcols[1,], rgbcols[2,], rgbcols[3,], alpha, max=255)
}
