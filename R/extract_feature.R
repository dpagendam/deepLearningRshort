#' @title Extract images from directory.
#' @description \code{extract_feature }
#' #'
#' @param dir_path a directory as a character string.
#' @param width of image in pixels
#' @param height of image in pixels
#' @param grayScale boolean.  True converts to grayscale, FALSE retains RGB.
#' @param labelsExist boolean.  Are the labels in the file name.
#'
#' @return a matrix of features.
#' @export
#'
#' @examples


#need a vector y which contains 1s and zeros based on membership to the two classes
extract_feature <- function(dir_path, width, height, grayScale, labelsExist = T) {
  library(EBImage)
  library(stringr)
  library(pbapply)

  img_size <- width * height

  ## List images in path
  images_names <- list.files(dir_path, pattern = "*.png")

  if(labelsExist){
    diagnosis <- str_extract(images_names, "(class0.png|class1.png)$")
    # Set negative for IDC == 0 and postivie for IDC == 1
    key <- c("class0.png" = 0, "class1.png" = 1)
    y <- key[diagnosis]
  }

  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  rescaleImage <- function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale (normalized to max)
    if(grayScale)
    {
      grayimg <- channel(img_resized, "gray")
      ## Get the image as a matrix
      img_array <- grayimg@.Data
    }
    else
    {
      img_array <- img_resized@.Data
    }
    ## Coerce to a vector (row-wise)
    img_vector <- as.vector(img_array)
    return(img_vector)
  }
  feature_list <- pblapply(images_names, rescaleImage)
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))

  if(labelsExist)
  {
    return(list(X = feature_matrix, y = y))
  }
  else
  {
    return(feature_matrix)
  }
}

