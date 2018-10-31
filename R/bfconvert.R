#' Convert Bio-Format
#'
#' @param file File to Convert
#' @param outfile Output filename
#' @param series All images in the input file are converted by default.
#' To convert only one series, set this to a numeric
#' @param timepoint To convert only one timepoint, set this to a numeric
#' @param channel To convert only one channel, set this to a numeric
#' @param z To convert only z section, set this to a numeric
#' @param range To convert images between certain indices (inclusive),
#' set 0 to first
#' @param tilex width in pixels of each tile
#' @param tiley height in pixels of each tile.
#' @param crop of the format x,y,width,height. The (x, y) coordinate
#' (0, 0) is the upper-left corner of the image;
#' x + width must be less than or equal to the image width
#' and y + height must be less than or equal to the image height.
#' @param compression By default, all images will be written uncompressed.
#' This is the type of compression done, like LZW.
#' @param overwrite Should the file be overwritten
#' @param lookup To disable the conversion of lookup tables,
#' leaving the output file without any lookup tables, set this to
#' \code{FALSE}
#' @param bigtiff This option forces the writing of a BigTiff file
#' @param opts Additional options to pass to \code{bfconvert}
#' @param verbose Should the command be printed
#' @param run  Should the command be run?  Useful for diagnostics.
#'
#' @return The output file name
#' @export
#'
#' @examples
#' file = "~/Downloads/2017_08_03__0006.czi"
#' if (file.exists(file)) {
#' res = bfconvert(file = file, run = FALSE)
#' res
#' }
bfconvert = function(
  file,
  outfile = tempfile(fileext = ".tiff"),
  series = NULL,
  timepoint = NULL,
  channel = NULL,
  z = NULL,
  range = NULL,
  tilex = NULL,
  tiley = tilex,
  crop = NULL,
  compression = NULL,
  overwrite = TRUE,
  lookup = TRUE,
  bigtiff = FALSE,
  opts = "",
  verbose = TRUE,
  run = TRUE
) {

  stopifnot(file.exists(file))

  if (file.exists(outfile) & !overwrite) {
    stop("outfile exists and overwrite = FALSE")
  }
  overwrite = paste0("-", ifelse(overwrite, "", "no"),
                     "overwrite")
  opts = c(opts, overwrite)

  L = list(
    series = series,
    timepoint = timepoint,
    channel = channel,
    z = z,
    range = range,
    tilex = tilex,
    tiley = tiley,
    crop = crop,
    compression = compression
  )
  nulls = sapply(L, is.null)
  L = L[!nulls]
  if (length(L) > 0) {
    names(L) =  paste0("-", names(L))
    L = mapply(function(name, value) {
      value = paste(value, collapse = " ")
      paste(name, value)
    }, names(L), L, SIMPLIFY = TRUE)
    opts = c(opts, L)
  }

  if (!lookup) {
    opts = c(opts, "-nolookup")
  }
  if (bigtiff) {
    opts = c(opts, "-bigtiff")

  }
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")

  cmd = bf_cmd("bfconvert")
  cmd = paste(cmd, file, opts, outfile)
  if (verbose) {
    message("Command is: ", cmd)
  }
  if (run) {
    res = system(cmd)
    if (res != 0) {
      warning("Result was not zero!")
    }
    return(outfile)
  } else {
    return(cmd)
  }

}
