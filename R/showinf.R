#' Displaying images and metadata
#'
#' @param file File to display
#' @param pixel_data Should the image/pixel data be read in?
#' If \code{FALSE}, the \code{-nopix} is passed.
#' @param range To convert images between certain indices (inclusive),
#' set 0 to first
#' @param series All images in the input file are converted by default.
#' To convert only one series, set this to a numeric
#' @param crop of the format x,y,width,height. The (x, y) coordinate
#' (0, 0) is the upper-left corner of the image;
#' x + width must be less than or equal to the image width
#' and y + height must be less than or equal to the image height.
#' @param debug Enables debugging output if more information is needed
#' @param opts Additional options to pass to \code{showinf}
#' @param verbose Should the command be printed
#' @param run  Should the command be run?  Useful for diagnostics.
#' @param autoscale Adjusts the display range to the
#' minimum and maximum pixel values.
#' @param ome_xml should this populate OME-XML metadata
#'
#' @return The output file name
#' @export
#'
#' @examples
#' file = "~/Downloads/2017_08_03__0006.czi"
#' if (file.exists(file)) {
#' res = showinf(file = file, run = FALSE)
#' res
#' }
showinf = function(
  file,
  pixel_data = FALSE,
  series = NULL,
  range = NULL,
  crop = NULL,
  autoscale = FALSE,
  ome_xml = FALSE,
  opts = c("-no-upgrade", "-novalid"),
  debug = FALSE,
  verbose = TRUE,
  run = TRUE
) {


  stopifnot(file.exists(file))

  opts = c(opts,
           ifelse(!pixel_data, "-nopix", ""))

  L = list(
    series = series,
    range = range,
    crop = crop
  )
  nulls = sapply(L, is.null)
  L = L[!nulls]

  if (length(L) > 0) {
    names(L) =  paste0("-", names(L))
    L = mapply(function(name, value) {
      collapser = " "
      if (name == "crop") {
        collapser = ","
      }
      value = paste(value, collapse = collapser)
      paste(name, value)
    }, names(L), L, SIMPLIFY = TRUE)
    opts = c(opts, L)
  }

  if (debug) {
    opts = c(opts, "-debug")
  }
  if (autoscale) {
    opts = c(opts, "-autoscale")
  }
  if (ome_xml) {
    opts = c(opts, "-omexml")
  }
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")

  cmd = bf_cmd("showinf")
  cmd = paste(cmd, file, opts)
  outfile = tempfile(fileext = ".txt")
  cmd = paste(cmd, ">", outfile)
  if (verbose) {
    message("Command is: ", cmd)
  }
  if (run) {

    res = system(cmd)
    if (res != 0) {
      warning("Result was not zero!")
    }
    class(outfile) = "showinf_result"
    attr(outfile, "result") = res
    attr(outfile, "ome_xml") = ome_xml
    return(outfile)
  } else {
    return(cmd)
  }

}

#' @export
#' @rdname showinf
showinf.help = function() {
  cmd = bf_cmd("showinf")
  suppressWarnings({
    res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  })
  cat(res, sep = "\n")
}

#' @export
#' @rdname showinf
bf_showinf = showinf


#' @export
#' @rdname showinf
bf_show_info = showinf

#' @rdname showinf
#' @export
showinf_version = function() {
  cmd = bf_cmd("showinf")
  outfile = tempfile(fileext = ".txt")
  cmd = paste(cmd, "-version >", outfile)
  res = system(cmd)

  if (res != 0) {
    warning("Result was not zero!")
  }
  return( readLines(outfile))
}

