#' Get Path of BioFormat Command
#'
#' @param cmd Bioformat command
#'
#' @return A character string
#' @export
#'
#' @examples
#' bf_cmd("bfconvert")
bf_cmd = function(cmd) {
  cmd = system.file("bftools", cmd, package = "bftools")
  stopifnot(file.exists(cmd))
  return(cmd)
}
