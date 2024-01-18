
#' parse_script
#'
#' @description Internal slimmr function.
#'
#' @param script_path Valid character path to a .slim script file.
#' @param drop_empty_lines Boolean, default TRUE. Drop empty lines from script?
#' @returns A character vector with one item per line in the parsed script.
#'
#' @noRd
#'
parse_script <- function(script_path, drop_empty_lines = TRUE)
{
  stopifnot(file.exists(script_path))
  stopifnot(file.)
  script <- readLines(script_path)

  if (drop_empty_lines)
  {
    empty_lines <- which(script == "")
    script <- script[-empty_lines]
  }

  return(script)
}
