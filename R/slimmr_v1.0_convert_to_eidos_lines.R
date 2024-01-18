
#' convert_to_eidos_lines
#'
#' @description Internal slimmr function.
#'
#' @param script Character vector of script parsed with slimmr::parse_script().
#' @returns A list() of slimmr::EidosLine objects.
#'
#' @include slimmr_v1.0_EidosLine.R
#' @noRd
#'
convert_to_eidos_lines <- function(script)
{
  stopifnot(is.character(script))
  stopifnot(! is.null(script))

  n_eidos_lines <- length(script)
  eidos_lines <- vector("list", length = n_eidos_lines)

  for (line_number in 1 : n_eidos_lines)
  {
    line_string <- script[line_number]
    eidos_line <- EidosLine$new(number = line_number, string = line_string)
    eidos_lines[[line_number]] <- eidos_line
  }

  return(eidos_lines)
}

