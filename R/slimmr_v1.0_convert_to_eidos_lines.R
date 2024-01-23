
#' convert_to_eidos_lines
#'
#' @description Internal slimmr function.
#'
#' @param script_lines char. vector of script lines from slimmr::parse_script().
#' @returns A list() of slimmr::EidosLine objects.
#'
#' @include slimmr_v1.0_EidosLine.R
#' @noRd
#'
convert_to_eidos_lines <- function(script_lines)
{
  stopifnot(is.character(script_lines))
  stopifnot(! is.null(script_lines))

  line_numbers <- 1 : length(script_lines)
  eidos_lines <- list()

  for (number in line_numbers)
  {
    eidos_lines[[number]] <- script_lines[number] |> EidosLine$new()
  }

  return(eidos_lines)
}

