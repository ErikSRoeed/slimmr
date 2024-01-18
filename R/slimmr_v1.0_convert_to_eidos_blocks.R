
#' convert_to_blocks
#'
#' @description Internal slimmr function.
#'
#' @param eidos_lines A list() of slimmr::EidosLine objects.
#' @returns A list of slimmr::EidosBlock objects.
#'
#' @include slimmr_v1.0_EidosBlock.R
#' @noRd
#'
convert_to_eidos_blocks <- function(eidos_lines)
{
  inputs_are_eidos_lines <- sapply(
    eidos_lines,
    function(line) "EidosLine" %in% class(line)
  )
  stopifnot(all(inputs_are_eidos_lines))
  stopifnot(is.list(eidos_lines))

  last_line_number <- 0
  last_block_index <- 0
  blocks <- list()

  for (line in eidos_lines)
  {
    line_is_new_block_callback <- ! is.null(line$callback)

    if (line_is_new_block_callback)
    {
      new_block_index <- last_block_index + 1
      new_block <- EidosBlock$new(index = new_block_index, lines = list(line))
      blocks <- append(blocks, new_block)
      last_block_index <- new_block_index
    }
    else
    {
      blocks[[last_block_index]]$add_line(line, after_line_number = last_line_number)
    }

    last_line_number <- line$number
  }

  return(blocks)
}
