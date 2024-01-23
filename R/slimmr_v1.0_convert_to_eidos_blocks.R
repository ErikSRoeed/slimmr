
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
  stopifnot(is.list(eidos_lines))

  test_if_eidos_line <- function(object) "EidosLine" %in% class(object)
  inputs_are_valid <- sapply(eidos_lines, test_if_eidos_line)

  stopifnot(all(inputs_are_valid))

  blocks <- list()

  for (line in eidos_lines)
  {
    last_block_number <- length(blocks)
    line_is_callback_of_new_block <- ! is.null(line$callback)

    if (line_is_callback_of_new_block)
    {
      new_block_number <- last_block_number + 1
      blocks[[new_block_number]] <- EidosBlock$new(eidos_lines = list(line))
      next
    }

    blocks[[last_block_number]]$add_line(line)
  }

  return(blocks)
}
