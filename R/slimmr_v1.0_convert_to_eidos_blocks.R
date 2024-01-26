
#' convert_to_blocks
#'
#' @description Internal slimmr function.
#'
#' @param eidos_lines A list() of slimmr::EidosLine objects.
#' @returns A list of slimmr::EidosBlock objects.
#'
#' @include slimmr_v1.0_EidosBlock.R
#' @include slimmr_v1.0_EidosComposition.R
#' @noRd
#'
convert_to_eidos_blocks <- function(eidos_lines)
{
  stopifnot(is.list(eidos_lines))

  check_eidos_line <- function(object) "EidosLine" %in% class(object)
  inputs_are_valid <- sapply(eidos_lines, check_eidos_line)

  stopifnot(all(inputs_are_valid))

  empty_first_block <- EidosBlock$new(items = list())
  blocks <- list(empty_first_block)

  current_block_number <- 1
  must_add_new_block <- FALSE

  for (line in eidos_lines)
  {
    possibly_add_new_block <- line$is_comment || line$is_callback

    if (possibly_add_new_block)
    {
      must_add_new_block <- TRUE
    }

    current_block_needs_callback <- blocks[[current_block_number]]$callback |> is.null()

    if (current_block_needs_callback)
    {
      must_add_new_block <- FALSE
    }

    not_add_new_block <- ! line$is_toplevel

    if (not_add_new_block)
    {
      must_add_new_block <- FALSE
    }

    if (must_add_new_block)
    {
      current_block_number <- current_block_number + 1
      blocks[[current_block_number]] <- EidosBlock$new(items = list(line))
      next
    }

    blocks[[current_block_number]]$add(line)
  }

  return(blocks)
}
