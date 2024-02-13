
#' Duplicates a block in a slimmr SLiM model.
#'
#' @description Duplicates one Eidos block in a slimmr SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param block_index Integer index: block to duplicate.
#' @param after_block Integer index: which block to add duplicate block after?
#'
#' @export
#'
duplicate_block <- function(slim_model, block_index, after_block)
{
  original_block <- slim_model$blocks[[block_index]]
  original_characters <- original_block$lines |>
    vapply(
      function(line) line$string,
      character(1)
    )

  add_blocks(
    slim_model = slim_model,
    blocks_script = original_characters,
    after_block = after_block
  )
}
