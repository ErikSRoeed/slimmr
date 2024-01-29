
#' construct_eidosmodel_from_eidosblocks
#'
#' @description Internal slimmr function.
#'
#' @param eidos_blocks A list() of slimmr::EidosBlock objects.
#' @returns An EidosModel object.
#'
#' @include slimmr_v1.0_EidosModel.R
#' @include slimmr_v1.0_EidosComposition.R
#' @noRd
#'
construct_eidosmodel_from_eidosblocks <- function(name, eidos_blocks)
{
  eidos_blocks |>
    is.list() |>
    stopifnot()
  eidos_blocks |>
    check_list_contains_only_correct_class("EidosBlock") |>
    stopifnot(all())

  model <- EidosModel$new(name = name, eidos_blocks = eidos_blocks)
  return(model)
}
