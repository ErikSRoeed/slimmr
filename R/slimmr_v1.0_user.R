
#' Import a SLiM model from an Eidos script.
#'
#' @description This functions import a .slim script file and converts it to a
#' slimmr EidosModel object. This object can be manipulated and run with the
#' various functions of slimmr.
#'
#' @param script_path Path to .slim script.
#' @param name A descriptive name for the model.
#' @return An EidosModel object.
#'
#' @export
#'
import_slim_model <- function(script_path, name = "")
{
  model <- parse_script(script_path) |>
    convert_script_to_eidoslines() |>
    group_eidoslines_in_eidosblocks() |>
    construct_eidosmodel_from_eidosblocks(name = name)

  return(model)
}

