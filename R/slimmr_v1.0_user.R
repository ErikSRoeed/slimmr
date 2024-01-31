
#' Import a SLiM model from an Eidos script.
#'
#' @description This function import a .slim script file and converts it to a
#' slimmr EidosModel object. This object can be manipulated and run with the
#' various functions of slimmr.
#'
#' @param script_path Path to .slim script.
#' @param name A name for the model (defaults to script_path)
#' @return An EidosModel object.
#'
#' @export
#'
import_slim_model <- function(script_path, name = script_path)
{
  model <- parse_script(script_path) |>
    convert_script_to_eidoslines() |>
    group_eidoslines_in_eidosblocks() |>
    construct_eidosmodel_from_eidosblocks(name = name)

  return(model)
}

#' Inspect the Eidos script of a SLiM model.
#'
#' @description This functions outputs the script contained in an EidosModel
#' object to the console.
#'
#' @param slim_model An EidosModel object.
#'
#' @export
#'
inspect_eidos_script <- function(slim_model)
{
  slim_model$write_to_console()
}

#' Run a SLiM model.
#'
#' @description This function runs a SLiM model.
#'
#' @param model An EidosModel object.
#' @param reps Number of replicate simulations.
#' @param syscall_prefix Prefix amended to system call for calling slim (if it
#' is not installed on the PATH, must be called via WSL or MSYS2/MINGW64, etc.)
#' @return Either the raw output from SLiM, or that output as parsed by
#' output_parse_function.
#'
#' @export
#'
run_slim <- function(model, reps, syscall_prefix = NULL)
{
  SLIM_CALL_FAIL_CODE = 127
  test_slim_call <- write_slim_call(syscall_prefix, suffix = " --version")

  if (try(system(test_slim_call)) == SLIM_CALL_FAIL_CODE)
  {
    stop("SLiM call failed. Is SLiM on your PATH? (See also syscall_prefix.)")
  }

  temporary_script_path <- tempfile(fileext = ".slim")
  model$write_to_file(file_path = temporary_script_path)
  path_argument <- paste('"', temporary_script_path, '"', sep = "")

  slim_call <- write_slim_call(prefix = syscall_prefix, suffix = path_argument)

  for (rep in 1 : reps)
  {
    slim_output <- system(slim_call, intern = TRUE, ignore.stderr = TRUE)
    cat(slim_output, sep = "\n")
  }
}
