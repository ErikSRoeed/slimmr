
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
#' @param seed Random seed for simulations
#' @param slim_output_verbosity An integer 0 - 2.
#' @param output_parsing_function A function applied to raw output from SLiM
#' @param syscall_wrapper Wrapper amended to system call for calling slim (if it
#' is not installed on the PATH, must be called via WSL or MSYS2/MINGW64, etc.)
#' @param ... Named constant arguments to SLiM (i.e. passed to -d / -define)
#' @return Either the raw output from SLiM, or that output as parsed by
#' output_parse_function.
#'
#' @export
#'
run_slim <- function(
    model,
    reps = 1,
    seed = NULL,
    slim_output_verbosity = 1,
    output_parsing_function = function(x) return(x),
    syscall_wrapper = NULL,
    parallel_nodes = 1,
    ...
)
{
  check_slim_installation(syscall_wrapper = syscall_wrapper)
  temporary_script_path <- tempfile(fileext = ".slim")
  model$write_to_file(file_path = temporary_script_path)

  slim_argument_path <- paste('"', temporary_script_path, '"', sep = "")
  slim_argument_seed <- ifelse(is.null(seed), "", paste("-s", seed))
  slim_argument_verbosity <- paste("-l", slim_output_verbosity)

  constants <- list(...)
  if (length(constants) == 0)
  {
    slim_arguments_constants <- ""
  }
  else
  {
    slim_arguments_constants <- paste(
      "-d ", names(constants), "=", constants,
      collapse = " ", sep = ""
    )
  }

  slim_arguments <- paste(
    slim_argument_seed,
    slim_argument_verbosity,
    slim_arguments_constants,
    slim_argument_path,
    sep = " "
  )

  slim_call <- write_slim_call(
    wrapper = syscall_wrapper,
    arguments = slim_arguments
  )

  output_data_structure <- list(
    seed = seed,
    constants = constants,
    system_call = slim_call,
    replicate_runs = reps
  )

  node_export_list <- list(
    slim_call = slim_call,
    output_parsing_function = output_parsing_function
    )

  cluster <- parallel::makeCluster(parallel_nodes)
  cluster |> parallel::clusterExport(
    varlist = names(node_export_list),
    envir = list2env(node_export_list)
  )

  slim_outputs <- parallel::clusterApplyLB(
    cl = cluster,
    x = seq(reps),
    fun = function(rep)
    {
      slim_output <- system(slim_call, intern = TRUE) |>
        output_parsing_function()
      return(slim_output)
    }
  )

  parallel::stopCluster(cluster)

  output_data_structure$slim_outputs <- slim_outputs
  return(output_data_structure)
}
