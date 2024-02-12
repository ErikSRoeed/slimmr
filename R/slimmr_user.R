
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
#' @param parallel_nodes Number of load balancing parallel nodes to use.
#' @param slim_command Full path to SLiM executable. Default "slim" if on PATH.
#' @param output_runtime_diagnostics TRUE/FALSE. Output CPU and RAM usage?
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
    parallel_nodes = 1,
    slim_command = "slim",
    output_runtime_diagnostics = FALSE,
    ...
)
{
  check_slim_installation(slim_command)
  temporary_script_path <- tempfile(fileext = ".slim")
  model$write_to_file(file_path = temporary_script_path)

  slim_argument_path <- paste('"', temporary_script_path, '"', sep = "")
  slim_argument_verbosity <- paste("-l", slim_output_verbosity)
  slim_argument_seed <- ifelse(is.null(seed), "", paste("-s", seed))
  slim_argument_cpu <- ifelse(output_runtime_diagnostics, "-t", "")
  slim_argument_ram <- ifelse(output_runtime_diagnostics, "-m", "")

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
    slim_argument_cpu,
    slim_argument_ram,
    slim_argument_verbosity,
    slim_arguments_constants,
    slim_argument_path,
    sep = " "
  )

  output_data_structure <- list(
    script = readLines(temporary_script_path),
    seed = seed,
    constants = constants,
    system_call = paste(slim_command, slim_arguments),
    replicate_runs = reps
  )

  export_to_nodes <- list(
    slim_command = slim_command,
    slim_arguments = slim_arguments,
    output_parsing_function = output_parsing_function
  )

  cluster <- parallel::makeCluster(parallel_nodes)
  cluster |> parallel::clusterExport(
    varlist = names(export_to_nodes),
    envir = list2env(export_to_nodes)
  )

  slim_outputs <- parallel::clusterApplyLB(
    cl = cluster,
    x = seq(reps),
    fun = function(rep)
    {
      slim_output <- system2(
        slim_command,
        slim_arguments,
        stdout = TRUE,
        stderr = TRUE
      ) |> output_parsing_function()

      return(slim_output)
    }
  )

  parallel::stopCluster(cluster)

  output_data_structure$slim_outputs <- slim_outputs
  return(output_data_structure)
}
