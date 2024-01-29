
#' EidosModel
#'
#' @description Internal class for Eidos model objects.
#'
#' @noRd
#'
EidosModel <- R6::R6Class(

  classname = "EidosModel",
  inherit = EidosComposition,

  public = list(

    initialize = function(name, eidos_blocks)
    {
      super$initialize(eidos_blocks)
      self$set_name(name)
    },

    set_name = function(name)
    {
      private$name_private <- name
    },

    print = function()
    {
      # PLACEHOLDER PRINT FUNCTION
      paste(self$name, " has ", length(self$blocks), " blocks.", sep = "") |>
        print()
    },

    write_to_file = function(file_path, overwrite = FALSE)
    {
      if (file.exists(file_path))
      {
        stopifnot(overwrite)
      }
      else
      {
        file.create(file_path)
      }

      file_connection <- file(file_path, open = "wt")

      for (block in self$blocks)
      {
        for (line in block$lines)
        {
          writeLines(text = line$string, con = file_connection)
        }
        writeLines(text = "", con = file_connection)
      }

      close(file_connection)
    }

  ),

  active = list(

    name = function()
    {
      return(private$name_private)
    },

    blocks = function()
    {
      return(private$items)
    }

  ),

  private = list(

    name_private = ""

  )

)
