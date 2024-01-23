
#' EidosBlock
#'
#' @description Internal class for Eidos block objects.
#'
#' @noRd
#'
EidosBlock <- R6::R6Class(

  classname = "EidosBlock",

  public = list(

    initialize = function(eidos_lines)
    {
      for(line in eidos_lines)
      {
        self$add_line(line)
      }

      self$set_callback()
    },

    add_line = function(line, after_line_number = length(self$lines))
    {
      private$lines_private <- append(
        x = self$lines,
        value = line,
        after = after_line_number
      )
    },

    remove_line = function(line_number)
    {
      private$lines_private <- private$lines_private[-line_number]
    },

    substitute_phrase = function(phrase, substitute)
    {
      for(line in private$lines_private)
      {
        line$substitute_phrase(phrase, substitute)
      }
    },

    set_callback = function(overwrite_existing = NULL)
    {
      CALLBACK_LINE_INDEX <- 1
      READ_CALLBACK_FROM_LINE <- is.null(overwrite_existing)

      if (READ_CALLBACK_FROM_LINE)
      {
        current_callback <- self$lines[[CALLBACK_LINE_INDEX]]$callback
        private$callback_private <- current_callback
        return()
      }

      private$lines_private[[CALLBACK_LINE_INDEX]]$substitute_phrase(
        phrase = private$callback_private,
        substitute = new_callback
      )
      self$set_callback()
    }

  ),

  active = list(

    callback = function()
    {
      return(private$callback_private)
    },

    lines = function()
    {
      return(private$lines_private)
    }

  ),

  private = list(

    callback_private = "",
    lines_private = list()

  )

)
