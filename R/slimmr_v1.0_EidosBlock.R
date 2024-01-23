
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
    },

    add_line = function(line, after_line_number = length(self$lines))
    {
      private$lines_private <- line |>
        append(x = self$lines, after = after_line_number)
    },

    remove_line = function(line_number)
    {
      private$lines_private <- private$lines_private[-line_number]
    },

    substitute_phrase_in_lines = function(line_numbers, phrase, substitute)
    {
      for(line in private$lines_private[line_numbers])
      {
        line$substitute_phrase(phrase, substitute)
      }
    },

    change_callback = function(new_callback)
    {
      FIRST_LINE <- 1
      self$substitute_phrase_in_lines(FIRST_LINE, self$callback, new_callback)
    }

  ),

  active = list(

    callback = function()
    {
      FIRST_LINE <- 1
      callback <- self$lines[[FIRST_LINE]]$callback
      return(callback)
    },

    lines = function()
    {
      return(private$lines_private)
    }

  ),

  private = list(

    lines_private = list()

  )

)
