
#' EidosBlock
#'
#' @description Internal class for Eidos block objects.
#'
#' @noRd
#'
EidosBlock <- R6::R6Class(

  classname = "EidosBlock",

  public = list(

    initialize = function(index, lines)
    {
      for(line in lines)
      {
        self$add_line(line, after_line_number = line$number - 1)
      }

      self$set_index(to = index)
      self$set_callback()
    },

    add_line = function(line, after_line_number)
    {
      private$lines_private <- append(
        x = self$lines,
        value = line,
        after = after_line_number
      )

      added_line_number <- after_line_number + 1
      self$increment_line_numbers(after_line = added_line_number, by = 1)
    },

    remove_line = function(line_number)
    {
      private$lines_private <- private$lines_private[-line_number]
      self$increment_line_numbers(after_line = line_number, by = -1)
    },

    increment_line_numbers = function(after_line, by)
    {
      for (line in private$lines_private)
      {
        if (line$number <= after_line)
        {
          next
        }
        line$increment_number(by = by)
      }
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
    },

    set_index = function(to)
    {
      private$index_private <- to
    }

  ),

  active = list(

    callback = function()
    {
      return(private$callback_private)
    },

    index = function()
    {
      return(private$index_private)
    },

    lines = function()
    {
      return(private$lines_private)
    }

  ),

  private = list(

    callback_private = "",
    index_private = 0,
    lines_private = list()

  )

)
