
#' EidosLine
#'
#' @description Internal class for Eidos line objects.
#'
#' @noRd
#'
EidosLine <- R6::R6Class(

  classname = "EidosLine",

  public = list(

    initialize = function(string, number)
    {
      self$overwrite_string(new_string = string)
      self$increment_number(by = number)
    },

    overwrite_string = function(new_string)
    {
      private$string_private <- new_string
    },

    increment_number = function(by)
    {
      private$number_private <- self$number + by
    },

    substitute_phrase = function(phrase, substitute)
    {
      with_substitutions <- gsub(phrase, substitute, self$string)
      self$overwrite_string(new_string = with_substitutions)
    }

  ),

  active = list(

    number = function()
    {
      return(private$number_private)
    },

    string = function()
    {
      return(private$string_private)
    },

    is_toplevel = function()
    {
      EMPTY_LINE <- ""
      NON_TOPLEVEL_FIRST_CHARACTERS <- c("  ", "\t", "//")

      if (self$string == EMPTY_LINE)
      {
        return(FALSE)
      }

      first_characters <- substr(self$string, start = 1, stop = 2)

      if (first_characters %in% NON_TOPLEVEL_FIRST_CHARACTERS)
      {
        return(FALSE)
      }

      return(TRUE)
    },

    callback = function()
    {
      REGEX_OPENING_BRACKET <- "\\{"
      BLANK <- ""

      if (! self$is_toplevel)
      {
        return(NULL)
      }

      string_has_opening_bracket <- grepl(REGEX_OPENING_BRACKET, self$string)

      if (! string_has_opening_bracket)
      {
        return(NULL)
      }

      string_without_bracket <- gsub(REGEX_OPENING_BRACKET, BLANK, self$string)
      callback_string <- trimws(string_without_bracket)
      return(callback_string)
    }

  ),

  private = list(

    number_private = 0,
    string_private = ""

  )

)
