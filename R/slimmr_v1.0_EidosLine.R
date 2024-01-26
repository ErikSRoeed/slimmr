
#' EidosLine
#'
#' @description Internal class for Eidos line objects.
#'
#' @noRd
#'
EidosLine <- R6::R6Class(

  classname = "EidosLine",

  public = list(

    initialize = function(eidos_string)
    {
      self$overwrite(new_string = eidos_string)
    },

    overwrite = function(new_string)
    {
      private$string_private <- new_string
    },

    substitute = function(phrase, substitute)
    {
      with_substitutions <- gsub(phrase, substitute, self$string, fixed = TRUE)
      self$overwrite(new_string = with_substitutions)
    },

    read_head = function(n_characters, ignore_whitespace = FALSE)
    {
      HEAD_START <- 1
      string <- self$string

      if (ignore_whitespace)
      {
        string <- trimws(string)
      }

      head <- substr(string, start = HEAD_START, stop = n_characters)
      return(head)
    }

  ),

  active = list(

    string = function()
    {
      return(private$string_private)
    },

    is_empty = function()
    {
      EMPTY_LINE <- ""

      if (self$string == EMPTY_LINE)
      {
        return(TRUE)
      }
      return(FALSE)
    },

    is_toplevel = function()
    {
      NOT_TOPLEVEL_HEAD <- c("  ", "\t")

      if (self$read_head(n_characters = 2) %in% NOT_TOPLEVEL_HEAD)
      {
        return(FALSE)
      }
      return(TRUE)
    },

    is_comment = function()
    {
      COMMENT_HEAD <- "//"

      if (self$read_head(n_characters = 2, ignore_whitespace = TRUE) == COMMENT_HEAD)
      {
        return(TRUE)
      }
      return(FALSE)
    },

    is_callback = function()
    {
      if (! is.null(self$callback))
      {
        return(TRUE)
      }
      return(FALSE)
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

      callback <- gsub(REGEX_OPENING_BRACKET, BLANK, self$string) |> trimws()
      return(callback)
    }

  ),

  private = list(

    string_private = ""

  )

)
