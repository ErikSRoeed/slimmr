# ScriptBLock.R

# This file contains the definition of the ScriptBlock class
# This file is part of the R package slimmr

ScriptBlock <- R6Class("ScriptBlock",

  inherit = Script,

  public = list(

    initialize = function(type, index, prewritten = NULL, ...) {

      self$type <- type
      self$index <- index

      if (is.null(prewritten)) {
        private$script <- readLines(paste("SLiM/block_", type, ".slim", sep = ""))
        arg = list(...)
        argnames = names(arg)
        self$replacetext(NULL, as.character(argnames), as.character(arg), display = FALSE)
      } else {
        private$script <- prewritten
      }

    },

    writein = function(newscript) private$script <- newscript,
    writeout = function() return(private$script),
    reindex = function(newindex) self$index <- newindex,

    type = c(),
    index = 0

  )

)
