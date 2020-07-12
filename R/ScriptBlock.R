# ScriptBLock.R

# This file contains the definition of the ScriptBlock class
# This file is part of the R package slimmr

ScriptBlock <- R6Class("ScriptBlock",

  inherit = Script,

  public = list(

    initialize = function(type, ...) {
      self$type <- type
      private$script <- readLines(paste("SLiM/block_", type, ".slim", sep = ""))

      arg = list(...)
      argnames = names(arg)

      self$replacetext(NULL, as.character(argnames), as.character(arg), display = FALSE)
    },

    writein = function(newscript) private$script <- newscript,

    writeout = function() return(private$script),

    type = c()

  )

)
