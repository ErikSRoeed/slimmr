# Script.R

# This file contains the definition of the Script class
# This file is part of the R package slimmr

#' Class: Script
#'
#' @description Internal superclass for models and scriptblocks
#'
Script <- R6::R6Class("Script",

  public = list(

    initialize = function() {},

    inspect = function(linenumshift = 0) {
      for(nline in 1 : length(private$script)) {
        cat(nline + linenumshift, paste(rep(" ", nchar(as.character(length(private$script))) - nchar(as.character(nline))), collapse = ""), private$script[nline], fill = TRUE)
      }
    },

    addlines = function(add, after, display = FALSE) {
      private$script <- append(private$script, add, after = after)
      if (display) self$inspect()
    },

    removelines = function(remove, display = FALSE) {
      private$script <- private$script[-remove]
      if (display) self$inspect()
    },

    replacetext = function(inlines = NULL, oldtext, newtext, display = FALSE) {
      for(line in if(is.null(inlines)) 1 : length(private$script) else inlines) {
        for(i in 1 : length(oldtext)) {
          private$script[line] <- gsub(oldtext[i], newtext[i], private$script[line], fixed = TRUE)
        }
      }
      if (display) self$inspect()
    }

  ),

  private = list(

    script = c(),

    findinline = function(tofind, after = 1, first = FALSE) {
      searchresults = sapply((after + 1) : length(private$script), function(l) regexpr(tofind, private$script[l], fixed = TRUE))
      foundlines = ((after + 1) : length(private$script))[which(searchresults != -1 & if(first) searchresults == 1)]
      return(foundlines)
    }

  )

)
