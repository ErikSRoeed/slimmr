# SlimModel.R

# This file contains the definition of the SlimModel class
# This file is part of the R package slimmr

SlimModel <- R6Class("SlimModel",

  inherit = Script,

  public = list(

    initialize = function(description, scriptfile = NULL, type = "WF", mu = 1e-8, rho = 1e-8, sex = "", dimensions = "", periodicity = "") {
      if (!is.null(scriptfile)) {
        private$script <- readLines(scriptfile)
        copyfile <- paste(gsub(".slim", "", scriptfile), "_slimmr.slim", sep = "")
        file.copy(from = scriptfile, to = copyfile)
        private$filename <- copyfile
        private$tmp <- FALSE
      } else {
        private$scriptblocks <- append(private$scriptblocks,
                                       ScriptBlock$new(type = "initialize", '%typ%' = type, '%dim%' = dimensions,
                                                       '%per%' = periodicity, '%sex%' = sex, '%mu%' = mu, '%rho%' = rho)
                                       )
        private$script <- private$writefromblocks()
        private$filename <- tempfile("slimmr_", fileext = ".slim")
        private$tmp <- TRUE
        writeLines(private$script, private$filename)
      }
      private$description <- description
    },

    print = function() {
      cat("-==|| slimmr SLiM model ||===========================================-\n\n")
      cat(private$description, "\n\n")
      cat(ifelse(private$tmp, "Temporary file: ", "File: "), private$filename, "\n")
      cat("\n-====================================================================-")
    },

    savetofile = function(path, filename) {
      newname <- paste(ifelse(substr(path, nchar(path), nchar(path)) == "/", path, paste(path, "/", sep = "")), filename, ".slim", sep = "")
      file.copy(private$filename, to = newname)
      private$filename <- newname
      private$tmp <- FALSE
    },

    run = function(...) {
      args <- list(...)
      slimargs <- paste(lapply(names(args), function(arg) sprintf("-d %s=%f", arg, args[arg])), collapse = " ")
      syscall <- paste("slim ", slimargs, " ", private$filename, sep = "")
      return(system(slimcall_str, intern = TRUE))
    },

    addlines = function(add, after) {
      super$addlines(add, after, display = TRUE)
      private$updateblocks()
    },

    removelines = function(remove) {
      super$removelines(remove, display = TRUE)
      private$updateblocks()
    },

    replacetext = function(inlines = NULL, oldtext, newtext) {
      super$replacetext(inlines, oldtext, newtext, display = TRUE)
      private$updateblocks()
    }

  ),

  private = list(

    writefromblocks = function() {
      script <- c()
      for (block in private$scriptblocks) script <- append(script, block$writeout())
      return(script)
    },

    updateblocks = function() {
      for (block in private$scriptblocks) {
        firstline <- paste(block$type, "(", sep = "")
        lastline <- "}"
        scriptfirst <- private$findinline(firstline, after = 0, first = TRUE)
        scriptlast <- private$findinline(lastline, after = scriptfirst, first = TRUE)
        block$writein(private$script[scriptfirst : scriptlast])
      }
    },

    tmp = TRUE,
    description = c(),
    filename = c(),
    scriptblocks = list(),
    mutationtypes = list(),
    genomicelementtypes = list(),
    genomicelements = list(),
    subpopulations = list(),
    interactiontypes = list()
  )

)
