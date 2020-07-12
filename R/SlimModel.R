# SlimModel.R

# This file contains the definition of the SlimModel class
# This file is part of the R package slimmr

SlimModel <- R6Class("SlimModel",

  inherit = Script,

  public = list(

    initialize = function(description, scriptfile = NULL, type = "WF", mu = 1e-8, rho = 1e-8, sex = "", dimensions = "", periodicity = "") {
      if (!is.null(scriptfile)) {
        private$script <- readLines(scriptfile)
        private$getblocks()
        copyfile <- paste(gsub(".slim", "", scriptfile), "_slimmr.slim", sep = "")
        file.copy(from = scriptfile, to = copyfile)
        private$filename <- copyfile
        private$tmp <- FALSE
      } else {
        private$scriptblocks <- append(private$scriptblocks,
                                       ScriptBlock$new(type = "initialize()", '%typ%' = type, '%dim%' = dimensions,
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
      cat("-/ slimmr SLiM model /-\n\n\n")
      cat("  ", private$description, "\n\n")
      cat(" Lines: ", length(private$script), "\n")
      cat(" Blocks: ", length(private$scriptblocks), "\n")
      cat("\n", ifelse(private$tmp, "Temporary file: ", "File: "), private$filename)
      cat("\n\n\n-/ slimmr v 0.1.0 /-")
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
        firstline <- paste(block$type, sep = "")
        lastline <- "}"
        scriptfirst <- private$findinline(firstline, after = 0, first = TRUE)
        scriptlast <- private$findinline(lastline, after = scriptfirst, first = TRUE)
        block$writein(private$script[scriptfirst : scriptlast])
      }
    },

    getblocks = function() {
      toplevel <- which(sapply(private$script, function(l) {
        l != "" &&
          !grepl("  ", substr(l, 1, 2), fixed = TRUE)[1] &&
          !grepl("\t", substr(l, 1, 2), fixed = TRUE)[1] &&
          !grepl("//", substr(l, 1, 2), fixed = TRUE)[1]
      }))

      for(t in seq(1, length(toplevel), 2)) {
        header <- names(toplevel)[t]

        if(suppressWarnings(is.na(as.integer(strsplit(header, "", fixed = TRUE)[[1]][1])))) {
          if(substr(header, 1, 8) == "function") {
            type <- substr(header, regexpr("(", header, fixed = TRUE)[1], tail(gregexpr(")", func, fixed = TRUE)[[1]], 1))
          } else {
            type <- trimws(sub("{", "", header, fixed = TRUE))
          }
        } else {
          type <- ifelse(grepl("(", header, fixed = TRUE), substr(header, 1, regexpr(")", header, fixed = TRUE)[1]), trimws(sub("{", "", header, fixed = TRUE)))
        }

        private$scriptblocks <- append(private$scriptblocks, ScriptBlock$new(type = type, prewritten = private$script[toplevel[t] : toplevel[(t + 1)]]))
      }
    },

    deep_clone = function(name, value) {
      if(name == "filename") return(tempfile("slimmr_", fileext = ".slim"))
      if(name == "tmp") return(TRUE)
      if(name == "scriptblocks") return(lapply(private$scriptblocks, function(sb) sb$clone(deep = TRUE)))
      else return(value)
    },

    tmp = TRUE,
    description = "",
    filename = "",
    scriptblocks = list(),
    mutationtypes = list(),
    genomicelementtypes = list(),
    genomicelements = list(),
    subpopulations = list(),
    interactiontypes = list()
  )

)
