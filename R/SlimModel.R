# SlimModel.R

# This file contains the definition of the SlimModel class
# This file is part of the R package slimmr

#' Class: SlimModel
#'
#' @import R6
#' @export
#'
SlimModel <- R6Class("SlimModel",

  inherit = Script,

  public = list(

    initialize = function(description, scriptfile = NULL, generations = 1000, type = "WF", mu = 1e-8, rho = 1e-8, sex = NULL, dimensions = NULL, periodicity = NULL) {
      if (!is.null(scriptfile)) {
        copyfile <- paste(gsub(".slim", "", scriptfile), "_slimmr.slim", sep = "")
        writeLines(readLines(scriptfile), copyfile)
        private$filename <- copyfile
        private$tmp <- FALSE
        private$script <- readLines(private$filename)
        private$getblocks()
      } else {
        private$filename <- tempfile("slimmr_", fileext = ".slim")
        private$tmp <- TRUE
        self$addblock(blocktype = "initialize", index = 1, type = type, mu = mu, rho = rho, sex = sex, dimensions = dimensions, periodicity = periodicity)
        self$addblock(blocktype = "event", index = 2, generation = generations, timing = "late", script = "  sim.simulationFinished();")
      }
      private$description <- description
      private$updatemodel()
    },

    print = function() {
      cat("-/ slimmr SLiM model /----------------------------------------------------\n\n\n")
      cat(" ", private$description, "\n\n", sep = "")
      cat(" Lines: ", length(private$script), "\n")
      cat(" Blocks:")
      firstb <- TRUE
      for(b in private$scriptblocks) {
        cat(ifelse(firstb, " [", "         ["), b$index, "] ", b$type, "\n")
        firstb <- FALSE
      }
      cat("\n", ifelse(private$tmp, "Temporary file: ", "File: "), private$filename)
      cat("\n\n\n-/ slimmr v 0.1.0 /-------------------------------------------------------")
    },

    setup_genome = function() {},

    setup_population = function() {},

    add_mutationtype = function() {},

    add_interactiontype = function() {},

    addblock = function(index, blocktype, ...) {

      block <- switch(blocktype,
                      initialize = private$addblock_initialize(index, ...),
                      event = private$addblock_event(index, ...))

      private$scriptblocks <- append(private$scriptblocks, block)
      private$updatemodel()
    },

    inspectblock = function(index) {
      if (index > 1) {
        beginatline <- sum(sapply((1 : (index - 1)), function(b) length(private$scriptblocks[[b]]$writeout())))
      } else {
        beginatline <- 0
      }
      private$scriptblocks[[index]]$inspect(linenumshift = beginatline)
    },

    savetofile = function(path, filename) {
      private$filename <- paste(ifelse(substr(path, nchar(path), nchar(path)) == "/", path, paste(path, "/", sep = "")), filename, ".slim", sep = "")
      private$tmp <- FALSE
      private$updatemodel()
    },

    run = function(...) {
      private$updatemodel()
      args <- list(...)
      slimargs <- paste(lapply(names(args), function(arg) sprintf("-d %s=%f", arg, args[arg])), collapse = " ")
      syscall <- paste("slim ", slimargs, " ", private$filename, sep = "")
      return(system(syscall, intern = TRUE))
    },

    addlines = function(add, after) {
      super$addlines(add, after, display = FALSE)
      private$updatemodel(updateblocks = TRUE)
    },

    removelines = function(remove) {
      super$removelines(remove, display = FALSE)
      private$updatemodel(updateblocks = TRUE)
    },

    replacetext = function(inlines = NULL, oldtext, newtext) {
      super$replacetext(inlines, oldtext, newtext, display = FALSE)
      private$updatemodel(updateblocks = TRUE)
    },

    moveblock = function(index, to) {
      if (index == to) return()

      to <- ifelse(to > length(private$scriptblocks), length(private$scriptblocks), to)
      shift <- ifelse(index > to, 1, -1)
      betweens <- seq(to, index - shift, shift)

      private$scriptblocks[[index]]$reindex(to)
      for (b in betweens) private$scriptblocks[[b]]$reindex(b + shift)

      private$updatemodel()
    },

    removeblock = function(index) {
      private$scriptblocks <- private$scriptblocks[- index]

      if (index != length(private$scriptblocks) + 1) {
        afters <- seq(index + 1, length(private$scriptblocks) + 1)
        for (a in afters) private$scriptblocks[[which(lapply(private$scriptblocks, function(block) block$index) == a)]]$reindex(a - 1)
      }

      private$updatemodel()
    }

  ),

  private = list(

    addblock_initialize = function(index, type = "WF", mu = 1e-8, rho = 1e-8, sex = NULL, dimensions = NULL, periodicity = NULL) {
      block <- ScriptBlock$new(type = "initialize()", '%1%' = type, '%2%' = dimensions, '%3%' = periodicity, '%4%' = sex, '%5%' = mu, '%6%' = rho,
                               index = ifelse(index > length(private$scriptblocks) + 1, length(private$scriptblocks) + 1, index))

      if(is.null(sex)) block$removelines(4)
      if(is.null(dimensions) && is.null(periodicity)) block$removelines(3)
      if(is.null(periodicity) && !is.null(dimensions)) block$replacetext(inlines = 3, oldtext = ", periodicity = 'NULL", newtext = "")

      return(block)
    },

    addblock_event = function(index, generation, until = NULL, timing = NULL, script = "  ") {

      until <- ifelse(is.null(until), "", paste(" : ", until, sep = ""))
      timing <- ifelse(is.null(timing), "", paste(" ", timing, "()", sep = ""))

      block <- ScriptBlock$new(type = "event", '%1%' = generation, '%2%' = until, '%3%' = timing,
                               index = ifelse(index > length(private$scriptblocks) + 1, length(private$scriptblocks) + 1, index))

      block$type <- paste(generation, until, timing, sep = "")
      block$addlines(add = script, after = 1)

      return(block)
    },

    addblock_fitness = function() {},

    addblock_mateChoice = function() {},

    addblock_modifyChild = function() {},

    addblock_recombination = function() {},

    addblock_interaction = function() {},

    addblock_reproduction = function() {},

    addblock_mutation = function() {},

    updatemodel = function(updateblocks = FALSE) {
      if(updateblocks) private$updateblocks()
      private$orderblocks()
      private$script <- private$writefromblocks()
      private$writefile()
    },

    writefile = function() writeLines(private$script, private$filename),

    writefromblocks = function() {
      script <- c()
      for (block in private$scriptblocks) script <- append(script, block$writeout())
      return(script)
    },

    updateblocks = function() {
      previous = 0
      for (block in private$scriptblocks) {
        firstline <- paste(block$type, sep = "")
        lastline <- "}"
        scriptfirst <- private$findinline(firstline, after = previous, first = TRUE)[1]
        scriptlast <- private$findinline(lastline, after = scriptfirst, first = TRUE)[1]
        previous <- scriptlast
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

        private$scriptblocks <- append(private$scriptblocks, ScriptBlock$new(type = type, index = (t + 1)/2, prewritten = private$script[toplevel[t] : toplevel[(t + 1)]]))
      }
    },

    orderblocks = function() {
      indices = sapply((1 : length(private$scriptblocks)), function(b) private$scriptblocks[[b]]$index)
      private$scriptblocks <- lapply((1 : length(indices)), function(i) private$scriptblocks[[which(indices == i)]])
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
