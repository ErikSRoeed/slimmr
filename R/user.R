# user.R

# This file contains the user-available functions of slimmr, including generalised generator functions.
# This file is part of the R package slimmr

#' Function: newSlimModel
#'
#' @param description A character description of the model.
#' @param scriptfile If using a prewritten model, the full path to the model. Otherwise NULL.
#' @param generations If creating model from scratch, integer value or constant symbol.
#' @param type If creating model from scratch, 'WF' or 'nonWF'.
#' @param mu If creating model from scratch, mutation rate.
#' @param rho If creating model from scratch, recombination rate.
#' @param sex If creating model from scratch, NULL, 'A', 'X', or 'Y'.
#' @param dimensions If creating model from scratch, NULL, 'XY', or 'XYZ'.
#' @param periodicity If creating model from scratch, 'NULL' or...
#'
#' @description Generate an object of class SlimModel.
#'
#' @export
#'
newSlimModel <- function(description, scriptfile = NULL, generations = 1000, type = "WF", mu = 1e-8, rho = 1e-8, sex = NULL, dimensions = NULL, periodicity = NULL) {
  return(SlimModel$new(description, scriptfile, generations, type, mu, rho, sex, dimensions, periodicity))
}
