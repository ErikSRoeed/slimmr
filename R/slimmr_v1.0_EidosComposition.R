
#' EidosComposition
#'
#' @description Internal superclass for Eidos block and model objects.
#'
#' @noRd
#'
EidosComposition <- R6::R6Class(

  classname = "EidosComposition",

  public = list(

    initialize = function(initial_elements)
    {
      for (element in initial_elements)
      {
        self$add_element(element)
      }
    },

    add_element = function(element, after_index = length(private$elements))
    {
      private$elements <- append(private$elements, element, after_index)
    },

    remove_element = function(element_index)
    {
      private$elements <- private$elements[-element_index]
    }

  ),

  private = list(

    elements = list()

  )

)
