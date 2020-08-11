# slimcode.R

# This file contains the utility function slimcode, which returns a requested slim scaffold or snippet as a character vector
# This file is part of the R package slimmr

#' Internal function: slimcode
#'
#' @param type As a string, the required slim code
slimcode <- function(type) {
  switch(type,

    block_event = return(
      c("%1%%2%%3% {",
        "}"
        )
    ),

    block_initialize = return(
      c("initialize() {",
        "  initializeSLiMModelType('%1%');",
        "  initialiseSLiMOptions(dimensionality = '%2%', periodicity = '%3%');",
        "  initialiseSex('%4%')",
        "  initializeMutationRate(%5%);",
        "  initializeRecombinationRate(%6%);",
        "}"
        )
    )

  )
}
