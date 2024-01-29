#' check_list_contains_only_correct_class
#'
#' @description Internal slimmr function.
#'
#' @param list A list() of objects.
#' @param correct_class The desired class of the objects in list
#' @returns TRUE/FALSE
#'
#' @noRd
#'
check_list_contains_only_correct_class <- function(list, correct_class)
{
  check_class <- function(object)
  {
    is_correct <- as.character(correct_class) %in% class(object)
    return(is_correct)
  }

  class_checks <- vapply(list, check_class, FUN.VALUE = logical(1))
  return(class_checks)
}
