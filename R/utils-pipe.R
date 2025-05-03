#' Pipe operator
#'
#' See `magrittr:pipe` `%>%` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @returns The result of calling `rhs(lhs)`.
NULL
