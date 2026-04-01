#' @importFrom dplyr %>%
#' @noRd
#' @keywords internal
NULL

#' Inverted versions of `%in%`
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
`%not_in%` <- Negate(`%in%`)
