#' Set dimensions and resolution of a plot in an R kernel-based Jupyter
#' notebook.
#'
#' @param width Width in inches.
#' @param height Height in inches.
#' @param res PPI for rasterization.
#'
#' @export
set_dim <- function(width, height, res = 150) {
  options(repr.plot.width = width,
          repr.plot.height = height,
          repr.plot.res = res)
}


#' Convience function that is a complement to \%in\%.
#'
#' @param x Vector of elements to be matched.
#' @param vec Vector of elements to be matched against.
#'
#' @return Logical vector of the same length as x.
#'
#' @export
'%notin%' <- function(x, vec) {
  match(x, vec, nomatch = 0L) == 0L
}


#' Paste two strings using an infix operator.
#'
#' @param a Character string.
#' @param b Character string.
#'
#'  @return Character string as paste0(a, b).
#'
#' @export
`%+%` <- function(a, b) {
  paste0(a, b)
}


#' Shortcut for suppressMessages().
#' @param expr Expression to evaluate.
#' @export
sm <- function(expr) suppressMessages(expr)


#' Shortcut for suppressPackageStartupMessages().
#' @param expr Expression to evaluate.
#' @export
ssm <- function(expr) suppressPackageStartupMessages(expr)


#' Split values into discrete, equal sized bins.
#'
#' @param x Values to bin.
#' @param n Number of bins.
#' @param num_labels Label the bins with simple sequence of numbers?
#'
#' @return Factor of bins.
bin_quantiles <- function(x, n, num_labels = FALSE) {
  breaks <- quantile(x, seq(0, 1, 1 / n))
  labels = if (num_labels) 1 : n else NULL
  cut(x, breaks, levels = breaks, include.lowest = TRUE, labels = labels)
}


#' Split values into discrete bins.
#'
#' @param x Values to bin.
#' @param breaks A vector of cut points or a single number determining
#'     the number of bins.
#'
#' @return Factor of bins.
bin_values <- function(x, breaks) {
  cut(x, breaks, levels = breaks, include.lowest = TRUE)
}
