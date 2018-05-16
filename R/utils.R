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
