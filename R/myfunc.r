#' My first function
#'
#' @param a A numerical vector.
#' @param b Also a numerical vector.
#'
#' @return A numerical vector of a + b * a.
#' @export
#'
#' @examples
#' myfunc(3, 5)
myfunc <- function(a, b) {
  result <- a * b + a
  return(result)
}

#' My second function
#'
#' @param y A numerical vector.
#' @param X A matrix.
#'
#' @return A numerical vector of \eqn{(X'X)^{-1} X' y}.
#' @export
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
#' y <- c(1, 2, 3)
#' myfunc(y, X)
estimate_beta <- function(y, X) {
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_hat)
}

#' My third function
#'
#' @return A ggplot2 theme object.
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   my_theme()
my_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", color = "blue"),
      axis.text = ggplot2::element_text(size = 12, color = "darkred"),
      panel.grid.major = ggplot2::element_line(color = "grey80", linetype = "dashed")
    )
}



