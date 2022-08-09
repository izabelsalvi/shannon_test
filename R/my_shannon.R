#' Function Shannon
#'
#' @param x Avector of numeric values representig the
#' abundance species in a community
#'
#' @return Shannon diversity value
#' @export
#'
#' @examples
#' com <- c(1, 2, 3)
#' my_shannon(com)
my_shannon <- function(x) {
  x <- x[x > 0]
  sum_all <- sum(x)
  pi <- x/sum_all
  -sum(pi*log(pi))
}
