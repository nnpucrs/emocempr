#' Test if a numeric input is an integer
#'@param x numeric value to test
#'@param tol limit defined as a really low number


is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
        abs(x - round(x)) < tol
}
