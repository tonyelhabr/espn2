

.zip2_query <-
  function(x, y = deparse(substitute(x)), sep = "=") {
    paste0(y, sep, x)
  }

# Reference: https://stackoverflow.com/questions/5831794/opposite-of-in.
`%ni%` <- function(x, y) {
  !('%in%'(x,y))
}

# "%ni%" <- Negate("%in%")

# Reference: https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
