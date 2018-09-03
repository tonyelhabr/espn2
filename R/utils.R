
# Reference: https://stackoverflow.com/questions/5831794/opposite-of-in.
`%ni%` <- function(x, y) {
  !('%in%'(x,y))
}

# "%ni%" <- Negate("%in%")

# Reference: https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
is.likeinteger <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# NOTE: Inspired by `set_tibble_class()`.
# (See https://github.com/tidyverse/tibble/blob/master/R/new.R.)
.add_class <-
  function(data, ..., sep = "_", where = c("after", "before")) {
    where <- match.arg(where)
    cls <- class(data)
    cls_new <- paste(..., sep = sep)
    if(where == "after") {
      clss <- c(cls, cls_new)
    } else {
      clss <- c(cls_new, cls)
    }
    class(data) <- clss
    data
  }

.unlistname <-
  function(x) {
    unname(unlist(x))
  }

