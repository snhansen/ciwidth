valid_range <- function(x, lower = -Inf, upper = Inf) {
  if (!is.numeric(x)) {
    return(FALSE)
  }
  if (any(x <= lower | x >= upper)) {
    return(FALSE)
  }
  return(TRUE)
}
