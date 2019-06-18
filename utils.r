force_digits <- function (num, n_digits) {
  return (format(round(num, n_digits), nsmall=n_digits))
}