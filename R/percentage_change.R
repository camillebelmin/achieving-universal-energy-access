percentage_change <- function(previous, new, as_decimal = FALSE) {
  x <- ((new - previous) / previous) * 100
  if (as_decimal) x <- x / 100
  return(x)
}
