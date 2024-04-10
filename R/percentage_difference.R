percentage_difference <- function(value, value_two) {
  (value - value_two) / ((value + value_two) / 2) * 100
}
