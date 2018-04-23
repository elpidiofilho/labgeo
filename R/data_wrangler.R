back <- function(data, ...) {
  dots <- quos(...)
  ndots <- map(dots, function(q) expr(-!!q))
  select(data, !!!ndtos, !!!dots)
}

front <- function(data, ...) {
  select(data, ..., everything())
}
