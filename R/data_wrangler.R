## https://gist.github.com/romainfrancois
back <- function(data, ...) {
  dots <- dplyr::quos(...)
  ndots <-  purrr::map(dots, function(q) expr(-!!q))
  dplyr::select(data, !!!ndtos, !!!dots)
}

front <- function(data, ...) {
  dplyr::select(data, ..., everything())
}

lags <- function(var, n = 3){
  var <- enquo(var)

  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>%
    set_names(sprintf("lag_%s_%02d", quo_text(var), indices))

}


na_set <- function(x, p){
  p <- as_mapper(p)
  x[p(x)] <- NA
  x
}

naset_all <- function(data, p){
  mutate_all(data, funs(na_set(., p)) )
}

naset_at <- function(data, at, p){
  mutate_at(data, at, funs(na_set(., p)) )
}

naset_if <- function(data, test, p){
  mutate_if(data, test, funs(na_set(., p)) )
}
