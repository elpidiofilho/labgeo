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

summary_lost <- function(d) {
  dm <- purrr::map_df(d, n_miss) %>%
    tidyr::gather(key = "variables", value = "lost") %>%
    dplyr::mutate(pct_lost = round(100 * lost / nrow(d), 3)) %>%
    dplyr::mutate(class = sapply(d, class)) %>%
    dplyr::mutate(unique = sapply(d, n_distinct), pct_unique = round(100* unique/(nrow(d)), 3)) %>%
    dplyr::select(variables, class, lost, pct_lost, unique, pct_unique)
  return(dm)
}



library(rlang)
library(dplyr)
library(purrr)

replace_when <- function(data, target, ...){
  target <- ensym(target)
  mutate( data, !!target := case_when( !!!quos(...), TRUE ~ !!target ) )
}

d <- data.frame(x = 1:10, y = 1:10)
d %>% replace_when(y,
                   x < 3 ~ 2L,
                   x > 7 ~ 8L
)




library(rlang)
library(dplyr)
library(purrr)

mutate_when <- function(data, condition, ...){
  condition <- enquo(condition)

  dots <- exprs(...)

  expressions <- map2( dots, syms(names(dots)), ~{
    quo( case_when(..condition.. ~ !!.x , TRUE ~ !!.y ) )
  })

  data %>%
    mutate( ..condition.. = !!condition ) %>%
    mutate( !!!expressions ) %>%
    select( -..condition..)
}

d <- data.frame( x = 1:4, y = 1:4)
mutate_when( d, x < 3,
             x = -x,
             y = -y
)


back <- function(data, ...){
  dots <- quos(...)

  # negate each expression
  ndots <- map(dots, function(q) expr(-!!q) )

  # select the negated (rm the columns) and then select them back
  select( data, !!!ndots, !!!dots )
}

front <- function(data, ...){
  select( data, ..., everything() )
}

library(dplyr)
library(purrr)
back(iris, Species) %>% head
back(iris, Species, starts_with("Petal")) %>% head
front(iris, Species) %>% head

