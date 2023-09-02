
pretty_round <- function(x, allow_negative_round=FALSE) {
  round(x, get_pretty_digits(x, allow_negative_round = allow_negative_round))
}

get_pretty_digits <- function(x, allow_negative_round=FALSE) {
  max_x <- max(abs(x))

  dig = 0

  if(allow_negative_round && max_x > 100 ) {
    dig = -1
  } else if(max_x < 1) {
    dig = abs(floor(log10(max_x)))
  }

  dig
}


pretty.character <- function(x, ...,  upper=c('first', 'all', 'none')) {

  cap_first_letter <- function(s) {
    paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)), collapse='')
  }

  upper = match.arg(upper)

  str <- stringr::str_split(x, '_')[[1]]

  if(upper == 'first') {
    str[1] %<>% cap_first_letter
  } else if (upper == 'all') {
    str %<>% sapply(cap_first_letter)
  }

  return(paste(str, collapse=" "))
}

unpretty <- function(str, ...) {
  UseMethod('unpretty')
}

unpretty.default <- function(str, ...) {
  return(str)
}

unpretty.character <- function(str, ...) {
  tolower(stringr::str_replace_all(str, " ", '_'))
}
