library(stringr)


#' Converts special defined lambda expression format to R function
#'
#' @param expr formula to convert
#' @return Function for applying
#' @export
#' @examples
#' lambda_to_func(x ~ {
#'     a <- x + 1
#'     return (a * 2)
#' })(3)
#' # 8
lambda_to_func <- function(expr) {
  expr <- as.expression(expr)

  expr_str <- as.character(expr, collapse='')
  expr_str_splitted <- strsplit(expr_str, split='~', fixed=TRUE)[[1]]
  vars <- c()
  for (v in expr_str_splitted[1:(length(expr_str_splitted) - 1)]) {
    if (v != '') {
      vars <- c(vars, v)
    }
  }

  vars_string <- ''
  if (length(vars) == 0) {
    vars_string <- ''
  }
  else if (length(vars) == 1) {
    vars_string <- as.character(vars)
  }
  else {
    for (var in vars) {
      vars_string <- paste(vars_string, as.character(var), sep=',')
    }
    vars_string <- sub('.', '', vars_string)
  }

  if (vars_string == '')
    vars_string <- str_extract(expr_str, '[a-zA-Z_]+')

  if (is.na(vars_string))
    vars_string <- '.'

  return (eval(parse(text=paste('(function(', vars_string, ') {', expr_str_splitted[length(expr_str_splitted)], '})'))))
}


#' Returns functor for 'map'-like applying
#'
#' @param init Initial vector
#' @return Functor
#' @export
#' @examples
#' init(c(1, 2, 3, 4))()
#' # c(1, 2, 3, 4)
#' init(c(1, 2, 3, 4))(function(x) {x * 2})()
#' # c(2, 4, 6, 8)
functor <- function(init=c(0)) {
  return (function(change=NULL) {
    if (is.null(change)) {
      return (init)
    }
    else if (is.language(change)) {
      change <- lambda_to_func(change)
      return ( functor(change(init)) )
    }
    else {
      return (functor(change(init)))
    }
  })
}


#' @export
map <- function(x, f) {
  stopifnot(is.vector(x))

  if (is.language(f)) {
    return (lambda_to_func(f)(x))
  }

  return (f(x))
}
#' @export
`%map%` <- map


#' @export
mapN <- function(x, f) {
  stopifnot(is.list(x))

  if (is.language(f)) {
    return (do.call(lambda_to_func(f), x))
  }

  return (do.call(f, x))
}
#' @export
`%mapN%` <- mapN


#' @export
invoke_map <- function(x, f) {
  stopifnot(is.list(x))
  stopifnot(is.vector(f))

  if (is.language(f[[1]])) {
    matrix <- sapply(1:length(x), function(i) do.call(lambda_to_func(f[[i]]), x[i]))
    result <- as.list(as.data.frame(matrix))
    names(result) <- NULL
    return (result)
  }

  matrix <- sapply(1:length(x), function(i) do.call(f[[i]], x[i]))
  result <- as.list(as.data.frame(matrix))
  names(result) <- NULL
  return (result)
}
#' @export
`%invoke_map%` <- invoke_map


#' @export
filter <- function(x, p) {
  stopifnot(is.vector(x))

  if (is.language(p)) {
    return (x[lambda_to_func(p)(x)])
  }

  return (x[p(x)])
}
#' @export
`%filter%` <- filter
#' @export
`%if%` <- filter


#' @export
filter_not <- function(x, p) {
  stopifnot(is.vector(x))

  if (is.language(p)) {
    return (x[!lambda_to_func(p)(x)])
  }

  return (x[!p(x)])
}
#' @export
`%filter_not%` <- filter_not
#' @export
`%if_not%` <- filter_not


#' @export
head_while <- function(x, p) {
  stopifnot(is.vector(x))

  if (length(x) == 0) {
    return (x)
  }

  if (is.language(p)) {
    p <- lambda_to_func(p)
  }

  end <- 0
  for (i in 1:length(x)) {
    if (!p(x[i])) {
      if (end == 0) {
        return (NULL)
      }
      return (x[1:end])
    }
    end <- i
  }

  if (end == 0) {
    return (NULL)
  }
  return (x[1:end])
}
#' @export
`%head_while%` <- head_while


#' @export
tail_while <- function(x, p) {
  stopifnot(is.vector(x))

  if (length(x) == 0) {
    return (x)
  }

  if (is.language(p)) {
    p <- lambda_to_func(p)
  }

  start <- length(x) + 1
  for (i in length(x):1) {
    if (!p(x[i])) {
      if (start > length(x)) {
        return (NULL)
      }
      return (x[start:length(x)])
    }
    start <- i
  }

  if (start > length(x)) {
    return (NULL)
  }
  return (x[start:length(x)])
}
#' @export
`%tail_while%` <- tail_while


#' @export
`%any%` <- function(x, p) {
  return (any(x %map% p))
}
#' @export
`%all%` <- function(x, p) {
  return (all(x %map% p))
}


#' @export
`%has%` <- function(x, e) {
  return (e %in% x)
}


#' @export
flatMap <- function(x, f) {
  stopifnot(is.list(x))

  if (length(x) == 0) {
    return (x)
  }

  if (is.language(f)) {
    f <- lambda_to_func(f)
  }

  return (f(unlist(x)))
}
#' @export
`%flatMap%` <- flatMap


#' @export
identity <- function (x) x


#' @export
flatten <- function(x) { x %flatMap% identity }
