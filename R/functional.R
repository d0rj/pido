

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

  vars_string <- ""
  if (length(vars) == 0) {
    vars_string <- ""
  }
  else if (length(vars) == 1) {
    vars_string <- as.character(vars)
  }
  else {
    for (var in vars) {
      vars_string <- paste(vars_string, as.character(var), sep=",")
    }
    vars_string <- sub(".", "", vars_string)
  }

  return (eval(parse(text=paste("(function(", vars_string, ") {", expr_str_splitted[length(expr_str_splitted)], "})"))))
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
functor <- function(init=c(1)) {
  return (function(change=NULL) {
    if (is.null(change)) { return (init) }
    else { return (functor(change(init))) }
  })
}
