library(stringr)


nGramm <- function(tokens, n) {
  stopifnot(is.vector(tokens))
  stopifnot(is.numeric(n))
  n <- n - 1
  stopifnot(length(tokens) > n)

  result <- list()
  index <- 1
  for (i in 1:(length(tokens) - n)) {
    result[[index]] <- tokens[i:(i + n)]
    index <- index + 1
  }

  return (result)
}


wordNGramm <- function(input, n, clearData = TRUE) {
  stopifnot(is.character(input))
  stopifnot(is.numeric(n))
  stopifnot(is.logical(clearData))

  if (clearData) {
    input <- str_replace(input, '[^a-zA-Z0-9 ]', ' ')
  }
  tokens <- strsplit(input, split=' ', fixed=TRUE)[[1]]

  return (nGramm(tokens, n))
}


symbolNGramm <- function(input, n) {
  stopifnot(is.character(input))
  stopifnot(is.numeric(n))

  tokens <- strsplit(input, split='', fixed=TRUE)[[1]]

  return (nGramm(tokens, n))
}
