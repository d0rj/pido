library(stringr)


wordNGramm <- function(input, n, clearData = TRUE) {
  stopifnot(is.character(input))
  stopifnot(is.numeric(n))
  stopifnot(is.logical(clearData))

  n <- n - 1

  if (clearData) {
    input <- str_replace(input, '[^a-zA-Z0-9 ]', ' ')
  }
  tokens <- strsplit(input, split=' ', fixed=TRUE)[[1]]

  stopifnot(length(tokens) > n)

  result <- list()
  index <- 1
  for (i in 1:(length(tokens) - n)) {
    result[[index]] <- tokens[i:(i + n)]
    index <- index + 1
  }

  return (result)
}
