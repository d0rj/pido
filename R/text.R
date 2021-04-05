rep_gram <- function(text, n) {
  r <- stringdist::qgrams(text, q=n)
  g <- unlist(sapply(1:length(r), function(x) rep(colnames(r)[x], r[x])))

  return (g)
}


rep_grams <- function(text, ngmin=1, ngmax=2) {
  g <- unlist(sapply(ngmin:ngmax, function(x) rep_gram(text, x)))

  return (g)
}


ngram_tokenize <- function(x, char=FALSE, ngmin=1, ngmax=3) {
  stopifnot(ngmin <= ngmax)
  stopifnot(is.logical(char))

  y <- paste(x, collapse=" ")
  if (char) {
    return (rep_grams(y, ngmin = ngmin, ngmax = ngmax))
  }
  else {
    return (ngramrr::ngramrr(x, char=char, ngmin=ngmin, ngmax=ngmax))
  }
}
