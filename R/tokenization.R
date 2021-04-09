#' @export
word_tokenizer = function(input) {
  stopifnot(is.character(input))

  result = stringi::stri_split_boundaries(input, type = "word", skip_word_none = TRUE)[[1]]

  return (result)
}


#' @export
char_tokenizer = function(input) {
  stopifnot(is.character(input))

  result = stringi::stri_split_boundaries(input, type = "character")[[1]]

  return (result)
}


#' @export
space_tokenizer = function(input, sep = " ") {
  stopifnot(nchar(sep) == 1)
  stopifnot(is.character(input))

  return (stringi::stri_split_fixed(input, pattern = sep))
}
