library(stringr)


OR <- function(inp) Reduce('|', inp)


#' Returns stemmed word
#'
#' @name PorterStemmer.ru
#'
#' @param word Input word to be stemmed
#' @return Stemmed word
#' @export
#' @examples
#' porterStem.ru('прилежность')
porterStem.ru <- function(word) {
  stopifnot(is.character(word))

  PERFECTIVEGROUND <- '((ив|ивши|ившись|ыв|ывши|ывшись)|((?<=[ая])(в|вши|вшись)))$'
  REFLEXIVE <- '(с[яь])$'
  ADJECTIVE <- '(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|ему|ому|их|ых|ую|юю|ая|яя|ою|ею)$'
  PARTICIPLE <- '((ивш|ывш|ующ)|((?<=[ая])(ем|нн|вш|ющ|щ)))$'
  VERB <- '((ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ен|ило|ыло|ено|ят|ует|уют|ит|ыт|ены|ить|ыть|ишь|ую|ю)|((?<=[ая])(ла|на|ете|йте|ли|й|л|ем|н|ло|но|ет|ют|ны|ть|ешь|нно)))$'
  NOUN <- '(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я)$'
  RVRE <- '^(.*?[аеиоуыэюя])(.*)$'
  DERIVATIONAL <- '.*[^аеиоуыэюя]+[аеиоуыэюя].*ость?$'
  DER <- 'ость?$'
  SUPERLATIVE <- '(ейше|ейш)$'
  I <- 'и$'
  P <- 'ь$'
  NN <- 'нн$'

  word <- tolower(word)
  word <- gsub('ё', 'е', word)

  m <- str_match(word, RVRE)

  if (OR(is.na(m))) {
    return ('')
  }

  pre <- m[2]
  rv <- m[3]

  temp <- str_replace(rv, PERFECTIVEGROUND, '')
  if (temp == rv) {
    rv <- str_replace(rv, REFLEXIVE, '')
    temp <- str_replace(rv, ADJECTIVE, '')
    if (temp != rv) {
      rv <- temp
      rv <- str_replace(rv, PARTICIPLE, '')
    }
    else {
      temp <- str_replace(rv, VERB, '')
      if (temp == rv) {
        rv <- str_replace(rv, NOUN, '')
      }
      else {
        rv <- temp
      }
    }
  }
  else {
    rv <- temp
  }

  rv <- str_replace(rv, I, '')

  if (OR(!is.na(str_match(rv, DERIVATIONAL)))) {
    rv <- str_replace(rv, DER, '')
  }

  temp <- str_replace(rv, P, '')

  if (temp == rv) {
    rv <- str_replace(rv, SUPERLATIVE, '')
    rv <- str_replace(rv, NN, '')
  }
  else {
    rv <- temp
  }

  word <- paste(pre, rv, sep='')
  return (word)
}


splitstring <- function(x) { strsplit(x, NULL)[[1]] }


# function that returns a logical vector of TRUE/FALSE depending on vowel/consonant
# for instance, for the input 'cry', it returns 'FALSE' 'FALSE' 'TRUE'
h1 <- function(x) {
  stopifnot(is.character(x))

  l <- str_detect('aeiou', splitstring(x))
  k <- length(splitstring(x))
  z <- splitstring(x)[1:k]
  i <- 2;

  while (k > 1 && i <= k) {
    if (splitstring(x)[i] == 'y' && !str_detect(z[i - 1], '[aeiou]')) {
      l[i] <- TRUE
    }
    i <- i + 1;
  }

  return (l)
}


# function for returning the interger value of m
m <- function(x1, y1='') {
  stopifnot(is.character(x1))
  stopifnot(is.character(y1))

  k <- (length(splitstring(x1)) - length(splitstring(y1)))
  x <- paste0(splitstring(x1)[1:k], collapse='')
  count <- 0;

  y <- h1(x);

  for(i in 1:length(y)){
    if((i + 1) <= length(y) && y[i] && !y[i + 1]){
      count <- count + 1;
    }
  }

  return (count)
}


# function returns TRUE if and only if there is a vowel in the word left after you remove suffix y1 from the word x1
v <- function(x1, y1='') {
  stopifnot(is.character(x1))
  stopifnot(is.character(y1))

  check <- FALSE
  k <- (length(splitstring(x1)) - length(splitstring(y1)))
  w <- paste0(splitstring(x1)[1:k], collapse='')
  z <- splitstring(w)[1:k]
  i <- 2;

  while (k > 1 && i <= k) {
    if (z[i] == 'y' && !str_detect(z[i - 1], '[aeiou]')) {
        check <- TRUE
    }
    i = i + 1;
  }

  if (str_detect(w, '[aeiou]'))
    check <- TRUE

  return (check)
}


# function returns TRUE if and only if there is a double consonant at the end of the word left after you remove suffix y1 from the word x1
d <- function(x1, y1='') {
  stopifnot(is.character(x1))
  stopifnot(is.character(y1))

  check <- FALSE

  k <- (length(splitstring(x1)) - length(splitstring(y1)))
  if(k > 1) {
    if(splitstring(x1)[k] == splitstring(x1)[k - 1] && !v(splitstring(x1)[k]))
      check <- TRUE
  }

  return (check)
}


# function returns TRUE if and only if there is a sequence of cvc at the end of the word left after you remove suffix y1 from the word x1
# except when the second consonant is w, x or y
cvc <- function(x1, y1='') {
  stopifnot(is.character(x1))
  stopifnot(is.character(y1))

  check <- FALSE
  k <- (length(splitstring(x1)) - length(splitstring(y1)))
  if (k >= 3) {
    c2 <- splitstring(x1)[k]
    v <- splitstring(x1)[k - 1]
    c1 <- splitstring(x1)[k - 2]

    t1 <- str_detect(c1, '[aeiou]')
    t2 <- str_detect(v, '[aeiouy]')
    t3 <- str_detect(c2, '[aeiou]')
    t4 <- str_detect(c2, '[wxy]')

    if (!t1 && t2 && !t3 && !t4)
      check <- TRUE
  }

  return (check)
}


#' Returns stemmed word
#'
#' @name PorterStemmer.en
#'
#' @param x Input word to be stemmed
#' @return Stemmed word
#' @export
#' @examples
#' porterStem.en('actually')
porterStem.en <- function(x) {
  stopifnot(is.character(x))

  if (length(splitstring(x)) > 2) {
    if (str_detect(x, 's$')) {
      if (str_detect(x, 'sses$')) {
        x <- str_replace(x,'sses$', 'ss')
      }
      else if (str_detect(x, 'ies$')) {
        x <- str_replace(x, 'ies$', 'i')
      }
      else if (str_detect(x, 'ss$')) {
        x <- str_replace(x, 'ss$', 'ss')
      }
      else if (str_detect(x, 's$')) {
        x <- str_replace(x, 's$', '')
      }
    }

  # Step 1b)
  flag <- 0
  if (str_detect(x, 'eed$')) {
    if (m(x, 'eed') > 0)
      x <- str_replace(x, 'eed$', 'ee')
  }
  else if (str_detect(x, 'ed$')) {
    if (v(x, 'ed')) {
      x <- str_replace(x, 'ed$', '')
      flag <- 1
    }
  }
  else if (str_detect(x, 'ing$')) {
    if (v(x, 'ing')) {
      x <- str_replace(x, 'ing$', '')
      flag <- 1
    }
  }

  if (flag == 1) {
    if (str_detect(x, 'at$')){
      x <- str_replace(x, 'at$', 'ate')
    }
    else if (str_detect(x, 'bl$')) {
      x <- str_replace(x, 'bl$', 'ble')
    }
    else if (str_detect(x, 'iz$')) {
      x <- str_replace(x, 'iz$', 'ize')
    }
    else if (!str_detect(x, '[lsz]$') && d(x)) {
      x <- paste0(splitstring(x)[1:(length(splitstring(x)) - 1)], collapse='')
    }
    else if (m(x) == 1 && cvc(x)) {
      x <- paste0(x, 'e', collapse='')
    }
  }

  # Step 1c)
  if (str_detect(x, 'y$')) {
    if (v(x, 'y'))
      x <- str_replace(x, 'y$', 'i')
  }


  # Step 2
  if (str_detect(x, 'ational$')) {
    if (m(x, 'ational') > 0)
      x <- str_replace(x, 'ational$', 'ate')
  }
  else if (str_detect(x, 'tional$')) {
    if (m(x, 'tional') > 0)
      x <- str_replace(x, 'tional$', 'tion')
  }
  else if (str_detect(x, 'enci$')) {
    if (m(x, 'enci') > 0)
      x <- str_replace(x, 'enci$', 'ence')
  }
  else if (str_detect(x, 'anci$')) {
    if (m(x, 'anci') > 0)
      x <- str_replace(x, 'anci$', 'ance')
  }
  else if (str_detect(x, 'izer$')) {
    if (m(x, 'izer') > 0)
      x <- str_replace(x, 'izer$', 'ize')
  }
  else if (str_detect(x, 'logi$')) {
    if (m(x, 'logi') > 0)
      x <- str_replace(x, 'logi$', 'log')
  }
  else if (str_detect(x, 'bli$')) {
    if (m(x, 'bli') > 0)
      x <- str_replace(x, 'bli$', 'ble')
  }
  else if (str_detect(x, 'alli$')) {
    if (m(x, 'alli') > 0)
      x <- str_replace(x, 'alli$', 'al')
  }
  else if (str_detect(x, 'entli$')) {
    if (m(x, 'entli') > 0)
      x <- str_replace(x, 'entli$', 'ent')
  }
  else if (str_detect(x, 'eli$')) {
    if (m(x, 'eli') > 0)
      x <- str_replace(x, 'eli$', 'e')
  }
  else if (str_detect(x, 'ousli$')) {
    if (m(x, 'ousli') > 0)
      x <- str_replace(x, 'ousli$', 'ous')
  }
  else if (str_detect(x, 'ization$')) {
    if (m(x, 'ization') > 0)
      x <- str_replace(x, 'ization$', 'ize')
  }
  else if (str_detect(x, 'ation$')) {
    if (m(x, 'ation') > 0)
      x <- str_replace(x, 'ation$', 'ate')
  }
  else if (str_detect(x, 'ator$')) {
    if (m(x, 'ator') > 0)
      x <- str_replace(x, 'ator$', 'ate')
  }
  else if (str_detect(x, 'alism$')) {
    if (m(x, 'alism') > 0)
      x <- str_replace(x, 'alism$', 'al')
  }
  else if (str_detect(x, 'iveness$')) {
    if (m(x, 'iveness') > 0)
      x <- str_replace(x, 'iveness$', 'ive')
  }
  else if (str_detect(x, 'fulness$')) {
    if (m(x, 'fulness') > 0)
      x <- str_replace(x, 'fulness$', 'ful')
  }
  else if (str_detect(x, 'ousness$')) {
    if (m(x, 'ousness') > 0)
      x <- str_replace(x, 'ousness$', 'ous')
  }
  else if (str_detect(x, 'aliti$')) {
    if (m(x, 'aliti') > 0)
      x <- str_replace(x, 'aliti$', 'al')
  }
  else if (str_detect(x, 'iviti$')) {
    if (m(x, 'iviti') > 0)
      x <- str_replace(x, 'iviti$', 'ive')
  }
  else if (str_detect(x, 'biliti$')) {
    if (m(x, 'biliti') > 0)
      x <- str_replace(x, 'biliti$', 'ble')
  }

  # Step 3
  if (str_detect(x, 'icate$')) {
    if (m(x, 'icate') > 0)
      x <- str_replace(x, 'icate$', 'ic')
  }
  else if (str_detect(x, 'ative$')) {
    if (m(x, 'ative') > 0)
      x <- str_replace(x, 'ative$', '')
  }
  else if (str_detect(x, 'alize$')) {
    if (m(x, 'alize') > 0)
      x <- str_replace(x, 'alize$', 'al')
  }
  else if (str_detect(x, 'iciti$')) {
    if (m(x, 'iciti') > 0)
      x <- str_replace(x, 'iciti$', 'ic')
  }
  else if (str_detect(x, 'ical$')) {
    if (m(x, 'ical') > 0)
      x <- str_replace(x, 'ical$', 'ic')
  }
  else if (str_detect(x, 'ful$')) {
    if (m(x, 'ful') > 0)
      x <- str_replace(x, 'ful$', '')
  }
  else if (str_detect(x, 'ness$')) {
    if (m(x, 'ness') > 0)
      x <- str_replace(x, 'ness$', '')
  }

  # Step 4
  if (str_detect(x, 'al$')) {
    if (m(x, 'al') > 1)
      x <- str_replace(x, 'al$', '')
  }
  else if (str_detect(x, 'ance$')) {
    if (m(x, 'ance') > 1)
      x <- str_replace(x, 'ance$', '')
  }
  else if (str_detect(x, 'ence$')) {
    if (m(x, 'ence') > 1)
      x <- str_replace(x, 'ence$', '')
  }
  else if (str_detect(x, 'er$')) {
    if (m(x, 'er') > 1)
      x <- str_replace(x, 'er$', '')
  }
  else if (str_detect(x, 'ic$')) {
    if (m(x, 'ic') > 1)
      x <- str_replace(x, 'ic$', '')
  }
  else if (str_detect(x, 'able$')) {
    if (m(x, 'able') > 1)
      x <- str_replace(x, 'able$', '')
  }
  else if (str_detect(x, 'ible$')) {
    if (m(x, 'ible') > 1)
      x <- str_replace(x, 'ible$', '')
  }
  else if (str_detect(x, 'ant$')) {
    if (m(x, 'ant') > 1)
      x <- str_replace(x, 'ant$', '')
  }
  else if (str_detect(x, 'ement$')) {
    if (m(x, 'ement') > 1)
      x <- str_replace(x, 'ement$', '')
  }
  else if (str_detect(x, 'ment$')) {
    if (m(x, 'ment') > 1)
      x <- str_replace(x, 'ment$', '')
  }
  else if (str_detect(x, 'ent$')) {
    if (m(x, 'ent') > 1)
      x <- str_replace(x, 'ent$', '')
  }
  else if (str_detect(x, 'sion$') || str_detect(x, 'tion$')) {
    if (m(x, 'ion') > 1)
      x <- str_replace(x, 'ion$', '')
  }
  else if (str_detect(x, 'ou$')) {
    if (m(x, 'ou') > 1)
      x <- str_replace(x, 'ou$', '')
  }
  else if (str_detect(x, 'ism$')) {
    if (m(x, 'ism') > 1)
      x <- str_replace(x, 'ism$', '')
  }
  else if (str_detect(x, 'ate$')) {
    if (m(x, 'ate') > 1)
      x <- str_replace(x, 'ate$', '')
  }
  else if (str_detect(x, 'iti$')) {
    if (m(x, 'iti') > 1)
      x <- str_replace(x, 'iti$', '')
  }
  else if (str_detect(x, 'ous$')) {
    if (m(x, 'ous') > 1)
      x <- str_replace(x, 'ous$', '')
  }
  else if (str_detect(x, 'ive$')) {
    if (m(x, 'ive') > 1)
      x <- str_replace(x, 'ive$', '')
  }
  else if (str_detect(x, 'ize$')) {
    if (m(x, 'ize') > 1)
      x <- str_replace(x, 'ize$', '')
  }

  # Step 5a)
  if (str_detect(x, 'e$')) {
    if (m(x, 'e') > 1) {
      x <- str_replace(x, 'e$', '')
    }
    else if (m(x, 'e') == 1 && !cvc(x, 'e')) {
      x <- str_replace(x, 'e$', '')
    }
  }

  # Step 5b)
  if (m(x) > 1 && str_detect(x, 'll$')) {
    x <- str_replace(x, 'l$', '')
  }
  }

  return (x)
}
