library(stringr)


OR <- function(inp) Reduce("|", inp)


porterStem <- function(word) {
  stopifnot(is.character(word))

  PERFECTIVEGROUND <- "((ив|ивши|ившись|ыв|ывши|ывшись)|((?<=[ая])(в|вши|вшись)))$"
  REFLEXIVE <- "(с[яь])$"
  ADJECTIVE <- "(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|ему|ому|их|ых|ую|юю|ая|яя|ою|ею)$"
  PARTICIPLE <- "((ивш|ывш|ующ)|((?<=[ая])(ем|нн|вш|ющ|щ)))$"
  VERB <- "((ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ен|ило|ыло|ено|ят|ует|уют|ит|ыт|ены|ить|ыть|ишь|ую|ю)|((?<=[ая])(ла|на|ете|йте|ли|й|л|ем|н|ло|но|ет|ют|ны|ть|ешь|нно)))$"
  NOUN <- "(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я)$"
  RVRE <- "^(.*?[аеиоуыэюя])(.*)$"
  DERIVATIONAL <- ".*[^аеиоуыэюя]+[аеиоуыэюя].*ость?$"
  DER <- "ость?$"
  SUPERLATIVE <- "(ейше|ейш)$"
  I <- "и$"
  P <- "ь$"
  NN <- "нн$"

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
