# PIDO.R package

## Overview

🛠Utils package. Contains:

* Functional analogues of **dplyr**'s pipe operator `%>%`, such as `%map%` etc. Supports default R types, for example **dataframe**
* Own syntax of λambda functions for this pipe operators (and pipe operators also support default R functions `function(...) {...}`)
* Porter stemmer for 🇬🇧 english and 🇷🇺 Russian languages
* Text tokenizers by sentences, words, n-gramms

## Install

```R
library(devtools)

install_github('d0rj/pido')

library(pido)
```

## Examples

### Lambda functions

#### Syntax variation

```R
1:4 %map% (num ~ num^2) # first variant
1:4 %map% (~ num^2) # another variant
1:4 %map% (~ .^2) # third
```
is equivalent of 

```R
1:4 %>% map(~ x^2) # in dplyr

sapply(1:4, function(x) x^2) # or without packages
```

#### Multiple arguments

```R
list(c(1, 2), c(2, 3)) %map% (first ~ second ~ first * second) # first
list(c(1, 2), c(2, 3)) %map% (~ x * y) # and second variant
```

is equivalent of

```R
sapply(list(c(1, 2), c(2, 3)), function(x, y) x * y)
```

#### Multi-line lambda

```R
list(c(1, 2), c(2, 3)) %map% (~ {
    dbl <- x * 2
    trpl <- y * 3
    return (dbl + trpl)
})
```

this code will work correctly and will be equivalent of

```R
sapply(list(c(1, 2), c(2, 3)), function(x, y) {
    dbl <- x * 2
    trpl <- y * 3
    return (dbl + trpl)
})
```

## Notes

- [x] Distribution-ready
- [ ] More tests will be added
- [ ] CRAN package

PIDO abbreviation stands for [it](https://www.youtube.com/watch?v=yJ6J0yeqt7Q&ab_channel=ExtremeCode).
