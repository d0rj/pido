library(testthat)


context('Porter stemmer for russian words')


test_that('Simple test', {
  expect_equal(porterStem.ru('академия'), 'академ')
  expect_equal(porterStem.ru('русский'), 'русск')
  expect_equal(porterStem.ru('область'), 'област')
  expect_equal(porterStem.ru('человеческий'), 'человеческ')
  expect_equal(porterStem.ru('гибридной'), 'гибридн')
  expect_equal(porterStem.ru('благополучие'), 'благополуч')
})

test_that('Errors check', {
  expect_equal(porterStem.ru(''), '')
  expect_equal(porterStem.ru('Somebody'), '')

  expect_error(porterStem.ru(1))
})
