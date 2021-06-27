library(httr)
library(rvest)
library(wdman)

lnk <- 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings'
lnk2 <- 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings/attempt'

r <- httr::GET(lnk2)
status_code(r)
headers(r)
str(content(r))
cookies(r)
content(r, 'text')

## ----------------------------------

sess <- lnk |> 
  rvest::session()
pgform <- html_form(sess)[[1]]


