## Estimating the number of components in Bayesian settings
library(RSelenium)
library(rvest)

lnk <- 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings'
lnk2 <- 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings/attempt'

driver <- rsDriver(port=4444L,browser="chrome")
rsDriv <- driver$client

rsDriv$open()
rsDriv$getStatus()

rsDriv$navigate(lnk2)
webElem <- rsDriv$findElement(using = 'xpath', value = '//*[@id="email"]')
webElem$sendKeysToElement(list('englianhu@gmail.com', key = 'enter'))










