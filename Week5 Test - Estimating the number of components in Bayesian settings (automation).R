## Estimating the number of components in Bayesian settings
## https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
library(RSelenium)
library(rvest)
library(wdman)

lnk <- 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings'
lnk2 <- 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings/attempt'

#rD <- rsDriver(browser='chrome', check=FALSE)
#rsDeriv <- rD[['client']]
#rsDeriv$navigate('http://www.google.com/ncr')
#rsDeriv$navigate('http://www.bbc.com')
#rsDeriv$close()

#driver <- rsDriver(remoteServerAddr='localhost', 
#                   port=4445L, browser=c('chrome'), 
#                   version = 'latest')
## https://cran.r-project.org/web/packages/RSelenium/vignettes/headless.html
Sys.which('phantomjs')
#phantomjs 
#"/usr/local/bin/phantomjs"

driver <- rsDriver(port=4444L, browser='chrome')
#driver <- rsDriver(port=4837L, browser='chrome', check=FALSE)
rsDriv <- driver$client
#driver[['server']]$stop()

rsDriv$open()
rsDriv$getStatus()

rsDriv$navigate(lnk2)
webElem <- rsDriv$findElement(using = 'xpath', value = '//*[@id="email"]')
webElem$sendKeysToElement(list('englianhu@gmail.com', key = 'enter'))


rsDriv$close()
driver$server$stop()






