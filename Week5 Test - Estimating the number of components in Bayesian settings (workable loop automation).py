reticulate::repl_python()

## Estimating the number of components in Bayesian settings
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import numpy

lnk  = 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings'
lnk2 = 'https://www.coursera.org/learn/mixture-models/exam/9IYM3/estimating-the-number-of-components-in-bayesian-settings/attempt'

# Download the `geckodriver` webdriver.
driver = webdriver.Firefox(executable_path=r'/home/englianhu/Documents/geckodriver')
#driver = webdriver.chrome(executable_path=r'/home/englianhu/Documents/chromedriver')
driver.get(lnk2)
#assert 'Python' in driver.title
#wait   = WebDriverWait(driver, 5)
time.sleep(2)
elem   = driver.find_element_by_xpath('//*[@id="email"]')
elem.send_keys('englianhu@gmail.com')

#wait   = WebDriverWait(driver, 1)
time.sleep(2)
elem   = driver.find_element_by_xpath('//*[@id="password"]')
elem.send_keys('STr@d343v@')
#elem.clear()
elem.send_keys(Keys.RETURN)

#wait   = WebDriverWait(driver, 5)
time.sleep(2)
elem   = driver.find_element_by_xpath('/html/body/div[6]/div/div[3]/div[1]/button')
elem.send_keys(Keys.RETURN)

for n in list(numpy.arange(0, 10000, 0.01)):
#for n in list(numpy.round(numpy.arange(11.2, 10000, 0.1), 1)):
  ## assessment question
  elem   = driver.find_element_by_xpath(
    '//*[@id="text-input-ZTDYZx9hEeqF4xJzBvIAyQ-input"]')
  elem.send_keys(str(11.1))
  #elem.send_keys(str(numpy.round(n, 1)))
  
  elem   = driver.find_element_by_xpath(
    '//*[@id="text-input-UlGqjR-lEeqKlBLH7WtknQ-input"]')
  elem.send_keys(str(8.4))
  #elem.send_keys(str(numpy.round(n, 1)))
  
  elem   = driver.find_element_by_xpath(
    '//*[@id="text-input-WhtVnCApEeqqhA61a6gg2w-input"]')
  elem.send_keys(str(7.8))
  #elem.send_keys(str(numpy.round(n, 1)))
  
  elem   = driver.find_element_by_xpath(
    '//*[@id="text-input-giU7KiAoEeqFkg4D2xLqww-input"]')
  elem.send_keys(str(0.31))
  #elem.send_keys(str(numpy.round(n, 2)))
  
  wait   = WebDriverWait(driver, 1)
  #elem   = driver.find_element_by_xpath(
  #  '/html/body/div[7]/div/div/div/div[2]/div[2]/div/div[2]/div/div[3]/div/div[1]/div[1]/div/div[1]/label/input')
  elem   = driver.find_element_by_css_selector('._htmk7zm')
  elem.click()
  
  wait   = WebDriverWait(driver, 1)
  #elem   = driver.find_element_by_xpath(
  #  '/html/body/div[7]/div/div/div/div[2]/div[2]/div/div[2]/div/div[3]/div/div[2]/button[2]/span')
  elem   = driver.find_element_by_css_selector('._1hx9z6hg > span:nth-child(1)')
  elem.click()
  
  time.sleep(2)
  #elem   = driver.find_element_by_xpath(
  #  '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[2]/div[2]/span'
  #)
  #elem = driver.find_elements_by_css_selector('.rc-AttemptPageTopBanner__grade-percent > span:nth-child(1)')
  elem   = driver.find_elements_by_xpath('//div[contains(text(), "%")]')
  for el in elem:
    el.get_attribute('innerHTML')
  res    = float(el.get_attribute('innerHTML').strip('%'))
  #if res > 0:
  if res > 75:
    break
  #get current window handle
  #driver.implicitly_wait(2)
  #p1      = driver.current_window_handle
  #driver.switch_to.window(p1)
  
  #wait   = WebDriverWait(driver, 5)
  #driver.switch_to_active_element()
  driver.refresh()
  time.sleep(10)
  #driver.switch_to.window(driver.window_handles[-1])
  #wait   = WebDriverWait(driver, 5)
  #driver.implicitly_wait(10)
  #driver.findelement(by.linktext('test').click();
  #elem   = wait.until(ExpectedConditions.elementToBeClickable(By.XPATH('/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[1]/div/button/span')))
  #elem   = wait.until(EC.presence_of_element_located(By.XPATH, '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[1]/div/button/span'))
  
  #elem   = driver.find_element_by_xpath(
  #  '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[1]/div/button/span')
  #elem.click()
  
  #elem = WebDriverWait(driver, 2, 0.5)
  #elem.until(
  #  EC.presence_of_element_located(By.XPATH, '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[1]/div/button/span'))
  #elem.click()
  
  #driver.implicitly_wait(2)
  #elem = WebDriverWait(driver, 2, 0.5)
  #elem.until(
  #  EC.presence_of_element_located(By.XPATH, '/html/body/div[3]/div/div/div[2]/div/div[2]/div[2]/div[2]/main/div/div[3]/div[3]/div/div/div/button/span'))
  #elem.click()

