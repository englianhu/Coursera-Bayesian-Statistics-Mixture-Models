
## assessment question
elem   = driver.find_element_by_xpath(
  '//*[@id="text-input-ZTDYZx9hEeqF4xJzBvIAyQ-input"]')
elem.send_keys(str(n))

elem   = driver.find_element_by_xpath(
  '//*[@id="text-input-UlGqjR-lEeqKlBLH7WtknQ-input"]')
elem.send_keys(str(n))

elem   = driver.find_element_by_xpath(
  '//*[@id="text-input-WhtVnCApEeqqhA61a6gg2w-input"]')
elem.send_keys(str(n))

elem   = driver.find_element_by_xpath(
  '//*[@id="text-input-giU7KiAoEeqFkg4D2xLqww-input"]')
elem.send_keys(str(n))

wait   = WebDriverWait(driver, 1)
elem   = driver.find_element_by_xpath(
  '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[2]/div/div[3]/div/div[1]/div[1]/div/div[1]/label/input')
elem.click()

wait   = WebDriverWait(driver, 1)
elem   = driver.find_element_by_xpath(
  '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[2]/div/div[3]/div/div[2]/button[2]/span')
elem.click()
  
#get current window handle
p1      = driver.current_window_handle
driver.switch_to.window(p1)
elem   = driver.find_element_by_xpath(
  '/html/body/div[6]/div/div/div/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[1]/div/button/span')
elem.click()

time.sleep(8)
elem   = driver.find_element_by_xpath(
  '/html/body/div[3]/div/div/div[2]/div/div[2]/div[2]/div[2]/main/div/div[3]/div[3]/div/div/div/button/span')
elem.click()

