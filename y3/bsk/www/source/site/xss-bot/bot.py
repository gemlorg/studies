import time

from selenium import webdriver
from selenium.webdriver.common.by import By

task_url = "https://web.kazet.cc:42448/?unread"

options = webdriver.ChromeOptions()
options.add_argument("no-sandbox")
options.add_argument("--headless=new")
options.add_experimental_option("excludeSwitches", ["disable-popup-blocking"])
options.binary_location = "/usr/bin/google-chrome"

while True:
    browser = webdriver.Chrome(options=options)
    try:
        browser.set_page_load_timeout(5)
        browser.implicitly_wait(5)
        browser.get(task_url)
        browser.find_element(By.NAME, "username").send_keys("admin")
        browser.find_element(By.NAME, "password").send_keys("[redacted]")
        browser.find_element(By.NAME, "submit").click()

        browser.find_elements(By.XPATH, "//li/a")[0].click()
        time.sleep(5)

    except Exception as e:
        print(e)
    finally:
        browser.quit()

    time.sleep(5)
