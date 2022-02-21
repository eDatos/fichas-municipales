from io import BytesIO
from PIL import Image
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import os
from fnmatch import fnmatch


def get_web_driver():
    options = Options()
    options.headless = False
    return webdriver.Firefox(executable_path='/Users/luis/Downloads/geckodriver', options=options)


def walk_directory():
    driver = get_web_driver()
    root = '/Users/luis/Documents/workspace/sta-mun/shiny/output'
    pattern = "*.html"
    for path, subdirs, files in os.walk(root):
        for name in files:
            if fnmatch(name, pattern):
                print(os.path.join(path, name))
                driver.get('file://' + os.path.join(path, name))
                img = Image.open(BytesIO(driver.find_element_by_tag_name('.main-container').screenshot_as_png))
                # img.save('test.png', "PNG", quality=100)
                img = img.convert('RGB')
                img.save(os.path.join(path, os.path.splitext(name)[0] + '.pdf'), "PDF", quality=100)


if __name__ == '__main__':
    walk_directory()
