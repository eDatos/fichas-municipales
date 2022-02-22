from io import BytesIO
from PIL import Image
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import os
from fnmatch import fnmatch
from bs4 import BeautifulSoup


def get_html(file):
    html = BeautifulSoup(open(file), features="html.parser")
    return html


def get_web_driver():
    options = Options()
    options.headless = False
    return webdriver.Firefox(executable_path='/Users/luis/Downloads/geckodriver', options=options)


def modify_html(html_path):
    # Modify resource paths
    html = get_html(html_path)
    nodes = html.find_all()
    for node in nodes:
        if node.name == 'script' and node.has_attr('src'):
            if node['src'].startswith('js/'):
                node['src'] = '../../resources/' + node['src']
        if node.name == 'link' and node.has_attr('href'):
            if node['href'].startswith('js/'):
                node['href'] = '../../resources/' + node['href']
        if node.name == 'img' and node.has_attr('src'):
            if node['src'].startswith('/') or node['src'].startswith('C'):
                node['src'] = '../../resources/img/' + node['src'].split("/")[-1]

    with open(html_path, "w") as file:
        file.write(str(html))


def generate_pdf(driver, path, name, html_path):
    pdf_filename = os.path.join(path, os.path.splitext(name)[0] + '.pdf')
    # if not os.path.exists(pdf_filename):

    driver.get('file://' + html_path)
    img = Image.open(BytesIO(driver.find_element_by_tag_name('.main-container').screenshot_as_png))
    img = img.convert('RGB')
    img.save(pdf_filename, "PDF", quality=100)


def walk_directory():
    driver = get_web_driver()
    root = '/Users/luis/Documents/workspace/sta-mun/output/'
    pattern = "GLOSARIO*.html"
    for path, subdirs, files in os.walk(root):
        for name in files:
            if fnmatch(name, pattern):
                html_path = os.path.join(path, name)
                print(os.path.join(path, name))
                modify_html(html_path)
                generate_pdf(driver, path, name, html_path)


if __name__ == '__main__':
    walk_directory()
