from io import BytesIO
from PIL import Image
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import os
from fnmatch import fnmatch
from bs4 import BeautifulSoup
import shutil


def get_html(file):
    html = BeautifulSoup(open(file), features="html.parser")
    return html


def get_web_driver():
    options = Options()
    options.headless = False
    return webdriver.Firefox(executable_path='/Users/luis/Downloads/geckodriver', options=options)


def modify_html(html_path, name, path):
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
            name = name.replace(".html", "")
            if node['src'].startswith('/') or node['src'].startswith('C'):
                node['src'] = '../../resources/img/' + node['src'].split("/")[-1]
            elif node['src'].startswith(name):
                old_src = node['src']
                chunk_name = node['src'].split("/")[-1]
                id_municipio = name.split("_")[-1]
                new_chunk_name = chunk_name.replace('unnamed-chunk', id_municipio)
                new_chunk_name = new_chunk_name.replace('1-1.', 'canarias.')
                new_chunk_name = new_chunk_name.replace('2-1.', 'isla.')
                new_chunk_name = new_chunk_name.replace('3-1.', 'comarca.')
                node['src'] = '../../resources/img/maps/' + new_chunk_name
                old_img = path + "/" + old_src
                new_img = path + "/" + node['src']
                if not os.path.exists(new_img) and os.path.exists(old_img):
                    os.rename(old_img, new_img)

                # old_dir = path + '/' + old_src.split('/')[0]

    with open(html_path, "w") as file:
        file.write(str(html))


def generate_pdf(driver, path, name, html_path):
    pdf_filename = os.path.join(path, os.path.splitext(name)[0] + '.pdf')
    # if not os.path.exists(pdf_filename):

    driver.get('file://' + html_path)
    img = Image.open(BytesIO(driver.find_element_by_tag_name('.main-container').screenshot_as_png))
    img = img.convert('RGB')
    img.save(pdf_filename, "PDF", quality=100)


def delete_chunks_directory(path, name):
    directory = os.path.join(path, os.path.splitext(name)[0]) + "_files"
    if os.path.exists(directory):
        shutil.rmtree(directory)


def delete_duplicated_js(path):
    if "js" in path and not ("resources" in path):
        shutil.rmtree(path)


def walk_directory():
    driver = get_web_driver()
    driver.set_window_size(1200, 1200)
    root = '/Users/luis/Documents/workspace/sta-mun/output/sector_primario'
    pattern = "*.html"
    for path, subdirs, files in os.walk(root):
        for name in files:
            if fnmatch(name, pattern):
                html_path = os.path.join(path, name)
                print(os.path.join(path, name))
                modify_html(html_path, name, path)
                delete_chunks_directory(path, name)
                generate_pdf(driver, path, name, html_path)

        delete_duplicated_js(path)


if __name__ == '__main__':
    walk_directory()
