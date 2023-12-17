import requests
import re
import json
import time

def page(url):
    '''Fetches webpage content using the requests library.'''
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20100101 Firefox/4.0.1'
    }  # Simulate browser access using Firefox 4.0.1
    response = requests.get(url, headers=headers)
    return response.text

def next_page(html):
    '''Parses the HTML using regular expressions.'''
    # Create a regular expression object to optimize code
    pattern = re.compile(
        '<dd>.*?board-index.*?>(\d+)</i>.*?data-src="(.*?)".*?name"><a'
        + '.*?>(.*?)</a>.*?star">(.*?)</p>.*?releasetime">(.*?)</p>'
        + '.*?integer">(.*?)</i>.*?fraction">(.*?)</i>.*?</dd>', re.S)
    contents = re.findall(pattern, html)
    for content in contents:
        # Use a generator function for efficiency
        yield {
            'index': content[0],
            'image': content[1],
            'title': content[2],
            'actor': content[3].strip()[3:],  # Remove the prefix from the actor string
            'time': content[4].strip()[5:],  # Remove the prefix from the time string
            'score': content[5] + content[6]
        }

def to_file(content):
    '''Writes data to a file.'''
    with open('maoyan.txt', 'a', encoding='utf-8') as f:
        # Serialize the dictionary into a JSON string using json.dumps, set ensure_ascii=False to support Chinese characters
        f.write(json.dumps(content, ensure_ascii=False) + '\n')

def main(offset):
    '''Main function to scrape data from the first 100 (10 pages) webpages.'''
    url = 'http://maoyan.com/board/4?offset=' + str(offset)
    html = page(url)
    for content in next_page(html):
        print(content)
        to_file(content)

if __name__ == '__main__':
    for i in range(10):
        # Implement pagination
        main(offset=i * 10)
        time.sleep(1)  # Pause for 1 second between requests to be polite to the server
