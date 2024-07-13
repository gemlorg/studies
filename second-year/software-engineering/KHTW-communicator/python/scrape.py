import bs4
import httplib2
import re

htlib = httplib2.Http()
status, latex_website_request = htlib.request('https://oeis.org/wiki/List_of_LaTeX_mathematical_symbols')
soup = bs4.BeautifulSoup(latex_website_request, features="html.parser")
tables = soup.find_all("table", {"class": "wikitable"})
print("const formulas_pool = [");
for symbol in soup.find_all('tt'):
    tds = soup.find_all('td')
    code = True
    comment = False
    latex = str()
    com = str()
    for i in range(len(tds)):
        if(comment == True):
            com = str(tds[i])
            break
        if(str(symbol) in str(tds[i])):
            if(code):
                code = False 
                continue
            latex = str(tds[i])
            comment = True 
    if("<td>" in str(com) and not "class=" in str(com) ):
        try:
            com = str(com).split("<td>")[1].split("</td>")[0].replace("\n", "").replace("<i>", "").replace("</i>", "")
            latex = str(latex).split("<tt>")[1].split("</tt>")[0].replace("\\", "\\\\")

            if(not re.search('[a-zA-Z]', com)):
                com = latex.replace("\\", "")
            print("{ title: '" + str(com) + "', formula: '" + str(latex) + "', weight: '1' },")
        except Exception:
            pass
print("]")
