from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
import re
import pandas as pd
from bs4 import BeautifulSoup

#options = Options()
#options.add_argument("start-maximized")

service = Service(executable_path="/snap/bin/chromium.chromedriver")

service.start()

driver = webdriver.Remote(service.service_url)

url = "https://sports.bwin.com/en/sports/football-4/betting/europe-7/euro-2024-0:17"
driver.get(url)

try:
    element = WebDriverWait(driver, 30).until(
        EC.presence_of_element_located((By.XPATH, "/html/body/vn-app/vn-dynamic-layout-slot[5]/vn-main/main/div/ms-main/div[1]/ng-scrollbar[2]/div/div/div/div/ms-main-column/div/ms-widget-layout/ms-widget-slot/ms-composable-widget/ms-widget-slot[2]/ms-tabbed-grid-widget/ms-grid/div/ms-event-group[1]/ms-event/div/a/ms-event-detail/div")) # set this to title of any of the matches in the sidebar 
    )
    soup = BeautifulSoup(driver.page_source, 'html.parser')
finally:
    driver.quit()

containers = soup.findAll('div', {'class':'grid-event-wrapper'})

resultAndOdds = []

for match in containers:
    row = []
    for team in match.findAll('div', {'class':'participant'}):
        row.append(team.text)
    
    for odds in match.findAll('div', {'class': 'option-indicator'}):
        row.append(odds.find('ms-font-resizer').text)
    #divs = container.findAll('div', {'class': 'grid-event-wrapper'})
    #texts = [div.text for div in divs]
    #it = iter(texts)
    #resultAndOdds.append(list(zip(it, it)))
    if(len(row) > 0 ):
        resultAndOdds.append(row)




df = pd.DataFrame(resultAndOdds)
df = df.iloc[:, 0:5] 
df.dropna(how='all', axis=1, inplace=True)
df.columns = ['home', 'away', 'home.odds', 'tie.odds', 'away.odds']
df.to_csv('bwin.csv', index = False)
