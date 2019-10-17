library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)

#open selenium session and authenticate
rD <- rsDriver(browser=c("firefox"))
driver <- rD$client
driver$open()
driver$navigate("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=MID.MID")  
webElem <- driver$findElement(using = "css selector", value = "p input")
webElem$sendKeysToElement(list("castro200kgd")) #authenticate
webElem <- driver$findElement(using = "css selector", value = "input.inputbutton")
webElem$clickElement()


#loop to grab MIDS urls for later scraping
counter <- (1:1484)
df <- list()
for(i in counter){
links <- driver$findElement(using = "xpath", value = paste("//*[@id='wikitext']/ul/li[",i,"]/a", sep= ""))
temp <- unlist(lapply(links, function(x){links$getElementAttribute("href")}))
temp <- unique(temp)
print(temp)
df <- rbind(df, temp) 
}


temp_df <- c("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=MID.MID-0051",
             "http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=MID.MID-0007")

#filter out weird urls containing 'edit' in them
df_1 <- Filter(function(x) !any(grepl("edit", x)), df)

# 
final_df <- list()
for(i in df_1){
driver$navigate(i)
page <- read_html(driver$getPageSource()[[1]]) %>% html_text() 
page <- gsub(".* ARTICLE TITLE AND DATE HERE"," ", page) 
page <- gsub("Page last modified.*", " ", page)
page <- gsub("[\r\n\t]", "", page)
page <- gsub("\\(Edit Section ↓)", "", page)
##start extracting the fields for each MID
mid_ID <- as.numeric(gsub(".*Dispute InformationMID #: |MID Name:.* ", "", page))
url <- paste0(i)
mid_name <- gsub(".*MID Name: |Start Date:.*", "", page)
start_date <- gsub(".*Start Date:|End Date:.*", "", page)
end_date <- gsub(".*End Date: |Initiators?: .*", "", page)
initiator <- gsub(".*Initiators?: |Targets?:.*", "", page)
target <- gsub(".*Targets?: |Highest Overall Act:.*", "", page)
highest_overall_act <- gsub(".*Highest Overall Act: |Total Fatalities:.*","", page)
total_fatalities <- gsub(".*Total Fatalities: |Outcome:.*", "",page)
outcome <- gsub(".*Outcome:|Location:.*","", page)
location <- gsub(".*Location:|Incident:.*","", page)
incidents <- gsub(".*Incident: |MID 2.1 Sources:.*", "", page)
mid_sources <- gsub(".*MID 2.1 Sources: |ProQuest Historical Newspapers:.*", "", page)
search_records <- gsub(".*Record of Sources Searched|Narrative.*","", page)
narrative <- gsub(".*Narrative|Non-state actor involvement?.*", "", page)
non_state_actor_involvement <- gsub(".*Non-state actor involvement\\?|Issue Description.*","", page)
issue_description <- gsub(".*Issue Description|Authority.*","", page)
authority <- gsub(".*Authority|Disputed Facts.*","", page)
disputed_facts <- gsub(".*Disputed Facts|Ultimatums/Demands.*", "", page)
ultimatums_demands <- gsub(".*Ultimatums/Demands|Research Notes.*", "", page)
notes <- gsub(".*Research Notes|Articles.*", "", page)
articles <- gsub(".*Articles", "", page)

temp_df <- as.data.frame(cbind(mid_ID, mid_name, start_date, end_date, initiator, target,
                 highest_overall_act, total_fatalities, outcome, location, incidents,
                 mid_sources, narrative, non_state_actor_involvement, issue_description,
                 authority, disputed_facts, ultimatums_demands, notes, articles, search_records, url))

final_df <- rbind(final_df, temp_df) %>% na.omit

}




write_csv(final_df, "mids_scrape.csv")


write_file(page, "page.txt")


