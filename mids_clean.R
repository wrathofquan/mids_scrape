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
webElem$sendKeysToElement(list("##")) #authenticate
webElem <- driver$findElement(using = "css selector", value = "input.inputbutton")
webElem$clickElement()


counter_0999 <- 1:64
counter_1999 <- 1:252
counter_2999 <- 1:521
counter_3999 <- 1:392
counter_4000 <- 1:301


#loop to grab MIDS urls for later scraping
df_0999 <- list()
  driver$navigate("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=Main.MIDs0000-0999")  
    for(i in counter_0999){
      links <- driver$findElement(using = "xpath", value = paste("//*[@id='wikitext']/ul/li[",i,"]/a", sep= ""))
      temp <- unlist(lapply(links, function(x){links$getElementAttribute("href")}))
      temp <- unique(temp)
      print(temp)
      df_0999 <- rbind(df_0999, temp) 
    }


df_1999 <- list()
  driver$navigate("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=Main.MIDs1000-1999")  
  for(i in counter_1999){
    links <- driver$findElement(using = "xpath", value = paste("//*[@id='wikitext']/ul/li[",i,"]/a", sep= ""))
    temp <- unlist(lapply(links, function(x){links$getElementAttribute("href")}))
    temp <- unique(temp)
    print(temp)
    df_1999 <- rbind(df_1999, temp) 
  }

df_2999 <- list()
  driver$navigate("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=Main.MIDs2000-2999")  
  for(i in counter_2999){
    links <- driver$findElement(using = "xpath", value = paste("//*[@id='wikitext']/ul/li[",i,"]/a", sep= ""))
    temp <- unlist(lapply(links, function(x){links$getElementAttribute("href")}))
    temp <- unique(temp)
    print(temp)
    df_2999 <- rbind(df_2999, temp) 
  }

df_3999 <- list()
  driver$navigate("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=Main.MIDs3000-3999")  
  for(i in counter_3999){
    links <- driver$findElement(using = "xpath", value = paste("//*[@id='wikitext']/ul/li[",i,"]/a", sep= ""))
    temp <- unlist(lapply(links, function(x){links$getElementAttribute("href")}))
    temp <- unique(temp)
    print(temp)
    df_3999 <- rbind(df_3999, temp) 
  }


df_4000 <- list()
  driver$navigate("http://web.stanford.edu/group/tomzgroup/cgi-bin/pmwiki/index.php?n=Main.MIDs4000")
  for(i in counter_4000){
    links <- driver$findElement(using = "xpath", value = paste("//*[@id='wikitext']/ul/li[",i,"]/a", sep= ""))
    temp <- unlist(lapply(links, function(x){links$getElementAttribute("href")}))
    temp <- unique(temp)
    print(temp)
    df_4000 <- rbind(df_4000, temp) 
  }
  


df <- rbind(df_0999, df_1999, df_2999, df_3999, df_4000)


#filter out weird urls containing 'edit' in them
df_1 <- Filter(function(x) !any(grepl("edit", x)), df)

# loop through urls and scrape
final_df <- list()
for(i in df_1){
driver$navigate(i)
page <- read_html(driver$getPageSource()[[1]]) %>% html_text() 
page <- gsub(".* ARTICLE TITLE AND DATE HERE"," ", page) 
page <- gsub("Page last modified.*", " ", page)
page <- gsub("[\r\n\t]", "", page)
page <- gsub("\\(Edit Section ↓)", "", page)
page_split <- str_split(string = page, pattern = "Record of Sources Searched", simplify = TRUE) # split for accuracy of fields
##start extracting the fields for each MID
mid_ID <- as.numeric(gsub(".*Dispute InformationMID #: |MID Name:.* ", "", page[1]))
url <- paste0(i)
mid_name <- gsub(".*MID Name: |Start Date:.*", "", page_split[, 1])
start_date <- gsub(".*Start Date:|End Date:.*", "", page_split[, 1])
end_date <- gsub(".*End Date: |Initiators?:.*", "", page_split[, 1])
initiator <- gsub(".*Initiators?: |Targets?:.*", "", page_split[, 1])
target <- gsub(".*Targets?: |Highest Overall Act:.*", "", page_split[, 1])
highest_overall_act <- gsub(".*Highest Overall Act: |Total Fatalities:.*","", page_split[, 1])
total_fatalities <- gsub(".*Total Fatalities: |Outcome:.*", "",page_split[, 1])
outcome <- gsub(".*Outcome:|Location:.*","", page_split[, 1])
location <- gsub(".*Location:|Incident:.*","", page_split[, 1])
incidents <- gsub(".*Incident: |MID 2.1 Sources:.*", "", page_split[, 1])
mid_sources <- gsub(".*MID 2.1 Sources: ", "", page_split[, 1])
### scrape second split
search_records <- gsub(".*ProQuest Historical Newspapers:|Narrative.*","", page_split[,2])
narrative <- gsub(".*Narrative|Non-state actor involvement?.*", "", page)
non_state_actor_involvement <- gsub(".*Non-state actor involvement\\?|Issue Description.*","", page_split[,2])
issue_description <- gsub(".*Issue Description|Authority.*","", page_split[,2])
authority <- gsub(".*Authority|Disputed Facts.*","", page_split[,2])
disputed_facts <- gsub(".*Disputed Facts|Ultimatums/Demands.*", "", page_split[,2])
ultimatums_demands <- gsub(".*Ultimatums/Demands|Research Notes.*", "", page_split[,2])
notes <- gsub(".*Research Notes|Articles.*", "", page_split[,2])
articles <- gsub(".*Articles", "", page_split[,2])

temp_df <- as.data.frame(cbind(mid_ID, mid_name, start_date, end_date, initiator, target,
                 highest_overall_act, total_fatalities, outcome, location, incidents,
                 mid_sources, narrative, non_state_actor_involvement, issue_description,
                 authority, disputed_facts, ultimatums_demands, notes, articles, search_records, url))

final_df <- rbind(final_df, temp_df)}

write_csv(final_df, "mids_scrape.csv")


