#### LIBRARIES ####
library(devtools)
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(data.table)


#### API authentication and base ####
token <- Sys.getenv("ZENDESK_API_TOKEN")
email <- Sys.getenv("ZENDESK_API_EMAIL")

#### Get Raw Zendesk Article Metadata #####
#set request URL
domain <- "https://portlandoregon.zendesk.com/api/v2/"
path <- "help_center/articles.json?"
request_url <- paste0(domain,path)

# Call Zendesk Articles API to pull metadata on each article
i <- 1
apidata <- list()
next_url <- ""

while(!is.null(next_url)) {
call <- GET(paste0(request_url,"page=",i),
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
next_url <- api_response$next_page
temp <- api_response$articles
apidata <- bind_rows(apidata,temp)
i <- i+1
}



#Replace nested lists with character strings
zendesk_articles <- apidata %>% mutate(across(where(is.list),as.character))

zendesk_articles <- zendesk_articles %>%
  select(id,section_id,name)

#### write data output to csv ####
output_dir <- file.path(getwd(), "data")
dir.create(output_dir)
write.csv(zendesk_articles, file.path(output_dir, "articles.csv"))
