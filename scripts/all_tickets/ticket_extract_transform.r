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

domain <- "https://portlandoregon.zendesk.com/api/v2/"

####===== Pull Zendesk Article Metadata =====#####
#set request URL
path <- "help_center/articles"
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

#replace nested lists with character strings
metadata <- apidata %>% mutate(across(where(is.list),as.character))

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , apidata
  , temp
  , i
  , next_url
  , temp
  , raw_content
)

gc(reset = TRUE)



#####===== Get Zendesk Groups data =====#####
path <- "groups"
request_url <- paste0(domain,path)

#Initial API call
call <- GET(URLencode(request_url), 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
groups <- api_response$groups

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
)

gc(reset = TRUE)



#####===== Get Zendesk Ticket Forms data =====#####
path <- "ticket_forms"
request_url <- paste0(domain,path)

#Initial API call
call <- GET(URLencode(request_url), 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
ticket_forms <- api_response$ticket_forms

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
)

gc(reset = TRUE)


####===== Get Zendesk Article Categories data =====#####
#api authentication and base
base_url <- "https://portlandoregon.zendesk.com/api/v2/help_center/categories.json"
call <- GET(paste0(base_url), 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
categories <- api_response$categories

#write to csv
write.csv(categories, paste0(directory,"/article_categories.csv"))

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
  , base_url
)

gc(reset = TRUE)



####===== Get Zendesk Article Sections data =====#####
#api authentication and base
base_url <- "https://portlandoregon.zendesk.com/api/v2/help_center/sections.json"
call <- GET(paste0(base_url), 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)

# Loop to pull all sections
i <- 1
apidata <- api_response$sections
next_url <- api_response$next_page

#While loop to move through API response pages and union all data together
while(!is.null(next_url)) {
  call <- GET(next_url, 
              authenticate(email,token,type="basic"))
  raw_content <- rawToChar(call$content)
  api_response <- fromJSON(raw_content,flatten=TRUE)
  next_url <- api_response$next_page
  temp <- api_response$sections
  apidata <- bind_rows(apidata,temp)
}

#set up a dataframe for each layer in the article section hierarchy
sec_all <- apidata %>% 
  select(parent_section_id,id,url,html_url,category_id,created_at,updated_at,name,description) %>% 
  rename("section_id"="id")

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
  , base_url
  , apidata
  , temp
  , next_url
  , i
)

gc(reset = TRUE)


####===== Get ticket events =====####
#May be able to use incremental metric events API instead
#Note to self - do we still need to use the audits API if we do this?
load(file = "next_url.Rdata")
load(file = "ticket_events.Rdata")

# base_url <- "https://portlandoregon.zendesk.com/api/v2/incremental/ticket_events?start_time=1640995200"

#start_time <- "1669881600" #2022-12-01 00:00:00
#start_time <- "1640995200" #2022-01-01 00:00:00

#request_url <- paste0(base_url,start_time,".json")
#request_url <- next_url
request_url <- base_url

#make initial call to the audits API
# call <- GET(request_url, 
#             authenticate(email,token,type="basic"))
# raw_content <- rawToChar(call$content)
# api_response <- fromJSON(raw_content,flatten=TRUE)
end_of_stream <- ""
apidata <- ticket_events
# next_url <- api_response$next_page
# apidata <- api_response$ticket_events

while(!is.null(end_of_stream)) {
  call <- GET(next_url, 
              authenticate(email,token,type="basic"))
  raw_content <- rawToChar(call$content)
  api_response <- fromJSON(raw_content,flatten=TRUE)
  next_url <- api_response$next_page
  temp <- api_response$ticket_events
  apidata <- bind_rows(apidata,temp)
  end_of_stream <- api_response$end_of_stream
  Sys.sleep(10)
}

incremental_error_check <- api_response

save(apidata, file = "ticket_events.Rdata")
save(next_url, file = "next_url.Rdata")

# unlist nested list with id
apidata.events <- rbindlist(apidata$child_events,use.names=T,fill=T,idcol=T)
#apidata.events <- subset(apidata.events, select = -c(1))
#apidata.events <- filter(apidata.events,type=="KnowledgeLinked" | field_name=="status")


# create same id in remaining data frame
apidata$.id <- seq.int(nrow(apidata))

# join data frame with unlisted list
ticket_events <- left_join(apidata, apidata.events, by = c(".id"=".id"))
ticket_events <- filter(ticket_events,!is.na(status))



####===== Get  knowledge base links data =====####
#Call ticket audit API to pull data on when someone uses knowledge links
base_url <- "https://portlandoregon.zendesk.com/api/v2/ticket_audits.json"
#start_date <- "&[start_date]=2022-12-01"
#end_date <- "&[start_date]=2022-12-31"
#request_url <- paste0(base_url,start_date,end_date,".json")
request_url <- base_url

#make initial call to the audits API
call <- GET(request_url, 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
next_url <- api_response$before_url

# initialize while loop fields
i <- 1
apidata <- api_response$audits

#While loop to move through API response pages and union all data together
#Would like to set this up to approximate incremental updates so it doesn't take so long
while(!is.null(next_url)) {
  call <- GET(next_url, 
              authenticate(email,token,type="basic"))
  raw_content <- rawToChar(call$content)
  api_response <- fromJSON(raw_content,flatten=TRUE)
  next_url <- api_response$before_url
  temp <- api_response$audits
  apidata <- bind_rows(apidata,temp)
}

#Removed unneeded fields
apidata <- apidata %>% select(
  -"metadata.suspension_type_id"
  , -"metadata.flags",-"metadata.trusted"
  , -"metadata.notifications_suppressed_for"
  , -"metadata.system.location"
  , -"metadata.system.latitude"
  , -"metadata.system.longitude"
  , -"metadata.system.client"
  , -"metadata.system.ip_address"
  , -"metadata.system.message_id"
  , -"metadata.system.email_id"
  , -"metadata.system.raw_email_identifier"
  , -"metadata.system.json_email_identifier"
  , -"metadata.system.eml_redacted"
  , -"metadata.system.machine_generated"
  , -"metadata.flags_options.25.trusted"
  , -"metadata.flags_options.2.trusted"
  , -"metadata.flags_options.15.trusted"
  , -"metadata.flags_options.9.trusted"
  , -"metadata.flags_options.4.trusted"
  , -"metadata.flags_options.21.trusted"
  , -"metadata.flags_options.11.trusted"
  , -"metadata.flags_options.11.message.user"
  , -"metadata.flags_options.11.message.user_id"
)

# unlist nested list with id
apidata.events <- apidata %>% slice(1:10000)
apidata.events <- rbindlist(apidata.events$events,fill=T)
apidata.events <- filter(apidata.events,type=="KnowledgeLinked" | field_name=="status")
x <- 10001
y <- 20000

#getting a stack overflow error when I try to bind all at once, so I'm doing it bit by bit
while(y<=nrow(apidata)+10000) {
  temp <- apidata %>% slice(x:y)
  temp <- rbindlist(temp$events,fill=T)
  temp <- filter(temp,type=="KnowledgeLinked" | field_name=="status")
  apidata.events <- bind_rows(apidata.events,temp)
  x <- x+10000
  y <- y+10000
  }

# apidata$id <- format(apidata$id, scientific = FALSE)
# apidata.events$id <- format(apidata.events$id, scientific = TRUE)
# apidata.events$audit_id <- format(apidata.events$audit_id, scientific = TRUE)
# apidata.events$audit_id

apidata.events$audit_id <- as.character(apidata.events$audit_id)


# create same id in remaining data frame
#apidata$id1 <- seq.int(nrow(apidata))

# join data frame with unlisted list
article_links <- left_join(apidata, apidata.events, by = c("id" = "audit_id"))

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
  , base_url
  , apidata
  , temp
  , next_url
  , i
  , x
  , y
  , apidata.events
)

gc(reset = TRUE)

# remove all remaining lists in the data
article_links <- article_links %>% mutate(across(where(is.list),as.character))
article_links <- article_links[, sapply(article_links, class) != "list"]



####===== Get  ticket field lookup table =====####
base_url <- "https://portlandoregon.zendesk.com/api/v2/ticket_fields"

#Initial API call
call <- GET(URLencode(base_url), 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
ticket_fields <- api_response$ticket_fields

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
  , base_url
)



####===== Get  tickets data =====####
#NEED TO UNNEST THE 311 ACTIONS TAKEN TO ALLOW FOR BETTER HANDLING OF MULTIPLE ENTRIES
#ALSO NEED INCREMENTAL REFRESH OF THIS DATA TO SPEED THIS UP
base_url <- "https://portlandoregon.zendesk.com/api/v2/search/export.json?"
group <- "-group:4549352062487"
ery <- "query="
type <- "&filter[type]=ticket"
request_url <- paste0(base_url,query,group,type)

#Initial API call
call <- GET(URLencode(request_url), 
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)
next_url <- api_response$links$`next`

# Initialize fields for a while loop to request all ticket records from the Zendesk API
i <- 1
apidata <- api_response$results
has_more <- TRUE

# While loop to request all ticket records from the Zendesk API
while(has_more==TRUE) {
  call <- GET(next_url, 
              authenticate(email,token,type="basic"))
  raw_content <- rawToChar(call$content)
  api_response <- fromJSON(raw_content,flatten=TRUE)
  has_more <- api_response$meta$has_more
  next_url <- api_response$links$`next`
  temp <- api_response$results
  apidata <- bind_rows(apidata,temp)
}

# unlist nested list with id
apidata.fields <- rbindlist(apidata$custom_fields,fill=T,idcol="id1")
apidata.fields <- apidata.fields[!is.na(apidata.fields$value),]

# create same id in remaining data frame
apidata$id1 <- seq.int(nrow(apidata))

# join data frame with unlisted list
tickets <- left_join(apidata, apidata.fields, by = "id1")

#remove unused dataframes and fields. Clean unused data
remove(
  call
  , api_response
  , raw_content
  , base_url
  , apidata
  , temp
  , next_url
  , i
  , has_more
  , group
  , request_url
  , apidata.fields
)

gc(reset = TRUE)

#Add ticket field definitions to tickets data
ticket_fields <- ticket_fields %>% rename("field_id" = "id")
tickets <- tickets %>% rename("field_id" = "id.y")
tickets_joined <- left_join(tickets, ticket_fields, by = "field_id")

gc(reset = TRUE)

#pivot fields to make dataset wide - avoid having too many rows with repeated data to make the size a little more manageable.
ticket_fieldvals <- tickets_joined %>%
  pivot_wider(id_cols = "id.x", names_from="title")

tickets <- tickets_joined %>%
  select(
        "id.x"
        , "status"
        , "requester_id"
        , "submitter_id"
        , "assignee_id"
        , "organization_id"
        , "group_id"
        , "via.channel"
        , "ticket_form_id"
        , "created_at.x"
      ) %>%
      rename(
        "ticket_id" = "id.x"
      )
    
tickets <- distinct(tickets)

# join data frame with ticket fields. Definitely possible to do this more efficiently
tickets <- inner_join(tickets, ticket_fieldvals, by = c("ticket_id" = "id.x"))

# convert lists into character strings.
# TO DO: MAKE SEPARATE COLUMNS FOR EACH RESOLUTION TYPE SO WE CAN HAVE MULTIPLE
tickets <- tickets %>% mutate(across(where(is.list),as.character))

#remove unused dataframes and fields. Clean unused data
remove(
  tickets_joined
  , ticket_fields
  , ticket_fieldvals
)

gc(reset = TRUE)



####===== Join categories and sections to ticket data =====####
#Repeatedly join section dataframe to itself to build article hierarchy
sec_joined <- left_join(sec_all, sec_all, by = c("parent_section_id" = "section_id"), suffix = c("",".1"))
sec_joined <- left_join(sec_joined, sec_joined, by = c("parent_section_id.1" = "section_id"), suffix = c("",".2"))
sec_joined <- left_join(sec_joined, sec_joined, by = c("parent_section_id.2" = "section_id"), suffix = c("",".3"))

#Because the number of section layers varies by article, we want to set a single section to represent the top-level section 
sec_joined$sec_a.id <- ifelse(is.na(sec_joined$parent_section_id.2),sec_joined$parent_section_id.1,sec_joined$parent_section_id.2)
sec_joined$sec_a.id <- ifelse(is.na(sec_joined$sec_a.id),sec_joined$parent_section_id,sec_joined$sec_a.id)
sec_joined$sec_a.id <- ifelse(is.na(sec_joined$sec_a.id),sec_joined$section_id,sec_joined$sec_a.id)

#Join to bring in section names
sec_joined <- left_join(sec_joined, sec_all, by = c("sec_a.id" = "section_id"), suffix = c("",".sec_a"))


#Repeat pattern for layer b - if top level parent id = Section A ID then we know that B uses the parent id at the next-lowest level, and so on
sec_joined$sec_b.id <- ifelse(sec_joined$parent_section_id.2==sec_joined$sec_a.id,sec_joined$parent_section_id.1,NA)
sec_joined$sec_b.id <- ifelse(sec_joined$parent_section_id.1==sec_joined$sec_a.id,sec_joined$parent_section_id,sec_joined$sec_b.id)
sec_joined$sec_b.id <- ifelse(sec_joined$parent_section_id==sec_joined$sec_a.id,sec_joined$section_id,sec_joined$sec_b.id)

sec_joined <- left_join(sec_joined, sec_all, by = c("sec_b.id" = "section_id"), suffix = c("",".sec_b"))


#Repeat pattern for layer c
sec_joined$sec_c.id <- ifelse(sec_joined$parent_section_id.1==sec_joined$sec_b.id,sec_joined$parent_section_id,NA)
sec_joined$sec_c.id <- ifelse(sec_joined$parent_section_id==sec_joined$sec_b.id,sec_joined$section_id,sec_joined$sec_c.id)

sec_joined <- left_join(sec_joined, sec_all, by = c("sec_c.id" = "section_id"), suffix = c("",".sec_c"))


#Repeat pattern for layer d
sec_joined$sec_d.id <- ifelse(sec_joined$parent_section_id==sec_joined$sec_c.id,sec_joined$section_id,NA)

sec_joined <- left_join(sec_joined, sec_all, by = c("sec_d.id" = "section_id"), suffix = c("",".sec_d"))


#Select only the relevant fields to complete section lookup table
sec_lookup <- sec_joined %>% select(
  "section_id"
  , "category_id"
  , "name"
  , "description"
  , "sec_a.id"
  , "name.sec_a"
  , "sec_b.id"
  , "name.sec_b"
  , "sec_c.id"
  , "name.sec_c"
  , "sec_d.id"
  , "name.sec_d"
  )

write.csv(sec_lookup, paste0(directory,"/section_lookup.csv"))

####===== Create single dataframe with tickets, categories, sections, and articles =====#####
#create single dataframe with all relevant category, section, and article information
article_meta <- left_join(metadata, sec_lookup, by = c("section_id" = "section_id"))
article_meta <- left_join(article_meta, categories, by = c("category_id" = "id"))
article_meta <- select(article_meta,-"body")

write.csv(article_meta, paste0(directory,"/article_metadata.csv"))

#Break up article_links into knowledge capture and solved events
knowledge_capture <- filter(article_links, type == "KnowledgeLinked") 

## TO DO: Why am I only getting created_at dates in January 2023
#solve_date <- subset(article_links, (value == "solved" | value == "closed")) 
solve_date <- filter(ticket_events, (status == "solved"))
create_date <- filter(ticket_events, (event_type.y == "Create"))
open_date <- filter(ticket_events, (status == "open"))
pending_date <- filter(ticket_events, (status == "pending"))

#unique(apidata.events$status)

#collapse event dates to a single row
solve_date <- solve_date %>% group_by(ticket_id) %>% summarize(solve_date = min(timestamp))
create_date <- create_date %>% group_by(ticket_id) %>% summarize(create_date = min(timestamp))
open_date <- open_date %>% group_by(ticket_id) %>% summarize(open_date = min(timestamp))
pending_date <- pending_date %>% group_by(ticket_id) %>% summarize(pending_date = min(timestamp))

knowledge_capture_small <- knowledge_capture %>% 
  select(-events
         ,-author_id.x
         ,-via.source.rel.x
         ,-via.source.from.address
         ,-via.source.from.name
         ,-via.source.from.original_recipients
         ,-via.source.from.deleted.x
         ,-via.source.from.deleted.y
         ,-via.source.from.title.x
         ,-via.source.from.title.y
         ,-via.source.from.id.x
         ,-via.source.from.ticket_id
         ,-via.source.from.subject
         ,-via.source.from.channel
         ,-body.html_url
         ,-body.url
         )

gc(reset=TRUE)

tickets_all <- left_join(tickets, knowledge_capture_small, by = "ticket_id")

#Join tickets with article_links data to get all knowledge capture events
tickets_all <- left_join(tickets_all, solve_date, by = "ticket_id")
tickets_all <- left_join(tickets_all, create_date, by = "ticket_id")
tickets_all <- left_join(tickets_all, open_date, by = "ticket_id")
tickets_all <- left_join(tickets_all, pending_date, by = "ticket_id")

#Join with article metadata
tickets_all <- tickets_all %>% rename("article_id" = "body.id")
#metadata <- metadata %>% rename("article_id" = "id")
#tickets_full <- left_join(tickets_full, metadata, by = "article_id")

#tickets_full <- left_join(tickets_full, sec_lookup, by = "section_id")
tickets_all <- full_join(tickets_all, article_meta, by = c("article_id" = "id"))
tickets_all <- full_join(tickets_all, groups, by = c("group_id" = "id"))
tickets_all <- full_join(tickets_all, ticket_forms, by = c("ticket_form_id" = "id"))

length(unique(tickets_all$id.y.y))

#select only the fields that we need. Can expand this.
ticket_data <- tickets_all %>% 
  select(
  "ticket_id"
  , "status"
  , "via.channel"
  # , "Asset ID"
  # , "Company"
  # , "Repeat offender"
  # , "Confidentiality Waiver"
  # , "Waiver URL"
  # , "Waiver Received Date"
    , "PBOT Signs and Streetlights Asset"
  # , "Hate speech or gang related"
    , "Related ticket ID"
  # , "Vehicle License Plate"
  , "Webform ID"
  , "Location Lon"
  # , "Confidentiality Opt-In"
  # , "Resolution URL"
  # , "OK for public"
  , "Refer to Parks"
  # , "Graffiti abatement resolution"
  # , "Letter Generation URL"
  , "Location Lat"
  , "Location Address"
  , "Service Level Expectation"
  # , "Reported Issue"
  , "Contact Type"
  , "If transferred/forwarded"
  , "Graffiti Status"
  # , "Mural"
  # , "Vehicle Make and Model"
  , "Refer to PBOT"
  , "Graffiti contractor"
  # , "Public Description"
  # , "Waiver received"
  # , "Vehicle Present"
  # , "Location Notes"
  # , "Attachments"
  , "311 Action Taken"
  , "Square footage"
  , "Property Type"
  # , "Above ground floor"
  # , "Included for KB"
  , "created_at.x"
  , "created_at.x.x"
  , "id.y"
  , "article_id"
  , "name.x"
  #, "body.url"
  , "solve_date"
  , "description.x"
  , "sec_a.id"
  , "sec_b.id"
  , "sec_c.id"
  , "sec_d.id"
  , "name.sec_a"
  , "name.sec_b"
  , "name.sec_c"
  , "name.sec_d"
  , "name.x.x"
  , "name.y.y"
  , "name"
  , "end_user_visible"
  , "create_date"
  , "open_date"
  , "solve_date"
  , "pending_date"

) %>%
  rename("ticket_created_at" = "created_at.x.x"
         , "event_created_at" = "created_at.x"
         , "event_id" = "id.y"
         , "description" = "description.x"
         , "name.article" = "name.x"
         , "name.category" = "name.x.x"
         , "name.group" = "name.y.y"
         , "name.form" = "name"
         , "form_end_user_visible" = "end_user_visible"
         )

remove(apidata.events)
remove(knowledge_capture)

gc(reset = TRUE)

#write to csv
# save(ticket_data, file = "ticket_data.Rdata")
# save(article_links, file = "article_links.Rdata")
# save(article_meta, file = "article_meta.Rdata")
# save(categories, file = "categories.Rdata")
# save(groups, file = "groups.Rdata")
# save(knowledge_capture_small, file = "knowledge_capture.Rdata")
# save(metadata, file = "metadata.Rdata")
# save(sec_lookup, file = "sec_lookup.Rdata")
# save(ticket_events, file = "ticket_events.Rdata")
# save(ticket_forms, file = "ticket_forms.Rdata")
# save(tickets, file = "tickets.Rdata")


ticket_data <- ticket_data %>% mutate(across(where(is.list),as.character))
write.csv(ticket_data, paste0(directory,"/tickets.csv"))
