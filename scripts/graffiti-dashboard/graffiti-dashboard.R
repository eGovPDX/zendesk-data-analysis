gc()
#### LIBRARIES ####
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)

### Graffiti API Request#####
##api authentication and base
 token <- Sys.getenv("ZENDESK_API_TOKEN")
 email <- Sys.getenv("ZENDESK_API_EMAIL")

#base url and query parameters
base_url <- "https://portlandoregon.zendesk.com/api/v2/search/export.json?"
query <- "query="
type <- "&filter[type]=ticket"
group <- ""
form <- "+custom_field_6353388345367:report_graffiti"

#concatenate request URL
request_url <- paste0(base_url,query,form,type)

#Initial call to tickets export API
call <- GET(URLencode(request_url),
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,simplifyVector = TRUE,flatten=TRUE)
next_url <- api_response$links$`next`


# Initialize variables for while loop
i <- 1
apidata <- api_response$results
has_more <- TRUE

#While loop to pull all tickets from Zendesk
while(has_more==TRUE) {
  call <- GET(next_url,
              authenticate(email,token,type="basic"))
  raw_content <- rawToChar(call$content)
  api_response <- fromJSON(raw_content,simplifyVector = TRUE,flatten=TRUE)
  has_more <- api_response$meta$has_more
  next_url <- api_response$links$`next`
  temp <- api_response$results
  apidata <- bind_rows(apidata,temp)
}


#un-nest the lists in the dataframe we get from the API calls into separate rows.
apidata_clean <- apidata %>% mutate(across(where(is.list),as.character))

list1 <- apidata %>% unnest_longer(
  col=custom_fields,
  values_to='values_list',
  indices_to='id_list',
  )


list1_clean <- unnest(list1,cols=values_list,values_to='values_cust')

apidata_wide <- pivot_wider(list1_clean,id_cols=id,names_from="id1")

apidata_clean <- left_join(apidata,apidata_wide,by='id')


#replace nested lists with character strings
apidata_clean <- apidata_clean %>% mutate(across(where(is.list),as.character))










## EXTRACT FIELDS FROM TICKET DESCRIPTION FIELD##
#replace new lines and returns with semicolons. Probably not necessary, but keeps things a bit simpler.
apidata_clean <- apidata_clean %>% mutate(
  description = gsub("[\r\n]", ";",description)
)

apidata_clean <- apidata_clean %>% mutate(
  description = gsub(";;", ";",description)
)

apidata_clean <- apidata_clean %>% mutate(
  description = gsub(";;", ";",description)
)


#find maximum number of splits needed in the description field.
max_splits <- max(str_count(apidata_clean$description, pattern = ";"))
i <- 1
apidata_clean <- apidata_clean %>% relocate(description)

apidata_clean <- separate(apidata_clean,description,sep=";",remove=FALSE,extra="merge",c(paste0("split.",toString(i)),paste0("split.",toString(i+1))))

#for each semicolon in the description field create another split field.
#New field names need to be created dynamically so we can have a flexible number of splits
while(i<max_splits) {
  apidata_clean <- separate(apidata_clean,paste0("split.",i+1),sep=";",remove=TRUE,extra="merge",c(paste0("split.",toString(i+1)),paste0("split.",toString(i+2))))
  i<-i+1
}


##
apidata_clean$loc_type <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$loc_type <-
    ifelse(
      apidata_clean$loc_type=="NA", #if loc_type field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Location type:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Location type: ","",apidata_clean[,i+1]),
        "NA" #set loc_type = value of column, removing the "Location type " string to retain only the useful data
      ),apidata_clean$loc_type
    )
  i <- i+1
}

apidata_clean$repeat_offender <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$repeat_offender <-
    ifelse(
      apidata_clean$repeat_offender=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Is it from a repeat offender",apidata_clean[,i+1]), #if column contains field name:
        sub("Have you seen this graffiti or tag before\\? Is it from a repeat offender\\? ","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$repeat_offender
    )
  i <- i+1
}

apidata_clean$resolution <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$resolution <-
    ifelse(
      apidata_clean$resolution=="NA", #if field has no useful data (NA) then check see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Resolution:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Resolution: ","",apidata_clean[,i+1]),
        "NA" #set field = value of column, removing the prompt string to retain only the useful data
      ),apidata_clean$resolution
    )
  i <- i+1
}



apidata_clean$prop_type <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$prop_type <-
    ifelse(
      apidata_clean$prop_type=="NA", #if loc_type field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("type of property is it?",apidata_clean[,i+1]), #if column contains Location type:
        sub("What type of property is it\\? ","",apidata_clean[,i+1]),
        "NA" #set loc_type = value of column, removing the "Location type " string to retain only the useful data
      ),apidata_clean$prop_type
    )
  i <- i+1
}


apidata_clean$loc_name <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$loc_name <-
    ifelse(
      apidata_clean$loc_name=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Location name:",apidata_clean[,i+1]), #if column contains field name:
        sub("Location name: ","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$loc_name
    )
  i <- i+1
}

apidata_clean$loc_details <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$loc_details <-
    ifelse(
      apidata_clean$loc_details=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Details:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Details: ","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$loc_details
    )
  i <- i+1
}

apidata_clean$address <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$address <-
    ifelse(
      apidata_clean$address=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Address:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Address: ","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$address
    )
  i <- i+1
}

# does the ticket include a portlandmaps link (implies that the requester is a community member vs. a cleanup crew)
apidata_clean$portlandmaps_indesc <- 0
i <- 1
while(i<max_splits) {
  apidata_clean$portlandmaps_indesc <-
    ifelse(
      apidata_clean$portlandmaps_indesc==0, #if field has no useful data (0) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("PortlandMaps",apidata_clean[,i+1]), #if column contains Location type:
        1, #set field = value of column, removing the prompt string to retain only the useful data
        0
      ),apidata_clean$portlandmaps_indesc
    )
  i <- i+1
}

apidata_clean$above_groundlvl <- 0
i <- 1
while(i<max_splits) {
  apidata_clean$above_groundlvl <-
    ifelse(
      apidata_clean$above_groundlvl==0, #if field has no useful data (0) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("above ground level? Yes",apidata_clean[,i+1]), #if column contains Location type:
        1, #set field = value of column, removing the prompt string to retain only the useful data
        0
      ),apidata_clean$above_groundlvl
    )
  i <- i+1
}


apidata_clean$on_mural <- 0
i <- 1
while(i<max_splits) {
  apidata_clean$on_mural <-
    ifelse(
      apidata_clean$on_mural==0, #if field has no useful data (0) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("on mural? Yes",apidata_clean[,i+1]), #if column contains Location type:
        1, #set field = value of column, removing the prompt string to retain only the useful data
        0
      ),apidata_clean$on_mural
    )
  i <- i+1
}


apidata_clean$contact_name <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$contact_name <-
    ifelse(
      apidata_clean$contact_name=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Contact Name:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Contact Name: ","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$contact_name
    )
  i <- i+1
}

apidata_clean$contact_email <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$contact_email <-
    ifelse(
      apidata_clean$contact_email=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Contact Email:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Contact Email: ","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$contact_email
    )
  i <- i+1
}

apidata_clean$sqft_abated <- "NA"
i <- 1
while(i<max_splits) {
  apidata_clean$sqft_abated <-
    ifelse(
      apidata_clean$sqft_abated=="NA", #if field has no useful data (NA) then check to see if the data we need is in the split column, otherwise leave it as is.
      ifelse(
        grepl("Square footage of abatement:",apidata_clean[,i+1]), #if column contains Location type:
        sub("Square footage of abatement:","",apidata_clean[,i+1]), #set field = value of column, removing the prompt string to retain only the useful data
        "NA"
      ),apidata_clean$sqft_abated
    )
  i <- i+1
}

#clean sq ft abated so it only includes numbers
apidata_clean$sqft_abated <- gsub("[^0-9.-]", "", apidata_clean$sqft_abated)


#Hard code custom field names
apidata_clean <- apidata_clean %>% rename(
  "cust_repeat_offender" = "8197855908375",
  "waiver_url"="6379451469847",
  "hate_gang"="6586776052759",
  "webform_id"="6353388345367",
  "customfld_lon"="5581490332439",
  "resolution_url"="6355783758871",
  "okfor_public"="6807910266391",
  "referto_parks"="9255868285207",
  "customfld_lat"="5581480390679",
  "customfld_address"="1500012743961",
  "contact_type"="1500007391541",
  "form_uuid"="1500013095781",
  "referto_pbot"="8617236904343",
  "public_description"="7557381052311",
  "waiver_received"="6373817212823",
  "property_type"="9205221816983",
  "includedfor_kb"="1500007510961",
  "graffiti_status"="10074777716119",
  "graffiti_contractors"="10249202452887"
)



#remove empty columns and splits
apidata_clean <- apidata_clean %>% discard(~all(is.na(.) | . ==""))
apidata_clean <- apidata_clean %>% select(-starts_with("split"))
apidata_clean <- apidata_clean %>% select(-"custom_fields","fields")
apidata_search <- apidata_clean

## CALL METRICS EVENTS API. ##
#I really want the metrics API but it does not include archived tickets.##
base_url <- "https://portlandoregon.zendesk.com/api/v2/incremental/ticket_metric_events.json?"

#set start time to beginning of 2020
query <- "start_time=1577836800"

# Concatenate query parameters to build request URL
request_url <- paste0(base_url,query)


#call Zendesk Metrics Events API and parse JSON object
call <- GET(URLencode(request_url),
            authenticate(email,token,type="basic"))
raw_content <- rawToChar(call$content)
api_response <- fromJSON(raw_content,flatten=TRUE)

#initialize the URL for next page and set end_of_stream, a logical field that we'll use for our while loop
next_url <- api_response$next_page
end_stream <- api_response$end_of_stream

# Initialize field to hold all JSON objects in the API call
apidata_metric_events <- api_response$ticket_metric_events

# Loop through API call and union JSON objects together
while(end_stream==FALSE) {
  call <- GET(next_url,
              authenticate(email,token,type="basic"))
  raw_content <- rawToChar(call$content)
  api_response <- fromJSON(raw_content,flatten=TRUE)
  next_url <- api_response$next_page
  end_stream <- api_response$end_of_stream
  temp <- api_response$ticket_metric_events
  apidata_metric_events <- bind_rows(apidata_metric_events,temp)
}

#Filter metrics events data to only show ticket solved events
apidata_solved <- apidata_metric_events[apidata_metric_events$metric == 'resolution_time', ]
apidata_solved <- apidata_solved[apidata_solved$type == 'update_status', ]

#only show most recent ticket solved events
apidata_solved <- apidata_solved %>% group_by(ticket_id) %>% slice(which.max(status.calendar
))

#rename tickets from the Search API to make ticket_id a consistent variable for joining
apidata_search <- apidata_search %>% rename(ticket_id = id)

#join the search API data with the metrics event API data
data_output <- left_join(apidata_search,apidata_solved, by='ticket_id')

#Clean Datetime field
data_output <- data_output %>% mutate(
  time = gsub("T", "",time),
  graffiti_status = gsub("10074777716119","",graffiti_status)
)

data_output <- data_output %>% mutate(
  time = gsub("Z", "",time)
)

#Set blank graffiti status to NA
data_output$graffiti_status[data_output$graffiti_status==""] <- NA

##### Create primary status from custom status types. Defaults to Zendesk status if "graffiti_status" is blank
data_output$graffiti_status_primary <- ifelse(
  grepl("open",data_output$graffiti_status,fixed=TRUE),"Open",ifelse(
    grepl("pending",data_output$graffiti_status,fixed=TRUE),"Pending",ifelse(
      grepl("solved",data_output$graffiti_status,fixed=TRUE),"Solved",data_output$status
        )
      )
    )

#convert Zendesk statuses to either open, pending, or solved (this is not the most efficient way to handle this, but works for now)
data_output$graffiti_status_primary <- ifelse(
  grepl("new",data_output$graffiti_status_primary,fixed=TRUE),"Open",ifelse(
    grepl("open",data_output$graffiti_status_primary,fixed=TRUE),"Open",ifelse(
      grepl("pending",data_output$graffiti_status_primary,fixed=TRUE),"Pending",ifelse(
        grepl("closed",data_output$graffiti_status_primary,fixed=TRUE),"Solved",ifelse(
          grepl("solved",data_output$graffiti_status_primary,fixed=TRUE),"Solved",data_output$graffiti_status_primary
        )
      )
    )
  )
)


##### Create status details from custom status types. This is pretty inflexible. Revisit.
data_output$graffiti_status_details <- ifelse(
  grepl("referred_to_pbot_parking_ops",data_output$graffiti_status,fixed=TRUE),"Referred to PBOT parking ops",ifelse(
    grepl("ssl_contractor_resolution",data_output$graffiti_status,fixed=TRUE),"SSL asset referred to contractor",ifelse(
      grepl("referred_to_parks",data_output$graffiti_status,fixed=TRUE),"Referred to parks",ifelse(
        grepl("referred_to_pbot_maint_ops",data_output$graffiti_status,fixed=TRUE),"Referred to PBOT maintenance ops",ifelse(
          grepl("ssl_asset_cleaned_by_contractor",data_output$graffiti_status,fixed=TRUE),"SSL asset referred to contractor",ifelse(
            grepl("referred_to_private_partner",data_output$graffiti_status,fixed=TRUE),"Referred to private partner",ifelse(
              grepl("referred_to_public_partner",data_output$graffiti_status,fixed=TRUE),"Referred to public partner",ifelse(
                grepl("awaiting_waiver",data_output$graffiti_status,fixed=TRUE),"Awaiting waiver",ifelse(
                  grepl("awaiting_info",data_output$graffiti_status,fixed=TRUE),"Awaiting info",ifelse(
                    grepl("pbot_parking_ops_resolution",data_output$graffiti_status,fixed=TRUE),"PBOT parking ops resolution",ifelse(
                      grepl("1st_abatement",data_output$graffiti_status,fixed=TRUE),"1st abatement letter sent",ifelse(
                        grepl("2nd_abatement",data_output$graffiti_status,fixed=TRUE),"2nd abatement letter sent",ifelse(
                          grepl("cleaned_by_contractor",data_output$graffiti_status,fixed=TRUE),"Cleaned by contractor",ifelse(
                              grepl("cleaned_by_property_owner",data_output$graffiti_status,fixed=TRUE),"Cleaned by property owner",ifelse(
                                grepl("lack_of_response",data_output$graffiti_status,fixed=TRUE),"Lack of response",ifelse(
                                  grepl("referred_to_tri-met",data_output$graffiti_status,fixed=TRUE),"Referred to Tri-Met",ifelse(
                                    grepl("contractor_resolution",data_output$graffiti_status,fixed=TRUE),"Contractor resolution",ifelse(
                                      grepl("ssl_asset_referred_to_contractor",data_output$graffiti_status,fixed=TRUE),"SSL asset referred to contractor",ifelse(
                                        grepl("referred_to_odot",data_output$graffiti_status,fixed=TRUE),"Referred to ODOT",NA
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )


#Specify responsible party
data_output$responsible_party <- ifelse(
  grepl("parks",data_output$graffiti_status_details,fixed=TRUE),"Parks",ifelse(
    grepl("PBOT maintenance ops",data_output$graffiti_status_details,fixed=TRUE),"PBOT maintenance ops",ifelse(
      grepl("PBOT parking ops",data_output$graffiti_status_details,fixed=TRUE),"PBOT parking ops",ifelse(
        grepl("SSL",data_output$graffiti_status_details,fixed=TRUE),"PBOT SSL",ifelse(
          grepl("private partner",data_output$graffiti_status_details,fixed=TRUE),"Private Partner",ifelse(
            grepl("public partner",data_output$graffiti_status_details,fixed=TRUE),"Public Partner",ifelse(
              grepl("Tri-Met",data_output$graffiti_status_details,fixed=TRUE),"Tri-Met",ifelse(
                grepl("ODOT",data_output$graffiti_status_details,fixed=TRUE),"ODOT",ifelse(
                  grepl("property owner",data_output$graffiti_status_details,fixed=TRUE),"Property Owner","Contractor"

                )
              )
            )
          )
        )
      )
    )
  )
)


#Convert all NAs to "FALSE" for hate speech variable
data_output$hate_gang <- ifelse(
  is.na(data_output$hate_gang),"FALSE",data_output$hate_gang)


#Convert blanks to NAs
data_output$graffiti_contractors <- ifelse(
  data_output$graffiti_contractors=="",NA,data_output$graffiti_contractors)

data_output <- data_output %>% select(
  "address",
  "graffiti_contractors",
  "created_at",
  "customfld_lat",
  "customfld_lon",
  "group_id",
  "hate_gang",
  "ticket_id",
  "loc_details",
  "loc_name",
  "public_description",
  "url",
  "responsible_party",
  "sqft_abated",
  "graffiti_status_primary",
  "graffiti_status_details",
  "status"
  )

output_dir <- file.path(getwd(), "data")

#write data output to csv
dir.create(output_dir)
write.csv(data_output, file.path(output_dir, "graffiti-dashboard.csv"))
