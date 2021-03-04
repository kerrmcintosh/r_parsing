library(XML)
library(tidyverse)
# library(tidyverse)

#bring in XML file
clacks <- xmlParse(file = "data/Food_Hygiene_Information_Scheme_-_Clackmannanshire_Council.xml")

# TO DO   col_types()
clacks_root = xmlRoot(clacks)  #open xml and find fist node / starting point
xmlName(clacks_root) #check: "FHRSEstablishment"
xmlSize(clacks_root) #2 children
xmlName(clacks_root[[2]]) #name of root 2
clacks_root[[2]] #Take to node I want to extract
# create dataframe
xml_data <- xmlToDataFrame(clacks_root[[2]], stringsAsFactors = FALSE) 
# %>% 
#   mutate_all(~type.convert(., as.is = T))

nrow(xml_data) #537
class(xml_data$FHRSID) # character

#NOW function to add new XML data

angus <- xmlParse(file = "data/Food_Hygiene_Information_Scheme_-_Angus_Council.xml")

add_xml_data <- function(xml_update){
  xml_root <- xmlRoot(xml_update) 
  new_xml_data <- xmlToDataFrame(xml_root [[2]], stringsAsFactors = FALSE) 
  bound_rows <- rbind(xml_data, new_xml_data)
  return(bound_rows)
}

xml_data <- add_xml_data(angus)

nrow(xml_data) #1562 - as expected

# View(xml_data)
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# XML files

library(readr)
library(tidyverse)
library(stringr)

#reasing in as a text 
log_file <- read_tsv("data/log_file.log") %>% 
  janitor::clean_names()

class(log_file$log)

ip_pattern <- "[0-9]+.[0-9]+.[0-9]+.[0-9]"

log_file <- log_file %>% 
mutate(date = str_replace_all(str_sub(log, 1, 5)," ", "")) %>% # extract date and remove white space
filter(date != "initi") %>% 
filter(nchar(date) >2)  %>% #remove order numbers so only have rows with a date
mutate(date = as.Date(date,
                      format = "%m/%d") ) %>%
mutate(time = str_replace_all(str_sub(log, 6, 15)," ", "")) %>% # extract time and remove white space
mutate(time = chron::chron(times=time)) %>% 
  mutate(info = str_sub(log, 16, 22)) %>% 
mutate(detail = str_sub(log, 23, -1)) %>% 
mutate(ip_address = str_extract(detail,ip_pattern)) # regex extractip addresses

# View(log_file)

# List only warning notifications
log_warnings <- log_file %>% 
  filter(info == "WARNING")

#Check ip address listings for high numbers - could then do visualisation on this
ip_check <- log_file %>% 
  drop_na(ip_address) %>% 
  group_by(ip_address, date) %>% 
  summarise(n = n())

# Bring up any mention of failure or errors - could have added new variable to main df with ife
failures <-  log_file %>%
  mutate(detail = str_to_lower(detail)) %>% # so as not to loose casing differences
  filter(grepl("failed | error",detail))
# View(failures)

#checking instances of certain words - earch individual 
log_file_words <- 
  log_file %>%
  select(detail) %>% 
  unnest_tokens(word, detail)

log_file_words %>%  
  # anti_join(stop_words) %>%
  count(word, sort = TRUE)


# text mine failed - count common words and string detect
# loop to look through words

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
#----------JSON ---------------

library(jsonlite)
library(readr)
# method 1 - to a list
json_file <- read_file("data/json_file.json")

prettify(json_file)

json_data <- parse_json(json_file) # creates list

json_data_ul <- unlist(json_data)
json_df <- as.data.frame(json_data_ul)
class(json_df) # dataframe


## ----unnest play
df <- tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h")
)

df %>%
  transform(y = strsplit(y, ",")) %>%
  unnest(y)

df <- tibble(
  x = 1:2,
  y = list(
    tibble(z = 1),
    tibble(z = 3:4)
  )
)
df %>% unnest(y)