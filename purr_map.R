library(purrr)  
library(repurrrsive) 
#map on a list

colour_feelings <- list(
  blue   = c("Sad", "Calm"),
  red    = c("Angry", "Energetic", "Warm"),
  green  = c("Calm", "Nature"),
  yellow = c("Happy", "Warm", "Sunny")
)

map(.x = colour_feelings, .f = nchar)
#-------------------------------

dat <- got_chars
class(dat) #list
#below 3 all do same thing
map(dat, "name")
map(dat, pluck("name"))
map(dat, 3)


### DF map

#create df from list
dat_m <- dat %>% {
  tibble(
    name = map_chr(., "name"),
    gender = map_chr(., "gender"),
    culture = map_chr(., "culture"),
    aliases = map(., "aliases"),
    allegiances = map(., "allegiances")
  )}
View(dat_m)
map(dat_m, nchar)


#map custom function
dead_or_alive <- function(x){
  ifelse(x[["alive"]], paste(x[["name"]], "is alive!"),
         paste(x[["name"]], "is dead :("))
}
map_chr(dat, dead_or_alive)


#conditional mapping
blue <- list(
  translation = "gorm",
  feelings    = c("Sad", "Calm"),
  primary     = "Yes",
  wavelength  = 470
)

map_if(.x = blue, .p = is.character, .f = nchar)

#keep() and discard() based on logical function


#if function wont work ( round() wont work on character returns "not a number")
map(blue, possibly(round, "Not a number"))

#-------------->>>>>>>>>>>>>>>-------------->>>>>>>>>>.

# Text Mining
library(tidytext)

phrases <- 
  c(
    "here is some text",
    "again more text",
    "text is text"
  )

example_text <- tibble(
  phrase = phrases, id = 1:3)

words_df <- 
  example_text %>%
  unnest_tokens(word, phrase) #unnest_token splits string in tokens

#count words within variable
words_df %>%
  group_by(word, id) %>%
  summarise(
    count = n()
  )

#count word within doc and removed stop words
words_df %>%  
  anti_join(stop_words) %>%
  count(word, sort = TRUE)