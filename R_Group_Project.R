library(rvest)
library(tidyverse)
library(stringr)
library(tidytext)

getLines <- function(person){
  text <- text_debate1 
  id <- unlist(stringr::str_extract_all(text, "[A-Z]+:")) # get the speaker
  Lines <- unlist(strsplit(text, "[A-Z]+:"))[-1]  # split by speaker (and get rid of a pesky empty line)
  Lines[id %in% person] # retain speech by relevant speaker
}

debate_lines <- lapply(c("CLINTON:", "TRUMP:"), getLines)

#trying to break whole text into individual words
library(tidytext)
library(tokenizers)

text_debate1 <- read_html("http://www.presidency.ucsb.edu/ws/index.php?pid=118971") # load the first debate page
text_debate2 <- read_html("http://www.presidency.ucsb.edu/ws/index.php?pid=119038")  # load the second debate page
text_debate3 <- read_html("http://www.presidency.ucsb.edu/ws/index.php?pid=119039")# load the third debate page 

text_debate1 <- html_nodes(text_debate1, ".displaytext") %>% # isloate the text
 html_text()

text_debate1 <- html_nodes(text_debate1, ".displaytext") %>% # isloate the text
  html_text() # get the text

html_nodes(text_debate2, ".displaytext") %>% # isloate the text
  html_text() # get the text

html_nodes(text_debate3, ".displaytext") %>% # isloate the text
html_text() # get the text

getLines <- function(person){
  text <- text_debate1 
  id <- unlist(stringr::str_extract_all(text, "[A-Z]+:")) # get the speaker
  Lines <- unlist(strsplit(text, "[A-Z]+:"))[-1]  # split by speaker (and get rid of a pesky empty line)
  Lines[id %in% person] # retain speech by relevant speaker
}

debate_lines <- lapply(c("CLINTON:", "TRUMP:"), getLines)
clinton_lines <- debate_lines[1]
trump_lines <- debate_lines[2]

library(tidytext)
library(tokenizers)

#break into clinton lines
library(dplyr)
text_clinton <- data_frame(text_c = clinton_lines[[1]])
text_clinton


#break into trump lines
library(dplyr)
text_trump <- data_frame(text_t = trump_lines[[1]])
text_trump


text_df <- data_frame(text_c = clinton_lines[[1]])
text_df

text_debate1 %>%
  unnest_tokens(word, text_c)
text_debate1

clinton_lines <- debate_lines[1]
trump_lines <- debate_lines[2]

library(dplyr)
library(stringr)

text_debate1 %>%
  dplyr::group_by(clinton_lines) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()
text_debate1