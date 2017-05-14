################################################################################
# read data function
# Using googlesheets package. I want to read automatically the data from a google
# spreadsheet, not public. So I need the token stored in the rds file to access
# the spreadsheet. After that we retrieve the data. All of this must be included
# in a reading function to include in reactiveFileReader.

require(googlesheets)
require(tidyverse)

read_gs_data <- function(path){
  # get access by stored token
  suppressMessages(
    gs_auth(token = file.path(path, "googlesheets_token.rds"),
            verbose = FALSE)
  )
  
  # get data
  aggro_data <- gs_title('aggro_druid') %>%
    gs_read()
  
  # return data
  aggro_data %>%
    mutate(Class = get_class_from_archetype(Opponent))
}

################################################################################
# Function to get the basic class from the archetype
# I'm going to use stringr to separate the words in the archetype and match the
# basic class against a list. The result must be the basic class

require(stringr)

get_class_from_archetype <- function(arch) {
  pattern <- "Warrior|Shaman|Rogue|Paladin|Hunter|Druid|Warlock|Mage|Priest"
  basic_classes <- str_extract(arch, pattern)
  return(basic_classes)
}


