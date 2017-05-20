################################################################################
# Using googlesheets package. I want to read automatically the data from a google
# spreadsheet, not public. So I need the token stored in the rds file to access
# the spreadsheet. After that we retrieve the data.

require(googlesheets)
require(tidyverse)

# get access by stored token
suppressMessages(
  gs_auth(token = "googlesheets_token.rds",
          verbose = FALSE)
)

################################################################################
# check function for reactive poll
# This function returns the last update timestamp of the google spreadsheet data
check_function <- function(){
  (gs_ls() %>% filter(sheet_title == 'aggro_druid'))$updated
}

################################################################################
# read data function (value function for reactive poll)
# This function read the data

read_gs_data <- function(path, input){
  # get data
  aggro_data <- gs_title('aggro_druid') %>%
    gs_read()
  
  # return data
  aggro_data
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

################################################################################
# ggplot theme
# A basic & minimal theme

require(ggplot2)

theme_hs <- theme_bw() +
  theme(axis.ticks.x = element_line(size = 1, colour = 'black'),
        axis.line = element_line(size = 1, colour = "black"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size = .5, colour = 'grey80', linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = 'grey95'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        text = element_text(family = "Raleway", size = 12))

################################################################################
# extrafont library to access system fonts
require(extrafont)
