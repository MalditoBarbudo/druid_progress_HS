################################################################################
# read data function
# Using googlesheets package. I want to read automatically the data from a google
# spreadsheet, not public. So I need the token stored in the rds file to access
# the spreadsheet. After that we retrieve the data. All of this must be included
# in a reading function to include in reactiveFileReader.

require(googlesheets)

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
  aggro_data
}

################################################################################
# 
