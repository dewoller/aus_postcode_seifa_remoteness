## library() calls go here
library(targets)
library(conflicted)
library(dotenv)
library(tidyverse)
library(rmarkdown)
library(janitor)
library(lubridate)

library(sf)
library(s2)
library(sp)

library(readxl)
library(fs)
library(absmapsdata)
library(wrapr)



#library(rvest)
conflict_prefer("pluck", "purrr")

conflicted::conflict_prefer('filter','dplyr')
conflicted::conflict_prefer('summarise','dplyr')


GLOBAL_BASE_DATE_TIME = lubridate::ymd_hms('2020-07-06 10:00:00', tz='Australia/Melbourne')
GLOBAL_BASE_DATE = floor_date(GLOBAL_BASE_DATE_TIME, unit='day' )



