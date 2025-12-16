library(tidyverse)
library(googlesheets4)
library(googledrive)

acc_sheet <- "https://docs.google.com/spreadsheets/d/1qabZ_5z6RTo5PiEo1aIaPNFfPLS_cC60tCXX4trHUDw/edit#gid=1844667837"
acc_geos <- read_sheet(acc_sheet, sheet = "Service Areas") 

acc_geos <- acc_geos |> 
  filter(!is.na(Host))

places <- acc_geos |> 
  filter(GEOLEVEL == "Place")

counties <- acc_geos |> 
  filter(GEOLEVEL == "County")

states <- acc_geos |> 
  filter(GEOLEVEL == "State")

place_codes <- places$GEOID |> 
  unique()

county_codes <- counties$GEOID |> 
  unique()

places_states <- places |> 
  mutate(state_code = substr(GEOID,1,2)) |> 
  pull(state_code) |> 
  unique()

county_states <- counties |> 
  mutate(state_code = substr(GEOID,1,2)) |> 
  pull(state_code)  |> 
  unique()
