library(tidyverse)
library(googlesheets4)
library(googledrive)

geocodes <- read.csv("source/all-geocodes-v2020.csv")
place_geocodes_data <- read.csv("source/place_cdp_geocodes.csv")

states <- tribble(
  ~state_fips, ~state_name, ~state_abr,
  "01", "Alabama", "AL",
  "02", "Alaska", "AK",
  "04", "Arizona", "AZ",
  "05", "Arkansas", "AR",
  "06", "California", "CA",
  "08", "Colorado", "CO",
  "09", "Connecticut", "CT",
  "10", "Delaware", "DE",
  "11", "District of Columbia", "DC",
  "12", "Florida", "FL",
  "13", "Georgia", "GA",
  "15", "Hawaii", "HI",
  "16", "Idaho", "ID",
  "17", "Illinois", "IL",
  "18", "Indiana", "IN",
  "19", "Iowa", "IA",
  "20", "Kansas", "KS",
  "21", "Kentucky", "KY",
  "22", "Louisiana", "LA",
  "23", "Maine", "ME",
  "24", "Maryland", "MD",
  "25", "Massachusetts", "MA",
  "26", "Michigan", "MI",
  "27", "Minnesota", "MN",
  "28", "Mississippi", "MS",
  "29", "Missouri", "MO",
  "30", "Montana", "MT",
  "31", "Nebraska", "NE",
  "32", "Nevada", "NV",
  "33", "New Hampshire", "NH",
  "34", "New Jersey", "NJ",
  "35", "New Mexico", "NM",
  "36", "New York", "NY",
  "37", "North Carolina", "NC",
  "38", "North Dakota", "ND",
  "39", "Ohio", "OH",
  "40", "Oklahoma", "OK",
  "41", "Oregon", "OR",
  "42", "Pennsylvania", "PA",
  "44", "Rhode Island", "RI",
  "45", "South Carolina", "SC",
  "46", "South Dakota", "SD",
  "47", "Tennessee", "TN",
  "48", "Texas", "TX",
  "49", "Utah", "UT",
  "50", "Vermont", "VT",
  "51", "Virginia", "VA",
  "53", "Washington", "WA",
  "54", "West Virginia", "WV",
  "55", "Wisconsin", "WI",
  "56", "Wyoming", "WY"
)

ne_states <- c("09","25","23","33","44","50")


  
   

county_geocodes <- geocodes |> 
  left_join(states |> mutate(state_fips = as.integer(state_fips)),
            by = c("state" = "state_fips")) |> 
  mutate(area_name = paste0(area_name, ', ', state_abr)) |>
  mutate(state = sprintf("%0*d", 2, state),
         county = sprintf("%0*d", 3, county)) |>
  mutate(geoid = paste0(state,county)) |> 
  filter(level == 50) |> 
  select(state, area_name:geoid, level)

state_geocodes <- geocodes |> 
  filter(level == 40) |> 
  left_join(states |> mutate(state_fips = as.integer(state_fips)),
            by = c("state" = "state_fips")) |> 
  mutate(state = sprintf("%0*d", 2, state)) |> 
  mutate(geoid = state) |> 
  select(state, area_name:geoid, level)

place_geocodes <- place_geocodes_data |>  
  left_join(states |> mutate(state_fips),
            by = c("state" = "state_fips")) |> 
  filter(state != "State code") |> 
  mutate(geoid = paste0(state,place)) |> 
  rename(area_name = PlaceName) |> 
  mutate(level = 162) |> 
  select(names(state_geocodes))

geocodes_df <- rbind(place_geocodes, county_geocodes, state_geocodes) |> 
  filter(state <= 56) |> 
  mutate(level = case_when(
    level == 61 ~ "County Subdivision",
    level == 162 ~ "Place",
    level == 50 ~ "County",
    level == 40 ~ "State"
  ))  |> 
  select(area_name:geoid, level)

acc_sheet <- "https://docs.google.com/spreadsheets/d/1qabZ_5z6RTo5PiEo1aIaPNFfPLS_cC60tCXX4trHUDw/edit#gid=1844667837"
write_sheet(geocodes_df, acc_sheet, sheet = "geocodes")

