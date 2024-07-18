get_fcc_as_of_dates_list <- function(api_username = Sys.getenv('FCC_USERNAME'), api_key=Sys.getenv("FCC_API_KEY")) {
  # Base URL
  base_url <- "https://broadbandmap.fcc.gov/api/public/map/"
  
  # Construct the API URL
  api_url <- paste0(base_url, "/listAsOfDates")
  
  # API Request 
  response <- httr::GET(url = api_url, 
                        add_headers(username = api_username, hash_value = api_key))
  
  # Error handling
  if (response$status_code != 200) {
    stop("API request failed to get 'as of dates' list. Error: ", httr::content(response, as = "text"))
  }
  
  # Parse the JSON response
  dates_list <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Return as a data frame
  return(as.data.frame(dates_list$data))
}


get_fcc_availability_file_list <- function(as_of_date, category = NULL, subcategory = NULL, api_username = Sys.getenv('FCC_USERNAME'), api_key=Sys.getenv("FCC_API_KEY")) {
  # Validate input
  if (is.null(as_of_date)) {
    stop("Please provide a valid 'as_of_date' in the format YYYY-MM-DD.")
  }
  
  # Base URL
  base_url <- "https://broadbandmap.fcc.gov/api/public/map/downloads"
  
  # Construct the API URL
  api_url <- paste0(base_url, "/listAvailabilityData/", as_of_date)
  
  # API Request 
  response <- httr::GET(url = api_url, 
                        query = list(category = category, subcategory = subcategory),
                        add_headers(username = api_username, hash_value = api_key))
  
  # Error handling
  if (response$status_code != 200) {
    stop("API request failed to get file list. Error: ", httr::content(response, as = "text"))
  }
  
  # Parse the JSON response
  file_list <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Return as a data frame
  return(as.data.frame(file_list$data))
}


get_fcc_availability_file <- function(file_id, file_name, api_username = Sys.getenv('FCC_USERNAME'), api_key=Sys.getenv("FCC_API_KEY")) {
  # Base URL (no change here)
  base_url <- "https://broadbandmap.fcc.gov/api/public/map/downloads/downloadFile"
  
  # Construct API URL (no change here)
  api_url <- paste0(base_url, "/availability/", file_id)
  
  # File name for download and reading
  local_file_path <- file.path("source", "bb", paste0(file_name, ".csv"))  # Use file.path for better path handling
  
  # API Request with direct writing to local file path
  response <- httr::GET(
    url = api_url,
    add_headers(username = api_username, hash_value = api_key),
    write_disk(local_file_path, overwrite = TRUE),  # Write directly to the .csv
    progress()
  )
  
  # Error handling (no change here)
  if (response$status_code != 200) {
    stop("API request failed to get file list. Error: ", httr::content(response, as = "text"))
  }
  
  # Read the downloaded file
  bb_file <- read_csv(local_file_path) |> 
    mutate(geography_id = as.character(geography_id))
  
  return(bb_file)
}
