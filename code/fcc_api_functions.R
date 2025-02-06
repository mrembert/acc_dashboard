get_fcc_as_of_dates_list <- function(api_username = Sys.getenv("FCC_USERNAME"), api_key = Sys.getenv("FCC_API_KEY")) {
  base_url <- "https://broadbandmap.fcc.gov/api/public/map/"
  api_url <- paste0(base_url, "listAsOfDates")
  
  response <- httr::GET(url = api_url, 
                        httr::add_headers(username = api_username, hash_value = api_key))
  
  if (httr::http_error(response)) {
    stop("API request failed to get 'as of dates' list. Status code: ", 
         httr::status_code(response), "\nError: ", 
         httr::content(response, as = "text", encoding = "UTF-8"))
  }
  
  dates_list <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  
  return(dates_list$data)
}

get_fcc_availability_file_list <- function(as_of_date, category = NULL, subcategory = NULL, api_username = Sys.getenv("FCC_USERNAME"), api_key = Sys.getenv("FCC_API_KEY")) {
  if (is.null(as_of_date)) {
    stop("Please provide a valid 'as_of_date' in the format YYYY-MM-DD.")
  }
  
  base_url <- "https://broadbandmap.fcc.gov/api/public/map/downloads"
  api_url <- paste0(base_url, "/listAvailabilityData/", as_of_date)
  
  query_params <- list()
  if (!is.null(category)) {
    query_params$category <- category
  }
  if (!is.null(subcategory)) {
    query_params$subcategory <- subcategory
  }
  
  response <- httr::GET(url = api_url, 
                        query = query_params,
                        httr::add_headers(username = api_username, hash_value = api_key))
  
  if (httr::http_error(response)) {
    stop("API request failed to get file list. Status code: ", 
         httr::status_code(response), "\nError: ", 
         httr::content(response, as = "text", encoding = "UTF-8"))
  }
  
  file_list <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  
  return(file_list$data)
}

get_fcc_availability_file <- function(file_id, file_name, api_username = Sys.getenv("FCC_USERNAME"), api_key = Sys.getenv("FCC_API_KEY")) {
  base_url <- "https://broadbandmap.fcc.gov/api/public/map/downloads/downloadFile"
  api_url <- paste0(base_url, "/availability/", file_id, "/1")  # Assuming ESRI Shapefile format
  
  local_file_path <- file.path("source", "bb", paste0(file_name, ".zip")) # The API returns a zip file
  
  response <- httr::GET(
    url = api_url,
    httr::add_headers(username = api_username, hash_value = api_key),
    httr::write_disk(local_file_path, overwrite = TRUE),
    httr::progress()
  )
  
  if (httr::http_error(response)) {
    stop("API request failed to download file. Status code: ", 
         httr::status_code(response), "\nError: ", 
         httr::content(response, as = "text", encoding = "UTF-8"))
  }
  
  # Unzip the downloaded file (you might need to adjust the path)
  unzip(local_file_path, exdir = file.path("source", "bb"))
  
  # You'll need to figure out the exact name of the CSV file inside the zip
  # This is a placeholder; replace with the actual CSV file name
  csv_file_name <- file.path("source", "bb",paste0(file_name,".csv"))
  
  if (length(csv_file_name) == 0) {
    stop("No CSV file found in the downloaded zip archive.")
  }
  
  bb_file <- readr::read_csv(csv_file_name) |> 
    mutate(geography_id = as.character(geography_id))
  
  return(bb_file)
}