library(tidycensus)
library(dplyr)
library(stringr)
library(purrr)

#' Lookup ACS Variables
#'
#' Retrieves metadata (Label, Concept) for a given list of ACS variable codes.
#'
#' @param year Integer, the ACS year (e.g., 2023).
#' @param codes Character vector, the variable codes to look up.
#' @param survey Character, the ACS survey to use ("acs5" or "acs1"). Defaults to "acs5".
#' @return A dataframe with the variable code and its metadata.
lookup_acs_variables <- function(year, codes, survey = "acs5") {
    # Datasets to check
    datasets <- c(survey, paste0(survey, "/profile"), paste0(survey, "/subject"))

    # Load variables for all datasets
    all_vars <- map_dfr(datasets, function(ds) {
        tryCatch(
            {
                load_variables(year, ds, cache = TRUE) |> mutate(dataset = ds)
            },
            error = function(e) {
                # warning(paste("Could not load variables for dataset:", ds))
                return(NULL)
            }
        )
    })

    if (nrow(all_vars) == 0) {
        stop(paste("No variables found for year", year, "and survey", survey))
    }

    # Filter for the requested codes
    matched_vars <- all_vars |>
        filter(name %in% codes) |>
        select(name, label, concept, dataset) |>
        distinct(name, .keep_all = TRUE)

    return(matched_vars)
}

#' Search ACS Variables
#'
#' Searches for ACS variables by keyword.
#'
#' @param year Integer, the ACS year.
#' @param keyword Character, the search term.
#' @param survey Character, the ACS survey ("acs5" or "acs1").
#' @return A dataframe of matching variables.
search_acs_variables <- function(year, keyword, survey = "acs5") {
    datasets <- c(survey, paste0(survey, "/profile"), paste0(survey, "/subject"))

    all_vars <- map_dfr(datasets, function(ds) {
        tryCatch(
            {
                load_variables(year, ds, cache = TRUE) |> mutate(dataset = ds)
            },
            error = function(e) {
                return(NULL)
            }
        )
    })

    all_vars |>
        filter(str_detect(label, regex(keyword, ignore_case = TRUE)) |
            str_detect(concept, regex(keyword, ignore_case = TRUE)))
}

#' Fetch ACC Dashboard Data
#'
#' Validates variables and fetches ACS data for Place, County, State, and US levels.
#' Combines them into a single wide-format dataframe ready for the dashboard.
#'
#' @param year Integer, the ACS year.
#' @param variables Named character vector of variable codes.
#' @param survey Character, "acs5" or "acs1".
#' @return A dataframe with GEOID and variables in wide format.
fetch_acc_data <- function(year, variables, survey = "acs5") {
    # 1. Validate Variables
    message("Validating variables...")
    tryCatch(
        {
            lookup_acs_variables(year, as.character(variables), survey = survey)
        },
        error = function(e) {
            stop("Variable validation failed. Check your variable codes for the specified year/survey.")
        }
    )

    geographies <- c("place", "county", "state", "us")

    # 2. Fetch Data for each geography
    results <- map_dfr(geographies, function(geo) {
        message(paste("Fetching data for:", geo))

        df <- get_acs(
            geography = geo,
            variables = variables,
            year = year,
            survey = survey,
            cache_table = TRUE
        )

        # Standardize US GEOID
        if (geo == "us") {
            df <- df |> mutate(GEOID = "00")
        }

        return(df)
    })

    # 3. Process and Pivot
    message("Processing data...")
    final_data <- results |>
        select(-moe, -NAME) |>
        pivot_wider(names_from = variable, values_from = estimate) |>
        mutate(acs_year = paste0(year))

    return(final_data)
}

#' Get ACC Dashboard Variables
#'
#' Returns the standardized list of ACS variable codes used by the dashboard.
#'
#' @param year Integer, the ACS year. (Currently returns the same set for all recent years).
#' @return A named character vector of variable codes.
get_acc_vars <- function(year) {
    # Logic can be added here to return different codes for different years if mappings change.

    c(
        total_pop = "DP05_0001",
        pop_65_older = "DP05_0024",
        white_nh = "DP05_0082",
        black_nh = "DP05_0083",
        aian_nh = "DP05_0084",
        asian_nh = "DP05_0085",
        nhpi_nh = "DP05_0086",
        other_race_nh = "DP05_0087",
        hispanic = "DP05_0076",
        white_combo = "DP05_0069",
        black_combo = "DP05_0070",
        aian_combo = "DP05_0071",
        asian_combo = "DP05_0072",
        nhpi_combo = "DP05_0073",
        other_race_combo = "DP05_0074",
        multiracial = "DP05_0061",
        pop_25_older = "S1501_C01_006",
        bach_plus = "S1501_C01_015",
        median_earnings = "S2001_C01_013",
        median_income = "S1903_C03_001",
        poverty = "S1701_C03_001",
        pop_16_older = "S2301_C01_001",
        emp_pop_16_older = "S2301_C03_001",
        pop_25_29 = "S2301_C01_004",
        pop_30_34 = "S2301_C01_005",
        pop_35_44 = "S2301_C01_006",
        pop_45_54 = "S2301_C01_007",
        emp_pop_25_29 = "S2301_C03_004",
        emp_pop_30_34 = "S2301_C03_005",
        emp_pop_35_44 = "S2301_C03_006",
        emp_pop_45_54 = "S2301_C03_007",
        total_households = "S2801_C01_001",
        hh_w_computer = "S2801_C01_003",
        hh_w_bb = "S2801_C01_017",
        hh_less_20k_inc = "S2801_C01_020",
        hh_less_20k_inc_bb = "S2801_C01_022",
        inc_less_10k = "S1901_C01_002",
        inc_10k_15k = "S1901_C01_003",
        inc_15k_25k = "S1901_C01_004",
        inc_25k_35k = "S1901_C01_005",
        inc_35k_50k = "S1901_C01_006",
        inc_50k_75k = "S1901_C01_007"
    )
}
