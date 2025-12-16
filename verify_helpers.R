suppressPackageStartupMessages(library(tidyverse))
source("R/dashboard_helpers.R")

# Create dummy data
df_dummy <- tibble(
    total_pop = c(100, 200, 300, 400, 1000)
)

print("Testing calc_bins...")
tryCatch(
    {
        bins <- calc_bins(df_dummy)
        print(paste("Bins:", bins))
    },
    error = function(e) {
        print(paste("Error in calc_bins:", e$message))
    }
)

print("Testing create_race_vars...")
df_race <- tibble(
    white_combo = 10, black_combo = 10, aian_combo = 10, nhpi_combo = 10, hispanic = 10, asian_combo = 10,
    total_pop = 100
)
tryCatch(
    {
        res <- create_race_vars(df_race)
        print("create_race_vars passed")
    },
    error = function(e) {
        print(paste("Error in create_race_vars:", e$message))
    }
)

print("Sourcing and testing complete.")
