source("code/acs_helpers.R")

# Variables from acc_data_setup.qmd
vars_current <- c(
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

print("Looking up variables for 2024 ACS 1-year...")
codes <- as.character(vars_current)

tryCatch(
    {
        results <- lookup_acs_variables(2024, codes, survey = "acs1")

        vars_df <- data.frame(local_name = names(vars_current), name = as.character(vars_current))

        final_check <- vars_df |>
            left_join(results, by = "name")

        print(final_check)
    },
    error = function(e) {
        print(paste("Error:", e$message))
    }
)
