test_that("remove_duplicates prioritizes State level correctly", {
    df <- tibble(
        geogroup = c("State", "Local"),
        STATE = c("MA", "MA"),
        FIPS = c("25", "25025"),
        GEOLEVEL = c("State", "County")
    )

    result <- remove_duplicates(df)
    # Logic check: if State is present, it might process differently based on the group logic.
    # The function splits by 'includes_state'. If State is present, it looks for it.

    # Let's create a more specific scenario based on the logic:
    # If "State" is in geogroup options for the DATAFRAME (it checks `if("State" %in% df$geogroup)`),
    # then it prioritizes rows where geogroup == "State".

    # Case 1: State is present
    df_state <- tibble(
        geogroup = c("State", "Other"),
        STATE = c("MA", "MA"),
        FIPS = c("1", "1"), # Same FIPS or grouping key might matter, but code groups by STATE first?
        # Actually the code does: group_by(STATE) -> mutate(includes_state...)
        GEOLEVEL = c("State", "County")
    )

    res <- remove_duplicates(df_state)
    expect_equal(nrow(res), 1)
    expect_equal(res$geogroup, "State")
})

test_that("remove_duplicates resolves overlaps", {
    # Logic: filter(n() > 1 & ("Place" %in% GEOLEVEL) & ("County" %in% GEOLEVEL)) -> filter(GEOLEVEL == "County")

    df_overlap <- tibble(
        geogroup = c("Local", "Local"),
        FIPS = c("123", "123"),
        GEOLEVEL = c("Place", "County"),
        STATE = c("TX", "TX") # Needed if State logic triggers, but let's assume no "State" in geogroup col
    )

    res <- remove_duplicates(df_overlap)
    expect_equal(nrow(res), 1)
    expect_equal(res$GEOLEVEL, "County")
})

test_that("assign_region_area_type picks max population", {
    df <- tibble(
        ID = c(1, 1),
        host = c("H1", "H1"),
        area_type = c("TypeA", "TypeA"),
        total_pop = c(100, 200)
    )

    res <- assign_region_area_type(df)
    expect_equal(nrow(res), 1)
    expect_equal(res$total_pop, NULL) # It selects ID, host, area_type only
    # Wait, the function selects ID, host, area_type. It effectively dedups by picking the one from the max pop row.
    # But since it SELECTS only those cols, and Group By was ID, host, area_type...
    # If input had multiple rows for same ID/host/area_type with diff pops, it picks the max one.
    # But the output is just the key columns?
    # Let's check the function:
    # summarize(total_pop = sum(...)) -> This AGGREGATES first by ID/host/area_type.
    # So duplicates per group are SUMMED.
    # filter(total_pop == max(total_pop)) -> This filters ACROSS the groups?
    # No, the pipe is: df |> group_by(...) |> summarize(...) |> filter(...)
    # If summarize returns one row per group, filter(total_pop == max(total_pop)) keeps only the group(s) with the highest pop in the entire DF?
    # That seems like it finds the LARGEST area_type group?

    # Let's re-read the function carefully.
    # assign_region_area_type <- function(df){
    #   df |>
    #     group_by(ID, host, area_type) %>%
    #     summarize(
    #       total_pop = sum(total_pop, na.rm = TRUE)) |>
    #     filter(total_pop == max(total_pop)) |>
    #     select(ID, host, area_type)
    # }

    # Yes, it finds the ID/Host/AreaType combination that has the GLOBAL maximum population in the provided DF.
    # Or is it meant to work per ID? It's NOT grouped after summarize.
    # If it's meant to work per ID, it should be grouped by ID before filter.
    # But the code provided was: `summarize(...) |> filter(...)`. Summarize drops groups by one level (area_type).
    # So it's grouped by ID, host.
    # Then `filter(total_pop == max(total_pop))` acts within the (ID, host) group.
    # So for each host/ID, it picks the area_type with the largest population.

    df_multi <- tibble(
        ID = c(1, 1),
        host = c("H1", "H1"),
        area_type = c("Small", "Large"),
        total_pop = c(100, 500)
    )

    res <- assign_region_area_type(df_multi)
    expect_equal(nrow(res), 1)
    expect_equal(res$area_type, "Large")
})

test_that("calc_metrics calculates weighted means correctly", {
    # Simple test for poverty share (weighted mean)
    df <- tibble(
        group_col = c("A", "A"),
        poverty = c(10, 20),
        total_pop = c(100, 300),
        median_income = c(50000, 100000),
        # Add dummy cols for other calcs to avoid errors if function expects them
        aian_combo = 0, black_combo = 0, hispanic = 0, nhpi_combo = 0, white_combo = 0, asian_combo = 0,
        pop_65_older = 0, bach_plus = 0, pop_25_older = 1,
        geogroup = "Local", svi_level = "low",
        total_households = 10, hh_w_bb = 5, hh_w_computer = 5,
        total_units = 10, pct_served_25_3 = 0.5, pct_served_100_20 = 0.5, pct_served_1000_100 = 0.5,
        inc_less_10k = 0, inc_10k_15k = 0, inc_15k_25k = 0, inc_25k_35k = 0, inc_35k_50k = 0, inc_50k_75k = 0
    )

    # weighted mean of poverty: (10*100 + 20*300) / (100+300) = (1000 + 6000)/400 = 7000/400 = 17.5
    # The function divides poverty rate by 100?
    # Code: weighted.mean(poverty, total_pop) / 100
    # So (17.5)/100 = 0.175

    res <- calc_metrics(df, group_col)

    # Check structure
    expect_true("poverty_rate" %in% names(res))
    expect_equal(res$poverty_rate, 0.175)
})
