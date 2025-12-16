#' Assign Region and Area Type
#'
#' Selects the record with the maximum total population for each ID, host, and area_type combination.
#'
#' @param df A dataframe containing ID, host, area_type, and total_pop columns.
#' @return A dataframe with unique ID, host, and area_type.
assign_region_area_type <- function(df) {
  df |>
    group_by(ID, host, area_type) %>%
    summarize(
      total_pop = sum(total_pop, na.rm = TRUE)
    ) |>
    filter(total_pop == max(total_pop)) |>
    select(ID, host, area_type)
}

#' Remove Duplicate Geography Records
#'
#' Handles duplicate geography records by prioritizing State level, then resolving overlaps between County and Place.
#'
#' @param df A dataframe with geogroup, STATE, FIPS, and GEOLEVEL columns.
#' @return A dataframe with duplicates removed.
remove_duplicates <- function(df) {
  if ("State" %in% df$geogroup) {
    df <- df |>
      group_by(STATE) |>
      mutate(includes_state = ifelse(geogroup == "State", 1, 0)) |>
      mutate(includes_state = ifelse(sum(includes_state) >= 1, 1, 0)) |>
      ungroup()

    df_state <- df |>
      filter((includes_state == 1 & geogroup == "State"))


    df_single <- df |>
      filter(includes_state != 1) |>
      group_by(FIPS) |>
      filter(n() == 1 | (n() > 1 & !(("Place" %in% GEOLEVEL) & ("County" %in% GEOLEVEL)))) |>
      ungroup()

    df_dup <- df |>
      filter(includes_state != 1) |>
      group_by(FIPS) %>%
      filter(n() > 1 & ("Place" %in% GEOLEVEL) & ("County" %in% GEOLEVEL)) |>
      filter(GEOLEVEL == "County") |>
      ungroup()

    df <- rbind(df_state, df_single, df_dup) |> select(-includes_state)
  } else {
    df_single <- df |>
      group_by(FIPS) |>
      filter(n() == 1 | (n() > 1 & !(("Place" %in% GEOLEVEL) & ("County" %in% GEOLEVEL)))) |>
      ungroup()

    df_dup <- df |>
      group_by(FIPS) %>%
      filter(n() > 1 & ("Place" %in% GEOLEVEL) & ("County" %in% GEOLEVEL)) |>
      filter(GEOLEVEL == "County") |>
      ungroup()

    df <- rbind(df_single, df_dup)
  }

  return(df)
}

#' Create Race Variables
#'
#' Aggregates race population counts and calculates shares.
#'
#' @param df A dataframe with race count columns (white_combo, black_combo, etc.) and total_pop.
#' @return A dataframe with race shares.
create_race_vars <- function(df) {
  df |>
    summarize(
      White = sum(white_combo) / sum(total_pop),
      "Black & African American" = sum(black_combo) / sum(total_pop),
      "American Indian & Alaska Native" = sum(aian_combo) / sum(total_pop),
      "Native Hawaiian & Pacific Islander" = sum(nhpi_combo) / sum(total_pop),
      Hispanic = sum(hispanic) / sum(total_pop),
      Asian = sum(asian_combo) / sum(total_pop),
    )
}

#' Calculate Histogram Bins
#'
#' Calculates the number of bins for a histogram using the Freedman-Diaconis rule.
#'
#' @param df A dataframe with a total_pop column.
#' @return A numeric value representing the number of bins.
calc_bins <- function(df) {
  bins <- round((max(df$total_pop, na.rm = TRUE) - min(df$total_pop, na.rm = TRUE)) / (2 * IQR(df$total_pop, na.rm = TRUE) / length(df$total_pop)^(1 / 3)))
}

#' Format Areas Served
#'
#' Creates a string listing areas served, adding state abbreviation for counties.
#'
#' @param df A dataframe with GEOLEVEL, NAME, and STATE_NAME columns.
#' @return A dataframe with an added 'areas_served' column.
areas_served <- function(df) {
  df |>
    mutate(areas_served = ifelse(GEOLEVEL == "County", paste0(NAME, ", ", usdata::state2abbr(STATE_NAME), collapse = "; "), paste0(NAME, collapse = "; ")))
}

#' Calculate Dashboard Metrics
#'
#' Aggregates various demographic and broadband metrics by a grouping variable.
#'
#' @param df A dataframe containing the data to aggregate.
#' @param var The grouping variable (unquoted).
#' @return A wide dataframe with aggregated metrics.
calc_metrics <- function(df, var) {
  df_list <- list(
    total_pop = df |> group_by({{ var }}) |> summarize(total_pop = sum(total_pop)),
    aian_share = df |> group_by({{ var }}) |> summarize(aian_share = sum(aian_combo) / sum(total_pop)),
    asian_share = df |> group_by({{ var }}) |> summarize(asian_share = sum(asian_combo) / sum(total_pop)),
    black_share = df |> group_by({{ var }}) |> summarize(black_share = sum(black_combo) / sum(total_pop)),
    hispanic_share = df |> group_by({{ var }}) |> summarize(hispanic_share = sum(hispanic) / sum(total_pop)),
    nhpi_share = df |> group_by({{ var }}) |> summarize(nhpi_share = sum(nhpi_combo) / sum(total_pop)),
    white_share = df |> group_by({{ var }}) |> summarize(white_share = sum(white_combo) / sum(total_pop)),
    pop_65_older_share = df |> group_by({{ var }}) |> summarize(pop_65_older_share = sum(pop_65_older) / sum(total_pop)),
    bach_plus_share = df |> group_by({{ var }}) |> summarize(bach_plus_share = sum(bach_plus) / sum(pop_25_older)),
    svi = {
      # Local and regional SVI
      svi_local_df <- df |>
        filter(geogroup %in% c("Local", "Regional")) |>
        group_by({{ var }}, svi_level) |>
        summarize(svi_pop = sum(total_pop, na.rm = TRUE)) |>
        ungroup()

      svi_state_df <- df |>
        filter(geogroup %in% c("State")) |>
        select({{ var }}, svi_high, svi_med_high, svi_med_low, svi_low) |>
        pivot_longer(cols = starts_with("svi_"), names_to = "svi_level", values_to = "svi_pop_state") |>
        mutate(svi_level = str_replace(svi_level, "svi_", "")) |>
        group_by({{ var }}, svi_level) |>
        summarize(svi_pop_state = sum(svi_pop_state, na.rm = TRUE))

      # Combine and Summarize
      combined <- full_join(svi_local_df, svi_state_df, by = c(as.character(substitute(var)), "svi_level")) |>
        mutate(svi_total = coalesce(svi_pop, 0) + coalesce(svi_pop_state, 0)) |>
        group_by({{ var }}) |>
        mutate(svi_share = svi_total / sum(svi_total, na.rm = TRUE)) |>
        select({{ var }}, svi_level, svi_share, svi_total) |>
        pivot_wider(names_from = c(svi_level), values_from = c(svi_share, svi_total))

      replace(combined, is.na(combined), 0)
    },
    poverty_share = df |> group_by({{ var }}) |> summarize(poverty_rate = weighted.mean(poverty, total_pop) / 100),
    median_income = df |> group_by({{ var }}) |> summarize(median_income = weighted.mean(median_income, total_pop)),
    wo_bb_sub_total = df |> group_by({{ var }}) |> summarize(wo_bb_sub_total = sum(total_households - hh_w_bb) * sum(total_pop) / sum(total_households)),
    wo_bb_sub_share = df |> group_by({{ var }}) |> summarize(wo_bb_sub_share = sum(total_households - hh_w_bb) / sum(total_households)),
    wo_comp_total = df |> group_by({{ var }}) |> summarize(wo_comp_total = sum(total_households - hh_w_computer) * sum(total_pop) / sum(total_households)),
    wo_comp_share = df |> group_by({{ var }}) |> summarize(wo_comp_share = sum(total_households - hh_w_computer) / sum(total_households)),
    access_25_3 = df |> group_by({{ var }}) |>
      mutate(units_25_3 = total_units * pct_served_25_3) |>
      summarize(
        served_25_3_total = sum(units_25_3),
        served_25_3_share = served_25_3_total / sum(total_units),
        unserved_25_3_total = sum((total_pop / total_units) * total_units * (1 - pct_served_25_3))
      ),
    access_100_20 = df |> group_by({{ var }}) |>
      mutate(units_100_20 = total_units * pct_served_100_20) |>
      summarize(
        served_100_20_total = sum(units_100_20),
        served_100_20_share = served_100_20_total / sum(total_units),
        unserved_100_20_total = sum((total_pop / total_units) * total_units * (1 - pct_served_100_20))
      ),
    access_1000_100 = df |> group_by({{ var }}) |>
      mutate(units_1000_100 = total_units * pct_served_1000_100) |>
      summarize(
        served_1000_100_total = sum(units_1000_100),
        served_1000_100_share = served_1000_100_total / sum(total_units),
        unserved_1000_100_total = sum((total_pop / total_units) * total_units * (1 - pct_served_1000_100))
      ),
    bb_not_afford = df |> group_by({{ var }}) |> summarize(
      not_afford_25_total = sum(
        inc_less_10k / 100 * total_households + inc_10k_15k / 100 * total_households
      ),
      not_afford_58_total = sum(
        inc_less_10k / 100 * total_households + inc_10k_15k / 100 * total_households + inc_15k_25k /
          100 * total_households + inc_25k_35k / 100 * total_households
      ),
      not_afford_83_total = sum(
        inc_less_10k / 100 * total_households + inc_10k_15k / 100 * total_households + inc_15k_25k /
          100 * total_households + inc_25k_35k / 100 * total_households + inc_35k_50k /
          100 * total_households
      ),
      not_afford_125_total = sum(
        inc_less_10k / 100 * total_households + inc_10k_15k / 100 * total_households + inc_15k_25k /
          100 * total_households + inc_25k_35k / 100 * total_households + inc_35k_50k /
          100 * total_households + inc_50k_75k / 100 * total_households
      ),
      not_afford_25_share = not_afford_25_total / sum(total_households),
      not_afford_58_share = not_afford_58_total / sum(total_households),
      not_afford_83_share = not_afford_83_total / sum(total_households),
      not_afford_125_share = not_afford_125_total / sum(total_households)
    )
  )

  df <- bind_rows(df_list) |>
    pivot_longer(cols = -{{ var }}, names_to = "metric", values_to = "value") |>
    filter(!is.na(value)) |>
    # Rounding logic
    mutate(value = case_when(
      str_detect(metric, "_share") ~ round(value, 3), # 3 decimal places for share
      str_detect(metric, "_total|_income") ~ round(value, 0), # Whole numbers for total
      TRUE ~ value # Leave other metrics unchanged
    )) |>
    pivot_wider(names_from = metric, values_from = value)


  return(df)
}
