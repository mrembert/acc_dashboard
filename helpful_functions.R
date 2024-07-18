assign_region_area_type <- function(df){

region_summary <- df %>%
  group_by(ID, Host, area_type) %>%
  summarize(
    total_pop = sum(total_pop, na.rm = TRUE),
    # urban_core_pop = sum(total_pop[area_type == "Urban Core"], na.rm = TRUE),
    # suburban_pop = sum(total_pop[area_type == "Suburban"], na.rm = TRUE),
    # rural_pop = sum(total_pop[area_type == "Rural"], na.rm = TRUE)
  ) |> 
  filter(total_pop == max(total_pop)) 

# Determine the majority area_type based on population
region_summary <- region_summary %>%
  mutate(majority_area_type = case_when(
    urban_core_pop == max(urban_core_pop, suburban_pop, rural_pop) ~ "Urban Core",
    suburban_pop == max(urban_core_pop, suburban_pop, rural_pop) ~ "Suburban",
    rural_pop == max(urban_core_pop, suburban_pop, rural_pop) ~ "Rural",
    TRUE ~ "Tie"  # In case of a tie between two or more area_types
  ))

# Merge the majority_area_type back to the original data
acc_site_data <- left_join(acc_site_data, region_summary[, c("GEOGROUP", "majority_area_type")], by = "GEOGROUP")

# Replace original area_type with the majority area_type
acc_site_data <- acc_site_data %>%
  mutate(area_type = ifelse(!is.na(majority_area_type), majority_area_type, area_type)) %>%
  select(-majority_area_type)

}

df <- acc_site_data
buckeye <- acc_site_data |> 
  filter(str_detect(Host, "Buckeye")) |>
  select(Host, NAME, area_type, total_pop) 
