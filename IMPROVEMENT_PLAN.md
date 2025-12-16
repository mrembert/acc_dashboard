# Project Improvement Plan for ACC Dashboard

This document outlines the strategy to refactor, document, and improve the maintainability of the ACC Dashboard project.

## 1. Refactor Data Pipeline (Performance)

**Goal:** Improve code organization while preserving real-time data requirements.

*   **CRITICAL CONSTRAINT: Keep Airtable Fetch Live.** The Airtable connection serves a live database feeding the current set of communities. It **must remain in the dashboard runtime (`acc_dashboard.qmd`)**. Do **NOT** move the Airtable data fetching to the offline setup script.
*   **Action:** Only move *static* data processing (like Census/FCC data preparation) to `acc_data_setup.qmd`.
*   **Strategy:** Focus on moving the *functions* and *logic* that process the live data (e.g., `calc_metrics`) into helper files to clean up the dashboard code, without changing *when* the data is fetched.

## 2. Modularize & Clean Code (Structure)

**Goal:** Eliminate code duplication, "spaghetti code," and dangerous global state.

*   **Clean `R/dashboard_helpers.R`:**
    *   Remove top-level execution code (e.g., `load(...)`, `readRenviron(...)`) that runs immediately when the file is sourced.
    *   Remove global assignment operators (`<<-`) which are bad practice and hard to debug.
*   **Centralize Logic:** Move the following large functions from `acc_dashboard.qmd` to `R/dashboard_helpers.R`:
    *   `calc_metrics` (The core aggregation logic)
    *   `remove_duplicates` (Geographic de-duplication)
    *   `assign_region_area_type`
    *   `create_race_vars`
    *   `areas_served`
    *   `calc_bins`
*   **Source Helpers:** The dashboard should simply `source("R/dashboard_helpers.R")` to make these functions available.

## 3. Documentation (Maintainability)

**Goal:** Make the code self-explanatory and easier to maintain for future developers.

*   **Add Docstrings:** Add standard `roxygen2` style comments to all functions in:
    *   `R/dashboard_helpers.R`
    *   `code/fcc_api_functions.R`
*   **Standardize Format:** Each function documentation should include:
    *   **Title:** A brief summary of what the function does.
    *   **@param:** Description of each input argument.
    *   **@return:** Description of what the function returns.

## 4. Testing (Reliability)

**Goal:** Ensure critical business logic is correct and doesn't break during updates.

*   **Create Test Suite:** Initialize a `tests/` directory (using `testthat`).
*   **Unit Tests:** Write specific tests for:
    *   `remove_duplicates`: Verify it correctly prioritizes State > County > Place and handles overlaps.
    *   `calc_metrics`: Verify it calculates weighted averages and sums correctly for a known small dataset.
    *   `assign_region_area_type`: Ensure it picks the area with the max population.

## 5. Cleanup

**Goal:** Remove clutter and confusion.

*   **Archive Legacy Code:** Move unused or replaced scripts (like `code/pull_acc_geographies.R`) to an `archive/` folder.
*   **Remove Duplicates:** Ensure functions are defined in only *one* place (the helper file) and not redefined in the dashboard.