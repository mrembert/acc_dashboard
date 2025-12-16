# ACC Dashboard

The ACC Dashboard is an interactive data visualization tool designed to display demographic, broadband, and social vulnerability data for communities served by the ACC.

## Project Structure

*   **`acc_dashboard.qmd`**: The primary Quarto document that defines the Shiny dashboard.
*   **`acc_data_setup.qmd`**: The Data Extraction, Transformation, and Load (ETL) script. This is run infrequently (e.g., annually) to update the static datasets used by the dashboard.
*   **`R/dashboard_helpers.R`**: A library of helper functions for data processing and calculation within the dashboard.
*   **`code/fcc_api_functions.R`**: Functions for interacting with the FCC Broadband Map API.
*   **`data/`**: Directory containing the processed `.rda` files used by the dashboard.

## Setup

1.  **Dependencies**: Ensure you have R installed along with the required packages:
    *   `tidyverse`, `shiny`, `quarto`, `leaflet`, `plotly`, `bslib`, `bsicons`, `rairtable`, `tigris`, `tidycensus`, `sf`, `httr`, `jsonlite`.
2.  **Environment Variables**: The project requires several API keys to be set in your `.Renviron` file:
    *   `CENSUS_API_KEY`: For `tidycensus`.
    *   `FCC_USERNAME` & `FCC_API_KEY`: For FCC Broadband Map API access.
    *   `AIRTABLE_PAT`: Personal Access Token for Airtable.

## Running the Dashboard

To run the dashboard locally:

```bash
quarto preview acc_dashboard.qmd
```

## Data Update Workflow

The dashboard relies on pre-processed data stored in `data/final_df.rda`. To update this data (e.g., when new Census or FCC data is released):

1.  Open `acc_data_setup.qmd`.
2.  Update any year variables (e.g., `current_acs`) if necessary.
3.  Run the chunks in the document to fetch new data and regenerate the artifacts in `data/`.
