# GEMINI.md: AI Collaboration Guide

## 1. Project Overview

* **Primary Goal:** To create an interactive data visualization dashboard using the Shiny dashboard feature in Quarto, written in R code.
* **Core Features:** Interactive plots, dynamic data filtering, and summary data tables.

## 2. Tech Stack

* **Language:** `R`
* **Framework / Runtime:** `Quarto` / `Shiny`
* **Package Manager:** None
* **Key Libraries/Dependencies:**
    * `tidyverse`
    * `shiny`
    * `pins`
    * `plotly`
    * `shinydashboard`
    * `bslib`
    * `bsicons`
    * `DescTools`
    * `leaflet`
    * `DT`
    * `rairtable`
    * `openxlsx`

## 3. Project Structure

* `acc_dashboard.qmd`: The primary Quarto document that renders the Shiny dashboard.
* `acc_data_setup.qmd`: A script for pulling and structuring data, run infrequently (1-2 times per year).
* `data/`: Contains data inputs for the dashboard.
* `code/`: Contains helper functions used by `acc_data_setup.qmd`.

## 4. Key Commands

* `quarto render acc_dashboard.qmd`: Renders the dashboard to HTML.

## 5. Coding Conventions & Style Guide

* **Formatting:** Strictly follow the tidyverse style guide, enforced with the `styler` package.
* **Naming Conventions:** Variables and functions should use `snake_case`. Shiny outputs should use `camelCase`.
* **Library Loading:** All `library()` calls must be at the top of the main script.
* **General Principles:**
    * Avoid using `attach()`. All data manipulation must use `dplyr` verbs.
    * All server script logic must stay in the primary `acc_dashboard.qmd` file. It cannot be moved to a separate `server.R` file.
    * The Airtable connection code must be located in the server section and run every time the dashboard is loaded.

## 6. Current Goals

* **What I'm working on:** Refactoring and documenting the existing code for long-term maintenance.