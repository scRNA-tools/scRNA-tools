#!/usr/bin/env Rscript

"Usage: process_csv

This utility script converts the 'single_cell_software.csv' spreadsheet to a set
of files in 'docs/data' required for the scRNA-tools.org website including:

- software.json
- software-table.json
- categories.json
" -> doc

#### PACKAGES ####

suppressPackageStartupMessages({
     library(readr)
     library(jsonlite)
     library(docopt)
     library(pbapply)
     library(magrittr)
})

#### SOURCE ####

source("R/json.R")
source("R/load.R")
source("R/manipulation.R")
source("R/misc.R")
source("R/plotting.R")
source("R/references.R")
source("R/repositories.R")
source("R/titles.R")

#### FUNCTIONS ####

#' Process CSV
#'
#' Process `single_cell_software.csv` and create the various output files
process_csv <- function() {

    message("Starting processing...")

    # Load data
    swsheet <- get_swsheet()
    pkgs <- get_pkgs()
    descs <- get_descriptions()
    titles_cache <- get_cached_titles()

    # Add new titles
    titles_cache <- add_to_titles_cache(swsheet, titles_cache)

    # Check repositories
    repos <- check_repos(swsheet$Name, pkgs)

    # Process table
    message("Processing table...")
    swsheet <- swsheet %>%
        add_refs(titles_cache) %>%
        add_github() %>%
        add_repos(repos)

    # Get shields
    get_shields(swsheet)

    # Convert to tidy format
    tidysw <- tidy_swsheet(swsheet)

    # Add categories to table
    swsheet <- add_cats(swsheet, tidysw)

    # Convert to JSON
    message("Converting to JSON...")
    message("Converting table...")
    table <- toJSON(swsheet, pretty = TRUE)
    tools <- make_tools_json(tidysw)
    cats <- make_cats_json(tidysw, swsheet, descs)

    # Output JSON
    message("Writing JSON...")
    message("Writing 'tools-table.json'...")
    write_lines(table, "docs/data/tools-table.json")
    message("Writing 'tools.json'...")
    write_lines(tools, "docs/data/tools.json")
    message("Writing 'categories.json'...")
    write_lines(cats, "docs/data/categories.json")

    # Make plots
    message("Plotting tools over time...")
    plot_number(swsheet)
    message("Plotting publication status...")
    plot_publication(swsheet)
    message("Plotting licenses...")
    plot_licenses(swsheet)
    message("Plotting platforms...")
    plot_platforms(swsheet)
    message("Plotting categories...")
    plot_categories(swsheet)

    write_footer()
    message("Done!")
}


#### MAIN CODE ####

# Show warnings
options(warn = 1)

# Setup progress bar
pboptions(type = "timer", char = "=", style = 3)

# Get options
opts <- docopt(doc)

# Process table
process_csv()
