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
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(stringr)
    library(rvest)
    library(rcrossref)
    library(BiocInstaller)
    library(docopt)
    library(pbapply)
})

#### FUNCTIONS ####

get_swsheet <- function() {

    message("Loading 'single_cell_software.csv'...")
    swsheet <- read_csv("single_cell_software.csv",
                        col_types = cols(
                            .default = col_logical(),
                            Name = col_character(),
                            Platform = col_character(),
                            DOI = col_character(),
                            PubDate = col_character(),
                            Code = col_character(),
                            Description = col_character(),
                            License = col_character(),
                            Added = col_date(format = ""),
                            Updated = col_date(format = "")
                        ))
}

get_pkgs <- function() {

    message("Getting package repositories...")

    message("Getting Bioconductor package list...")
    bioc.pkgs <- all_group()
    names(bioc.pkgs) <- str_to_lower(bioc.pkgs)

    message("Getting CRAN package list...")
    cran.url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
    cran.pkgs <- read_html(cran.url) %>%
        html_nodes("a") %>%
        html_text() %>%
        setdiff(LETTERS) # Remove letter links at top of page
    names(cran.pkgs) <- str_to_lower(cran.pkgs)

    message("Getting PyPI package list...")
    pypi.pkgs <- read_html("https://pypi.python.org/simple/") %>%
        html_nodes("a") %>%
        html_text()
    names(pypi.pkgs) <- str_to_lower(pypi.pkgs)

    pkgs <- list(BioC = bioc.pkgs, CRAN = cran.pkgs, PyPI = pypi.pkgs)
}

fix_doi <- function(swsheet) {

    message("Fixing references...")

    swsheet %>%
        mutate(Preprint = (PubDate == "PREPRINT")) %>%
        mutate(PubDate = ifelse(Preprint == FALSE, PubDate, NA)) %>%
        mutate(PubDate = as_date(PubDate)) %>%
        mutate(Preprint = ifelse(Preprint == TRUE, TRUE, NA)) %>%
        mutate(DOIURL = ifelse(is.na(DOI), NA,
                               paste0('http://dx.doi.org/', DOI)))
}

add_github <- function(swsheet) {

    message("Adding Github...")

    swsheet %>%
        mutate(Github = ifelse(str_detect(Code, "github"),
                               str_replace(Code, "https://github.com/", ""),
                               NA))
}

add_repos <- function(swsheet, pkgs) {

    message("Adding package repositories...")

    swsheet %>%
        mutate(LowerName = str_to_lower(Name)) %>%
        mutate(BioC = LowerName %in% names(pkgs$BioC)) %>%
        mutate(BioC = ifelse(BioC, pkgs$BioC[LowerName], NA)) %>%
        mutate(BioC = ifelse(str_detect(Platform, "R"), BioC, NA)) %>%
        mutate(CRAN = LowerName %in% names(pkgs$CRAN)) %>%
        mutate(CRAN = ifelse(CRAN, pkgs$CRAN[LowerName], NA)) %>%
        mutate(CRAN = ifelse(str_detect(Platform, "R"), CRAN, NA)) %>%
        mutate(PyPI = LowerName %in% names(pkgs$PyPI)) %>%
        mutate(PyPI = ifelse(PyPI, pkgs$PyPI[LowerName], NA)) %>%
        mutate(PyPI = ifelse(str_detect(str_to_lower(Platform), "python"),
                             PyPI, NA)) %>%
        select(-LowerName)
}

add_citations <- function(swsheet) {

    message("Adding citations...")

    dois <- swsheet$DOI

    cites <- pbsapply(dois, function(doi) {
            if (is.na(doi)) {
                return(NA)
            }

            cite <-  tryCatch({
                cr_citation_count(doi)
            }, error = function(e) {
                NA
            })

            Sys.sleep(sample(seq(0,2,0.5), 1))

            return(cite)
    })

    swsheet$Citations <- cites

    return(swsheet)
}

tidy_swsheet <- function(swsheet) {

    message("Tidying data...")

    gather(swsheet, key = 'Category', value = 'Val',
           -Description, -Name, -Platform, -DOI, -PubDate, -Updated, -Added,
           -Preprint, -Code, -Github, -DOIURL, -License, -BioC, -CRAN, -PyPI,
           -Citations) %>%
        filter(Val == TRUE) %>%
        select(-Val) %>%
        arrange(Name)
}

add_cats <- function(swsheet, tidysw) {

    message("Adding categories to table...")

    catlist <- split(tidysw$Category, f = tidysw$Name)

    catdf <- data.frame(Name = names(catlist), stringsAsFactors = FALSE)
    catdf[['categories']] <- catlist

    swsheet <- left_join(swsheet, catdf, by = "Name")
}

get_tools_json <- function(tidysw) {

    message("Converting tools...")

    catlist <- split(tidysw$Category, f = tidysw$Name)

    tools <- tidysw %>%
        select(-Category) %>%
        unique() %>%
        mutate(Categories = catlist[Name]) %>%
        toJSON(pretty = TRUE)
}

get_cats_json <- function(tidysw, swsheet) {

    message("Converting categories...")

    namelist <- split(tidysw$Name, f = tidysw$Category)
    namelist <- lapply(namelist, function(x) {
        swsheet %>%
            filter(Name %in% x) %>%
            select(Name, BioC, CRAN, PyPI)
    })

    cats <- tidysw %>%
        select(Category) %>%
        arrange(Category) %>%
        unique() %>%
        mutate(Tools = namelist[Category]) %>%
        toJSON(pretty = TRUE)
}

process_csv <- function() {

    message("Starting processing...")

    # Load data
    swsheet <- get_swsheet()
    pkgs <- get_pkgs()

    # Process table
    message("Processing table...")
    swsheet <- swsheet %>%
        fix_doi() %>%
        add_github() %>%
        add_repos(pkgs) %>%
        add_citations()

    # Convert to tidy format
    tidysw <- tidy_swsheet(swsheet)

    # Add categories to table
    swsheet <- add_cats(swsheet, tidysw)

    # Convert to JSON
    message("Converting to JSON...")
    message("Converting table...")
    table <- toJSON(swsheet, pretty = TRUE)
    tools <- get_tools_json(tidysw)
    cats <- get_cats_json(tidysw, swsheet)

    # Output JSON
    message("Writing JSON...")
    message("Writing 'tools-table.json'...")
    write_lines(table, "docs/data/tools-table.json")
    message("Writing 'tools.json'...")
    write_lines(tools, "docs/data/tools.json")
    message("Writing 'categories.json'...")
    write_lines(cats, "docs/data/categories.json")

    message("Done!")
}

#### MAIN CODE ####

# Setup progress bar
pboptions(type = "timer", char = "=", style = 3)

# Get options
opts <- docopt(doc)

# Process table
process_csv()
