#!/usr/bin/env Rscript
library(docopt)
"Usage: process_csv OUTPUT_DIR

-h --help    show this

This utility script converts the 'single_cell_software.csv'
spreadsheet to a set of files including:

  - data/software.json
" -> doc

opts <- docopt(doc)
print(opts)

library(readr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(rvest)

#' Create tidy sheet from the google sheet
#' @export
get_tidy_sw_list <- function() {
    message("Getting Bioconductor package list...")
    bioc.pkgs <- BiocInstaller::all_group()
    names(bioc.pkgs) <- str_to_lower(bioc.pkgs)

    message("Getting PyPI package list...")
    pypi.pkgs <- read_html("https://pypi.python.org/simple/") %>%
        html_nodes('a') %>%
        html_text()
    names(pypi.pkgs) <- str_to_lower(pypi.pkgs)

    message("Getting CRAN package list...")
    cran.url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
    cran.pkgs <- read_html(cran.url) %>%
        html_nodes('a') %>%
        html_text() %>%
        setdiff(LETTERS) # Remove letter links at top of page
    names(cran.pkgs) <- str_to_lower(cran.pkgs)

    message("Processing table...")
    swsheet <- read_csv("single_cell_software.csv",
                        col_types = cols(
                            .default = col_logical(),
                            Name = col_character(),
                            Platform = col_character(),
                            DOI = col_character(),
                            `Pub Date` = col_character(),
                            Code = col_character(),
                            Description = col_character(),
                            License = col_character(),
                            Added = col_date(format = ""),
                            Updated = col_date(format = "")
                            )) %>%
        rename(PubDate = `Pub Date`) %>%
        mutate(Preprint = (PubDate == "PREPRINT")) %>%
        mutate(PubDate = as_date(PubDate)) %>%
        mutate(Preprint = ifelse(Preprint == TRUE, TRUE, NA)) %>%
        mutate(DOI_url = ifelse(is.na(DOI), NA,
                                paste0('http://dx.doi.org/', DOI))) %>%
        mutate(Bioconductor = str_to_lower(Name) %in% names(bioc.pkgs)) %>%
        mutate(Bioconductor = ifelse(Bioconductor,
                                     bioc.pkgs[str_to_lower(Name)], NA)) %>%
        mutate(CRAN = str_to_lower(Name) %in% names(cran.pkgs)) %>%
        mutate(CRAN = ifelse(CRAN, cran.pkgs[str_to_lower(Name)], NA)) %>%
        mutate(CRAN = ifelse(str_detect(Platform, "R"), CRAN, NA)) %>%
        mutate(pypi = str_to_lower(Name) %in% names(pypi.pkgs)) %>%
        mutate(pypi = ifelse(pypi, pypi.pkgs[str_to_lower(Name)], NA)) %>%
        mutate(pypi = ifelse(str_detect(str_to_lower(Platform), "python"),
                             pypi, NA))

    message("Tidying data...")
    gather(swsheet, key = 'category', value = 'val',
           -Description, -Name, -Platform, -DOI, -PubDate, -Updated, -Added,
           -Preprint, -Code, -DOI_url, -License, -Bioconductor, -pypi, -CRAN) %>%
    #mutate(Github = grepl('github', Code)) %>%
    #mutate(CRAN = grepl('cran\\.r-project', Code)) %>%
        filter(val == TRUE) %>%
        select(-val) %>%
        arrange(Name)
}

tidysw_to_list_df <- function(tidysw) {
  catlist <- split(tidysw$category, f = tidysw$Name)
  tidyswl <- tidysw %>%
      select(-category) %>%
      unique()
  tidyswl[['categories']] <- catlist[tidyswl$Name]
  tidyswl
}


#' write out json and csv files
#'
#' @export
write_files = function(destdir) {
  dir.create(destdir, recursive = TRUE)
  swsheet = get_tidy_sw_list()
  #write_csv(swsheet,path=file.path(destdir,'single-cell-software_tidy.csv'))
  writeLines(toJSON(tidysw_to_list_df(swsheet), pretty = TRUE),
             file.path(destdir, 'software.json'))
}

write_files(opts$OUTPUT_DIR)
