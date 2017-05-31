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

#' Create tidy sheet from the google sheet
#' @export
get_tidy_sw_list <- function() {
    bioc.pkgs <- BiocInstaller::all_group()
    names(bioc.pkgs) <- str_to_lower(bioc.pkgs)

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
                                     bioc.pkgs[str_to_lower(Name)], NA))

    gather(swsheet, key = 'category', value = 'val',
           -Description, -Name, -Platform, -DOI, -PubDate, -Updated, -Added,
           -Preprint, -Code, -DOI_url, -License, -Bioconductor) %>%
    #mutate(Github = grepl('github', Code)) %>%
    #mutate(CRAN = grepl('cran\\.r-project', Code)) %>%
    filter(val == TRUE) %>%
    select(-val)
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
