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
    library(ggplot2)
    library(cowplot)
    library(plotly)
    library(htmlwidgets)
    library(widgetframe)
    library(purrr)
})

#### FUNCTIONS ####

#' Get software sheet
#'
#' Read `single_cell_software.csv`
#'
#' @return Tibble containing table
get_swsheet <- function() {

    message("Loading 'single_cell_software.csv'...")
    swsheet <- read_csv("single_cell_software.csv",
                        col_types = cols(
                            .default = col_logical(),
                            Name = col_character(),
                            Platform = col_character(),
                            DOIs = col_character(),
                            PubDates = col_character(),
                            Code = col_character(),
                            Description = col_character(),
                            License = col_character(),
                            Added = col_date(format = ""),
                            Updated = col_date(format = "")
                        ))
}


#' Get packages
#'
#' Get lists of the packages available in Bioconductor, CRAN and PyPI
#'
#' @return List of named vectors containg available packages
get_pkgs <- function() {

    message("Getting package repositories...")

    message("Getting Bioconductor package list...")
    bioc.pkgs <- all_group()
    names(bioc.pkgs) <- str_to_lower(bioc.pkgs)

    message("Getting CRAN package list...")
    cran.url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
    #cran.url <- "cran_packages.html"
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


#' Get category descriptions
#'
#' Read `docs/data/descriptions.json`
#'
#' @return data.frame containing categories and descriptions
get_descriptions <- function() {
    message("Getting category descriptions...")
    descs <- read_json("docs/data/descriptions.json", simplifyVector = TRUE)
}


#' Get cached reference titles
#'
#' Read `docs/data/titles.csv`, create if missing
#'
#' @return title containing DOIs and titles
get_cached_titles <- function() {
    message("Getting cached titles...")

    if (!file.exists("docs/data/titles.csv")) {
        message("Cache file missing, creating...")
        write_lines("DOI,Title", "docs/data/titles.csv")
    }

    titles <- read_csv("docs/data/titles.csv",
                       col_types = cols(
                           DOI = col_character(),
                           Title = col_character()
                       )
              )
}


#' Add to titles cache
#'
#' Add a DOI-Title pair to the title cache
#'
#' @param swsheet Tibble containing software table
#' @param titles_cache Current titles cache
#'
#' @return Updated titles cache
add_to_titles_cache <- function(swsheet, titles_cache) {
    message("Adding new titles to cache...")

    n_added <- 0
    for (dois in swsheet$DOIs) {
        for (doi in str_split(dois, ";")[[1]]) {
            if (!is.na(doi) & !(doi %in% titles_cache$DOI)) {
                title <- cr_works(doi)$data$title

                if (!is.null(title)) {
                    titles_cache <- bind_rows(titles_cache,
                                              c(DOI = doi, Title = title))
                    message(doi, " added to cache")
                    n_added <- n_added + 1
                }
            }
        }
    }

    write_csv(titles_cache, "docs/data/titles.csv")
    message("Added ", n_added, " new titles to cache")

    return(titles_cache)
}


#' Fix DOIs
#'
#' Determine which papers are preprints and add `DOIURL` field
#'
#' @param swsheet Tibble containing software table
#'
#' @return swsheet with additional columns
fix_doi <- function(swsheet) {

    message("Fixing references...")

    swsheet %>%
        mutate(Preprint = (PubDates == "PREPRINT")) %>%
        mutate(PubDates = ifelse(Preprint == FALSE, PubDates, NA)) %>%
        mutate(PubDates = as_date(PubDates)) %>%
        mutate(Preprint = ifelse(Preprint == TRUE, TRUE, NA)) %>%
        mutate(DOIURL = ifelse(is.na(DOI), NA,
                               paste0('http://dx.doi.org/', DOI)))
}


#' Get titles
#'
#' Get title for DOIs. Return from cache if present, otherwise requests from
#' Crossref
#'
#' @param dois Character vector of dois
#' @param titles_cache Tibble containing cached titles
#'
#' @return vector of titles
get_titles <- function(dois, titles_cache) {

    titles <- map(dois, function(doi) {
        if (doi %in% titles_cache$DOI) {
            titles_cache %>%
                filter(DOI == doi) %>%
                pull(Title)
        } else {
            NA
        }
    }) %>%
        flatten_chr()

    return(titles)
}


#' Add references
#'
#' Covert references to list column and get citations
#'
#' @param swsheet Tibble containing software table
#' @param titles_cache Tibble containing titles cache
#'
#' @return swsheet with additional columns
add_refs <- function(swsheet, titles_cache) {

    message("Adding references...")

    doi_list <- swsheet %>%
        mutate(DOIs = str_split(DOIs, ";")) %>%
        pull(DOIs) %>%
        setNames(swsheet$Name)

    date_list <- swsheet %>%
        mutate(PubDates = str_split(PubDates, ";")) %>%
        pull(PubDates) %>%
        setNames(swsheet$Name)

    ref_list <- pbsapply(names(doi_list), function(x) {
        dois <- doi_list[[x]]
        dates <- date_list[[x]]
        stopifnot(length(dois) == length(dates))

        if (all(is.na(dois))) {
            return(NA)
        }

        cites <- sapply(dois, function(doi) {
            cite <- tryCatch({
                cr_citation_count(doi)
            }, error = function(e) {
                NA
            })

            Sys.sleep(sample(seq(0, 1, 0.1), 1))

            return(cite)
        })

        titles <- get_titles(dois, titles_cache)

        ref <- tibble(Title = titles,
                      DOI = dois,
                      PubDate = ifelse(dates != "PREPRINT", dates, NA),
                      Preprint = dates == "PREPRINT",
                      Citations = cites)
    })

    swsheet$Refs <- ref_list
    swsheet$Citations <- ref_list %>%
        map_if(!is.na(ref_list), function(x) {sum(x$Citations)}) %>%
        unlist()

    return(swsheet)
}


#' Add Github
#'
#' Add field indicating Github repositories
#'
#' @param swsheet Tibble containing software table
#'
#' @return swsheet with Github column
add_github <- function(swsheet) {

    message("Adding Github...")

    swsheet %>%
        mutate(Github = ifelse(str_detect(Code, "github"),
                               str_replace(Code, "https://github.com/", ""),
                               NA))
}


#' Add repositories
#'
#' Add fields indicating if tools are available from various software
#' repositories
#'
#' @param swsheet Tibble containing software table
#' @param pkgs List of available packages
#'
#' @return swsheet with additional repository columns
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


#' Add citations
#'
#' Add citations from Crossref
#'
#' @param swsheet Tibble containing software table
#'
#' @return swsheet with additional citations column
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

            Sys.sleep(sample(seq(0, 2, 0.5), 1))

            return(cite)
    })

    swsheet$Citations <- cites

    return(swsheet)
}


#' Tidy software table
#'
#' Convert the software table to tidy format
#'
#' @param swsheet Tibble containing software table
#'
#' @return Tidy swsheet tibble
tidy_swsheet <- function(swsheet) {

    message("Tidying data...")

    gather(swsheet, key = 'Category', value = 'Val',
           -Description, -Name, -Platform, -DOIs, -PubDates, -Updated, -Added,
           -Code, -Github, -License, -BioC, -CRAN, -PyPI, -Refs, -Citations) %>%
        filter(Val == TRUE) %>%
        select(-Val) %>%
        arrange(Name)
}


#' Add categories
#'
#' Add categories column to software table
#'
#' @param swsheet Tibble containing software table
#' @param tidysw Tibble containing tidy software table
#'
#' @return swsheet with additional categories column
add_cats <- function(swsheet, tidysw) {

    message("Adding categories to table...")

    catlist <- split(tidysw$Category, f = tidysw$Name)

    catdf <- data.frame(Name = names(catlist), stringsAsFactors = FALSE)
    catdf[['Categories']] <- catlist

    swsheet <- left_join(swsheet, catdf, by = "Name")
}


#' Get tools JSON
#'
#' Create tools JSON
#'
#' @param tidysw Tibble containing tidy software table
#'
#' @return tools JSON
get_tools_json <- function(tidysw) {

    message("Converting tools...")

    catlist <- split(tidysw$Category, f = tidysw$Name)

    tools <- tidysw %>%
        select(-Category) %>%
        unique() %>%
        mutate(Categories = catlist[Name]) %>%
        toJSON(pretty = TRUE)
}


#' Get categories JSON
#'
#' Create categories JSON
#'
#' @param tidysw Tibble containing tidy software table
#' @param swsheet Tibble containing software table
#' @param descs data.frame containing category descriptions
#'
#' @return categories JSON
get_cats_json <- function(tidysw, swsheet, descs) {

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
        left_join(descs, by = "Category") %>%
        toJSON(pretty = TRUE)
}


#' Plot number of tools
#'
#' Produces a HTML page with an interactive plot of the number of tools over
#' time
#'
#' @param swsheet Tibble containing software table
plot_number <- function(swsheet) {

    datecount <- swsheet %>%
        select(Date = Added) %>%
        group_by(Date = as.Date(Date)) %>%
        summarise(Count = n()) %>%
        complete(Date = full_seq(Date, 1), fill = list(Count = 0)) %>%
        mutate(Total = cumsum(Count))

    plot <- ggplot(datecount, aes(x = Date, y = Total)) +
        geom_line(size = 2, colour = "#7A52C7") +
        xlab("Date") +
        ylab("Number of tools") +
        scale_x_date(breaks = scales::pretty_breaks(10)) +
        ggtitle("Number of tools over time") +
        theme_cowplot() +
        theme(plot.title = element_text(size = 20),
              axis.title.x = element_blank(),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 60, vjust = 0.5)
        )

    plot <- ggplotly(plot, dynamicTicks = TRUE) %>%
        layout(margin = list(l = 70, r = 40, b = 90, t = 50))

    saveWidget(frameableWidget(plot),
               file.path(getwd(), "docs/plots/number.html"),
               selfcontained = FALSE, libdir = "libraries")
}


#' Plot publication status
#'
#' Produces a HTML page with an interactive plot of the publication status of
#' tools in the database
#'
#' @param swsheet Tibble containing software table
plot_publication <- function(swsheet) {
    plot <- swsheet %>%
        mutate(HasPub = map_if(.$Refs, !is.na(Refs),
                               function(x) {any(x$Preprint == FALSE)}),
               HasPub = unlist(HasPub),
               HasPub = if_else(is.na(HasPub), FALSE, HasPub)) %>%
        mutate(HasPre = map_if(.$Refs, !is.na(Refs),
                               function(x) {any(x$Preprint == TRUE)}),
               HasPre = unlist(HasPre),
               HasPre = if_else(is.na(HasPre), FALSE, HasPre & !HasPub)) %>%
        mutate(HasNot = !HasPub & !HasPre) %>%
        summarise(NotPublished = sum(HasNot),
                  Published = sum(HasPub),
                  Preprint = sum(HasPre)) %>%
        gather(key = Type, value = Count) %>%
        mutate(Type = factor(Type,
                             levels = c("Published", "Preprint",
                                        "NotPublished"),
                             labels = c("Published", "Preprint",
                                        "Not Published"))) %>%
        arrange(Type) %>%
        mutate(Cumulative = cumsum(Count),
               Midpoint = Cumulative - (Count / 2),
               Midpoint = max(Cumulative) - Midpoint,
               Percent = round(Count / sum(Count) * 100, 1),
               Label = paste0(Type, " ", Percent, "%")) %>%
        ggplot(aes(x = 1, fill = Type, weight = Count,
                   text = paste("Percent:", Percent))) +
        geom_bar(width = 1, position = "stack") +
        geom_text(aes(x = 1, y = Midpoint, label = Label),
                  size = 6, colour = "white") +
        scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F")) +
        scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F")) +
        ggtitle("Publication status") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    plot <- ggplotly(plot, tooltip = c("fill", "weight", "text")) %>%
        layout(margin = list(l = 100, r = 100, b = 20, t = 50))

    saveWidget(frameableWidget(plot),
               file.path(getwd(), "docs/plots/publication.html"),
               selfcontained = FALSE, libdir = "libraries")
}

#' Plot licenses
#'
#' Produces a HTML page with an interactive plot of the licenses used by tools
#' in the database
#'
#' @param swsheet Tibble containing software table
plot_licenses <- function(swsheet) {
    plot <- swsheet %>%
        select(License) %>%
        mutate(IsGPL = str_detect(License, "GPL"),
               IsBSD = str_detect(License, "BSD"),
               IsMIT = str_detect(License, "MIT"),
               IsApache = str_detect(License, "Apache"),
               IsArtistic = str_detect(License, "Artistic"),
               IsUnknown = is.na(License),
               IsOther = !(IsGPL | IsBSD | IsMIT | IsApache | IsArtistic |
                               IsUnknown)) %>%
        summarise(Apache = sum(IsApache, na.rm = TRUE),
                  Artistic = sum(IsArtistic, na.rm = TRUE),
                  BSD = sum(IsBSD, na.rm = TRUE),
                  GPL = sum(IsGPL, na.rm = TRUE),
                  MIT = sum(IsMIT, na.rm = TRUE),
                  Other = sum(IsOther),
                  Unknown = sum(IsUnknown)) %>%
        gather(key = License, value = Count) %>%
        mutate(License = factor(License,
                                levels = c("Apache", "Artistic", "BSD", "GPL",
                                           "MIT", "Other", "Unknown")),
               Percent = round(Count / sum(Count) * 100, 1),
               Label = paste0(License, "\n", Percent, "%")) %>%
        ggplot(aes(x = License, weight = Count, fill = License,
                   text = paste("Percent:", Percent))) +
        geom_bar(width = 0.95, position = "dodge") +
        geom_text(aes(x = License, y = Count + 4, label = Label,
                      colour = License), size = 5) +
        scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F", "#00B7C6",
                                     "#F47920", "#7A52C7", "#999999")) +
        scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                       "#00B7C6", "#F47920", "#7A52C7",
                                       "#999999")) +
        ggtitle("Software licenses") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    plot <- ggplotly(plot, tooltip = c("x", "weight", "text")) %>%
        layout(margin = list(l = 10, r = 10, b = 20, t = 80))

    saveWidget(frameableWidget(plot),
               file.path(getwd(), "docs/plots/licenses.html"),
               selfcontained = FALSE, libdir = "libraries")
}


#' Plot platforms
#'
#' Produces a HTML page with an interactive plot of the platforms used by tools
#' in the database
#'
#' @param swsheet Tibble containing software table
plot_platforms <- function(swsheet) {

    plot <- swsheet %>%
        select(Platform) %>%
        mutate(IsR = str_detect(Platform, "R"),
               IsPython = str_detect(Platform, "Python"),
               IsMATLAB = str_detect(Platform, "MATLAB"),
               IsCPP = str_detect(Platform, "C++"),
               IsOther = !(IsR | IsPython | IsMATLAB | IsCPP)) %>%
        summarise(R = sum(IsR),
                  Python = sum(IsPython),
                  MATLAB = sum(IsMATLAB),
                  CPP = sum(IsCPP),
                  Other = sum(IsOther)) %>%
        gather(key = Platform, value = Count) %>%
        mutate(Platform = factor(Platform,
                                 levels = c("R", "Python", "MATLAB", "CPP",
                                            "Other"),
                                 labels = c("R", "Python", "MATLAB", "C++",
                                            "Other")),
               Percent = round(Count / sum(Count) * 100, 1),
               Label = paste0(Platform, "\n", Percent, "%")) %>%
        ggplot(aes(x = Platform, weight = Count, fill = Platform,
                   text = paste("Percent:", Percent))) +
        geom_bar(width = 0.95, position = "dodge") +
        geom_text(aes(x = Platform, y = Count + 4, label = Label,
                      colour = Platform), size = 5) +
        scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F", "#00B7C6",
                                     "#F47920", "#7A52C7", "#999999")) +
        scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                       "#00B7C6", "#F47920", "#7A52C7",
                                       "#999999")) +
        ggtitle("Platforms") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    plot <- ggplotly(plot, tooltip = c("x", "weight", "text")) %>%
        layout(margin = list(l = 10, r = 10, b = 20, t = 80))

    saveWidget(frameableWidget(plot),
               file.path(getwd(), "docs/plots/platforms.html"),
               selfcontained = FALSE, libdir = "libraries")
}

#' Plot categories
#'
#' Produces a HTML page with an interactive plot showing the percentage of tools
#' in each category
#'
#' @param swsheet Tibble containing software table
plot_categories <- function(swsheet) {

    catcounts <- swsheet %>%
        summarise_at(8:38, sum) %>%
        gather(key = Category, value = Count) %>%
        arrange(-Count, Category) %>%
        mutate(Prop = Count / nrow(swsheet)) %>%
        mutate(Category = str_replace_all(Category, "([[:upper:]])", " \\1")) %>%
        mutate(Category = str_trim(Category)) %>%
        mutate(Category = ifelse(Category == "U M Is", "UMIs", Category)) %>%
        mutate(Category = factor(Category, levels = Category)) %>%
        mutate(Percent = round(Prop * 100, 1))

    plot <- ggplot(catcounts,
                   aes(x = Category, weight = Prop,
                       text = paste("Count:", Count, "\n",
                                    "Percent:", Percent))) +
        geom_bar(fill = "#7A52C7") +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentage of tools") +
        ggtitle("Categories") +
        theme_cowplot() +
        theme(axis.title.x = element_blank(),
              legend.position = "none",
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.key.size = unit(25, "points"),
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
        )

    plot <- ggplotly(plot, tooltip = c("x", "text")) %>%
        layout(margin = list(l = 80, r = 10, b = 200, t = 50))

    saveWidget(frameableWidget(plot),
               file.path(getwd(), "docs/plots/categories.html"),
               selfcontained = FALSE, libdir = "libraries")
}


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

    # Process table
    message("Processing table...")
    swsheet <- swsheet %>%
        add_refs(titles_cache) %>%
        add_github() %>%
        add_repos(pkgs) #%>%
        #add_citations()

    # Convert to tidy format
    tidysw <- tidy_swsheet(swsheet)

    # Add categories to table
    swsheet <- add_cats(swsheet, tidysw)

    # Convert to JSON
    message("Converting to JSON...")
    message("Converting table...")
    table <- toJSON(swsheet, pretty = TRUE)
    tools <- get_tools_json(tidysw)
    cats <- get_cats_json(tidysw, swsheet, descs)

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

    message("Done!")
}


#### MAIN CODE ####

# Setup progress bar
pboptions(type = "timer", char = "=", style = 3)

# Get options
opts <- docopt(doc)

# Process table
process_csv()
