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

                if (str_detect(doi, "arxiv")) {
                    id <- str_remove(doi, "arxiv/")
                    title <- arxiv_search(id_list = id)$title
                } else {
                    title <- cr_works(doi)$data$title
                }

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
