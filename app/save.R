#' Save database
#'
#' Save the scRNA-tools database to disk
#'
#' @param database Database object
#' @param dir Path to directory containing the database
#' @param cache Whether to save database cache Rds file
save_database <- function(database, dir = "database", cache = TRUE) {

    `%>%` <- magrittr::`%>%`

    usethis::ui_todo("Saving database to {usethis::ui_path(dir)}...")
    
    fs::dir_create(dir)

    tools        <- get_tools(database$Tools)
    doi_idx      <- get_doi_idx(database$Tools)
    ignored      <- get_ignored(database$Tools)
    cat_idx      <- get_cat_idx(database$Tools)
    repositories <- get_repositories(database$Tools)

    database$References <- database$References %>%
        dplyr::filter(DOI %in% doi_idx$DOI) %>%
        dplyr::group_by(DOI) %>%
        # Select most recent timestamp
        dplyr::top_n(1, Timestamp) %>%
        # Select most recent date (or NA)
        dplyr::arrange(dplyr::desc(Date)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        arrange_locale(DOI)

    database$RefLinks <- database$RefLinks %>%
        dplyr::distinct() %>%
        arrange_locale(Preprint)
    
    citations  <- dplyr::select(database$References, DOI, Citations, Timestamp,
                                Delay)
    references <- dplyr::select(database$References, DOI, arXiv, Preprint, Date,
                                Title) %>%
        dplyr::distinct()

    readr::write_tsv(tools,               fs::path(dir, "tools.tsv"), escape = "none")
    readr::write_tsv(references,          fs::path(dir, "references.tsv"))
    readr::write_tsv(database$RefLinks,   fs::path(dir, "reference-links.tsv"))
    readr::write_tsv(citations,           fs::path(dir, "citations-cache.tsv"))
    readr::write_tsv(doi_idx,             fs::path(dir, "doi-idx.tsv"))
    readr::write_tsv(repositories,        fs::path(dir, "repositories.tsv"))
    readr::write_tsv(ignored,             fs::path(dir, "ignored.tsv"))
    readr::write_tsv(database$Categories, fs::path(dir, "categories.tsv"))
    readr::write_tsv(cat_idx,             fs::path(dir, "categories-idx.tsv"))

    usethis::ui_done("Files written")
    
    if (cache) {
        readr::write_rds(database, fs::path(dir, "database-cache.Rds"))
        usethis::ui_done("Cache written")
    }
}

#' Get tools
#'
#' Get the tools table from a list of `sctool` objects
#'
#' @param tools_list List of `sctool` objects
#'
#' @return tibble
get_tools <- function(tools_list) {
    tools <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool        = .tool$Tool,
            Platform    = .tool$Platform,
            Code        = .tool$Code,
            Description = .tool$Description,
            License     = .tool$License,
            Added       = .tool$Added,
            Updated     = .tool$Updated
        )
    })

    tools <- dplyr::distinct(arrange_locale(tools, Tool))

    return(tools)
}

#' Get DOI index
#'
#' Get the DOI index table from a list of `sctool` objects
#'
#' @param tools_list List of `sctool` objects
#'
#' @return tibble
get_doi_idx <- function(tools_list) {
    doi_idx <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool = .tool$Tool,
            DOI  = .tool$DOIs
        )
    })

    doi_idx <- dplyr::distinct(arrange_locale(doi_idx, Tool, DOI))

    return(doi_idx)
}

#' Get repositories
#'
#' Get the repositories table from a list of `sctool` objects
#'
#' @param tools_list List of `sctool` objects
#'
#' @return tibble
get_repositories <- function(tools_list) {
    repositories <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool   = .tool$Tool,
            Bioc   = .tool$Repositories["Bioc"],
            CRAN   = .tool$Repositories["CRAN"],
            PyPI   = .tool$Repositories["PyPI"],
            Conda  = .tool$Repositories["Conda"],
            GitHub = .tool$Repositories["GitHub"]
        )
    })

    repositories <- arrange_locale(repositories, Tool)

    return(repositories)
}

#' Get ignored
#'
#' Get the ignored repositories table from a list of `sctool` objects
#'
#' @param tools_list List of `sctool` objects
#'
#' @return tibble
get_ignored <- function(tools_list) {
    ignored <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool = .tool$Tool,
            Type = stringr::str_remove(.tool$Ignored, "^.+@"),
            Name = stringr::str_remove(.tool$Ignored, "@.+$")
        )
    })

    ignored <- dplyr::distinct(
        arrange_locale(ignored, Tool, Type, Name)
    )
}

#' Get categories index
#'
#' Get the categories index table from a list of `sctool` objects
#'
#' @param tools_list List of `sctool` objects
#'
#' @return tibble
get_cat_idx <- function(tools_list) {
    cat_idx <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool     = .tool$Tool,
            Category = .tool$Categories
        )
    })

    cat_idx <- dplyr::distinct(arrange_locale(cat_idx, Tool, Category))
}
