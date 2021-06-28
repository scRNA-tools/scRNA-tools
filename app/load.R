#' Load database
#'
#' Load the scRNA-tools database from disk
#'
#' @param dir Path to directory containg the database
#'
#' @return Database object
load_database <- function(dir = "database") {

    usethis::ui_todo("Loading database...")
    
    tools        <- load_tools(dir)
    doi_idx      <- load_doi_idx(dir)
    repositories <- load_repositories(dir)
    ignored      <- load_ignored(dir)
    cat_idx      <- load_cat_idx(dir)
    references   <- load_references(dir)
    ref_links    <- load_ref_links(dir)
    categories   <- load_categories(dir)

    tools_list <- create_tools_list(tools, doi_idx, repositories, ignored,
                                    cat_idx)

    database <- list(
        Tools      = tools_list,
        References = references,
        RefLinks   = ref_links,
        Categories = categories
    )

    usethis::ui_done(glue::glue(
        "Database loaded from {usethis::ui_path(dir)}"
    ))

    return(database)
}

#' Load tools
#'
#' Load the tools table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_tools <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "tools.tsv"),
        col_types = readr::cols(
            Tool        = readr::col_character(),
            Platform    = readr::col_character(),
            Code        = readr::col_character(),
            Description = readr::col_character(),
            License     = readr::col_character(),
            Added       = readr::col_date(format = ""),
            Updated     = readr::col_date(format = "")
        )
    )
}

#' Load DOI index
#'
#' Load the DOI index table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_doi_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "doi-idx.tsv"),
        col_types = readr::cols(
            Tool  = readr::col_character(),
            DOI   = readr::col_character()
        )
    )
}

#' Load ignored
#'
#' Load the ignored repositories table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_ignored <- function(dir) {
    ignored <- readr::read_tsv(
        fs::path(dir, "ignored.tsv"),
        col_types = readr::cols(
            Tool = readr::col_character(),
            Type = readr::col_character(),
            Name = readr::col_character()
        )
    )

    ignored <- dplyr::mutate(ignored, Repository = paste(Name, Type, sep = "@"))
}

#' Load categories index
#'
#' Load the categories index table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_cat_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "categories-idx.tsv"),
        col_types = readr::cols(
            Tool     = readr::col_character(),
            Category = readr::col_character()
        )
    )
}

#' Load references
#'
#' Load the references table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_references <- function(dir) {

    references <- readr::read_tsv(
        fs::path(dir, "references.tsv"),
        col_types = readr::cols(
            DOI       = readr::col_character(),
            arXiv     = readr::col_logical(),
            Preprint  = readr::col_logical(),
            Date      = readr::col_character(),
            Title     = readr::col_character()
        )
    )

    citations_path <- fs::path(dir, "citations-cache.tsv")
    if (fs::file_exists(citations_path)) {
        citations <- readr::read_tsv(
            citations_path,
            col_types = readr::cols(
                DOI       = readr::col_character(),
                Citations = readr::col_double(),
                Timestamp = readr::col_datetime(format = ""),
                Delay     = readr::col_double()
            )
        )
    } else {
        usethis::ui_info("Creating new citations cache")
        citations <- tibble::tibble(
            DOI       = references$DOI,
            Citations = 0,
            Timestamp = lubridate::as_datetime(0),
            Delay     = 0
        )
    }

    references <- dplyr::left_join(references, citations, by = "DOI")

    # If references were missing from the citations cache replace NAs
    references <- tidyr::replace_na(
        references,
        list(
            Citations = 0,
            Timestamp = lubridate::as_datetime(0),
            Delay     = 0
        )
    )

    return(references)
}

#' Load reference links
#'
#' Load the reference links table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_ref_links <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "reference-links.tsv"),
        col_types = readr::cols(
            Preprint    = readr::col_character(),
            Publication = readr::col_character(),
            Correct     = readr::col_logical()
        )
    )
}

#' Load repositories
#'
#' Load the repositories table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_repositories <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "repositories.tsv"),
        col_types = readr::cols(
            Bioc   = readr::col_character(),
            CRAN   = readr::col_character(),
            PyPI   = readr::col_character(),
            Conda  = readr::col_character(),
            GitHub = readr::col_character()
        )
    )
}

#' Load categories
#'
#' Load the categories table from the database
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_categories <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "categories.tsv"),
        col_types = readr::cols(
            Category    = readr::col_character(),
            Phase       = readr::col_character(),
            Description = readr::col_character()
        )
    )
}

#' Create tools list
#'
#' Convert the database tables into a list of `sctool` objects
#'
#' @param tools Tools table
#' @param doi_idx DOI index table
#' @param repositories Repositories table
#' @param ignored Ignored table
#' @param cat_idx Categories index table
#'
#' @return List of `sctool` objects
create_tools_list <- function(tools, doi_idx, repositories, ignored, cat_idx) {

    tools_list <- purrr::pmap(
        tools,
        ~ new_sctool(
            name         = ..1,
            platform     = ..2,
            code         = ..3,
            description  = ..4,
            license      = ..5,
            dois         = dplyr::filter(doi_idx,      Tool == ..1)$DOI,
            categories   = dplyr::filter(cat_idx,      Tool == ..1)$Category,
            bioc         = dplyr::filter(repositories, Tool == ..1)$Bioc,
            cran         = dplyr::filter(repositories, Tool == ..1)$CRAN,
            pypi         = dplyr::filter(repositories, Tool == ..1)$PyPI,
            conda        = dplyr::filter(repositories, Tool == ..1)$Conda,
            github       = dplyr::filter(repositories, Tool == ..1)$GitHub,
            ignored      = dplyr::filter(ignored,      Tool == ..1)$Repository,
            added        = ..6,
            updated      = ..7
        )
    )
    names(tools_list) <- tools$Tool

    return(tools_list)
}

#' Load packages cache
#'
#' Load the packages cache If the cache is out of date (or does not exist) then
#' `get_pkgs_cache()` is called and the results cached to disk.
#'
#' @param dir Path to the directory containing the database
#'
#' @return tibble
load_pkgs_cache <- function(dir) {

    `%>%` <- magrittr::`%>%`

    path <- fs::path(dir, "packages-cache.tsv")

    if (!fs::file_exists(path)) {
        usethis::ui_info(paste(
            "Packages cache does not exist.",
            "Creating packages cache..."
        ))
        pkgs_cache <- get_pkgs_cache()
        fs::dir_create(dir)
        readr::write_tsv(pkgs_cache, fs::path(dir, "packages-cache.tsv"))
        usethis::ui_done("Packages cache written to {usethis::ui_path(dir)}")

        return(pkgs_cache)
    }

    mod_time <- fs::file_info(path)$modification_time
    mod_diff <- difftime(lubridate::now("UTC"), mod_time, units = "days")

    pkgs_cache <- readr::read_tsv(
        path,
        col_types = readr::cols(
            Name       = readr::col_character(),
            Type       = readr::col_character(),
            Repository = readr::col_character(),
            Added      = readr::col_datetime()
        )
    )

    usethis::ui_done(
        "Loaded packages cache from {usethis::ui_path(dir)}"
    )

    usethis::ui_info(
        "Packages cache last modified at {usethis::ui_value(mod_time)}"
    )

    if (mod_diff > 7) {
        usethis::ui_info(paste(
            "Packages cache is out of date. Updating packages cache..."
        ))
        new_pkgs_cache <- get_pkgs_cache()

        pkgs_cache <- pkgs_cache %>%
            dplyr::filter(Repository %in% new_pkgs_cache$Repository) %>%
            dplyr::bind_rows(new_pkgs_cache) %>%
            dplyr::group_by(Repository) %>%
            dplyr::top_n(1, dplyr::desc(Added)) %>%
            dplyr::arrange(Repository)

        readr::write_tsv(pkgs_cache, fs::path(dir, "packages-cache.tsv"))
        usethis::ui_done("Packages cache written to {usethis::ui_path(dir)}")
    }

    return(pkgs_cache)
}
