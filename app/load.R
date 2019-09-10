load_database <- function(dir = "database") {

    tools        <- load_tools(dir)
    doi_idx      <- load_doi_idx(dir)
    repo_idx     <- load_repo_idx(dir)
    ignored_idx  <- load_ignored_idx(dir)
    cat_idx      <- load_cat_idx(dir)
    references   <- load_references(dir)
    repositories <- load_repositories(dir)
    categories   <- load_categories(dir)

    tools_list <- create_tools_list(tools, doi_idx, repo_idx, ignored_idx,
                                    cat_idx)

    list(
        Tools        = tools_list,
        References   = references,
        Repositories = repositories,
        Categories   = categories
    )
}

load_tools <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "tools.tsv"),
        col_types = readr::cols(
            Tool        = readr::col_character(),
            Platform    = readr::col_character(),
            Code        = readr::col_character(),
            Description = readr::col_character(),
            Added       = readr::col_date(format = ""),
            Updated     = readr::col_date(format = "")
        )
    )
}

load_doi_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "doi-idx.tsv"),
        col_types = readr::cols(
            Tool  = readr::col_character(),
            DOI   = readr::col_character()
        )
    )
}

load_repo_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "repositories-idx.tsv"),
        col_types = readr::cols(
            Tool       = readr::col_character(),
            Repository = readr::col_character()
        )
    )
}

load_ignored_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "ignored-idx.tsv"),
        col_types = readr::cols(
            Tool       = readr::col_character(),
            Repository = readr::col_character()
        )
    )
}

load_cat_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "categories-idx.tsv"),
        col_types = readr::cols(
            Tool     = readr::col_character(),
            Category = readr::col_character()
        )
    )
}

load_references <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "references.tsv"),
        col_types = readr::cols(
            DOI       = readr::col_character(),
            arXiv     = readr::col_logical(),
            Preprint  = readr::col_logical(),
            Date      = readr::col_character(),
            Title     = readr::col_character(),
            Citations = readr::col_double(),
            Timestamp = readr::col_datetime(format = "")
        )
    )
}

load_repositories <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "repositories.tsv"),
        col_types = readr::cols(
            Repository = readr::col_character(),
            Type       = readr::col_character(),
            Name       = readr::col_character()
        )
    )
}

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

create_tools_list <- function(tools, doi_idx, repo_idx, ignored_idx, cat_idx) {

    tools_list <- purrr::pmap(
        tools,
        ~ new_sctool(
            name         = ..1,
            platform     = ..2,
            code         = ..3,
            description  = ..4,
            dois         = dplyr::filter(doi_idx,     Tool == ..1)$DOI,
            repositories = dplyr::filter(repo_idx,    Tool == ..1)$Repository,
            ignored      = dplyr::filter(ignored_idx, Tool == ..1)$Repository,
            categories   = dplyr::filter(cat_idx,     Tool == ..1)$Category,
            added        = ..5,
            updated      = ..6
        )
    )
    names(tools_list) <- tools$Tool

    return(tools_list)
}
