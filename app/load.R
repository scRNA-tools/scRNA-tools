load_database <- function(dir = "database") {

    tools        <- load_tools(dir)
    doi_idx      <- load_doi_idx(dir)
    repositories <- load_repositories(dir)
    ignored      <- load_ignored(dir)
    cat_idx      <- load_cat_idx(dir)
    references   <- load_references(dir)
    categories   <- load_categories(dir)

    tools_list <- create_tools_list(tools, doi_idx, repositories, ignored,
                                    cat_idx)

    database <- list(
        Tools        = tools_list,
        References   = references,
        Categories   = categories
    )

    usethis::ui_done(glue::glue(
        "Database loaded from {usethis::ui_path(dir)}"
    ))

    return(database)
}

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

load_doi_idx <- function(dir) {
    readr::read_tsv(
        fs::path(dir, "doi-idx.tsv"),
        col_types = readr::cols(
            Tool  = readr::col_character(),
            DOI   = readr::col_character()
        )
    )
}

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
            Timestamp = readr::col_datetime(format = ""),
            Delay     = readr::col_double()
        )
    )
}

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
            bioc         = dplyr::filter(repositories, Tool == ..1)$Bioc,
            cran         = dplyr::filter(repositories, Tool == ..1)$CRAN,
            pypi         = dplyr::filter(repositories, Tool == ..1)$PyPI,
            conda        = dplyr::filter(repositories, Tool == ..1)$Conda,
            github       = dplyr::filter(repositories, Tool == ..1)$GitHub,
            ignored      = dplyr::filter(ignored,      Tool == ..1)$Repository,
            categories   = dplyr::filter(cat_idx,      Tool == ..1)$Category,
            added        = ..6,
            updated      = ..7
        )
    )
    names(tools_list) <- tools$Tool

    return(tools_list)
}

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
            Added      = readr::col_date(format = "")
        )
    )

    usethis::ui_done(glue::glue(
        "Loaded packages cache from {usethis::ui_path(dir)}"
    ))

    if (mod_diff > 7) {
        usethis::ui_info(paste(
            "Packages cache is out of date. Updating packages cache..."
        ))
        new_pkgs_cache <- get_pkgs_cache()

        pkgs_cache <- dplyr::bind_rows(pkgs_cache) %>%
            dplyr::group_by(Repository) %>%
            dplyr::top_n(1, dplyr::desc(Added)) %>%
            dplyr::arrange(Repository)

        readr::write_tsv(pkgs_cache, fs::path(dir, "packages-cache.tsv"))
        usethis::ui_done("Packages cache written to {usethis::ui_path(dir)}")
    }

    return(pkgs_cache)
}
