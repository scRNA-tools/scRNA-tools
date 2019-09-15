save_database <- function(database, dir = "database") {

    `%>%` <- magrittr::`%>%`

    fs::dir_create(dir)

    tools        <- get_tools(database$Tools)
    doi_idx      <- get_doi_idx(database$Tools)
    ignored      <- get_ignored(database$Tools)
    cat_idx      <- get_cat_idx(database$Tools)
    repositories <- get_repositories(database$Tools)

    references <- database$References %>%
        dplyr::group_by(DOI) %>%
        dplyr::top_n(1, Timestamp) %>%
        dplyr::arrange(DOI)

    readr::write_tsv(tools,                 fs::path(dir, "tools.tsv"))
    readr::write_tsv(database$References,   fs::path(dir, "references.tsv"))
    readr::write_tsv(doi_idx,               fs::path(dir, "doi-idx.tsv"))
    readr::write_tsv(repositories,          fs::path(dir, "repositories.tsv"))
    readr::write_tsv(ignored,               fs::path(dir, "ignored.tsv"))
    readr::write_tsv(database$Categories,   fs::path(dir, "categories.tsv"))
    readr::write_tsv(cat_idx,               fs::path(dir, "categories-idx.tsv"))

    usethis::ui_done(glue::glue(
        "Database written to {usethis::ui_path(dir)}"
    ))
}

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

    tools <- dplyr::distinct(dplyr::arrange(tools, Tool))

    return(tools)
}

get_doi_idx <- function(tools_list) {
    doi_idx <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool = .tool$Tool,
            DOI  = .tool$DOIs
        )
    })

    doi_idx <- dplyr::distinct(dplyr::arrange(doi_idx, Tool, DOI))

    return(doi_idx)
}

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

    repositories <- dplyr::arrange(repositories, Tool)

    return(repositories)
}

get_ignored <- function(tools_list) {
    ignored <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool = .tool$Tool,
            Type = stringr::str_remove(.tool$Ignored, "^.+@"),
            Name = stringr::str_remove(.tool$Ignored, "@.+$")
        )
    })

    ignored <- dplyr::distinct(
        dplyr::arrange(ignored, Tool, Type, Name)
    )
}

get_cat_idx <- function(tools_list) {
    cat_idx <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool     = .tool$Tool,
            Category = .tool$Categories
        )
    })

    cat_idx <- dplyr::distinct(dplyr::arrange(cat_idx, Tool, Category))
}
