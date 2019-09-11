save_database <- function(database, dir = "database") {

    `%>%` <- magrittr::`%>%`

    fs::dir_create(dir)

    tools       <- get_tools(database$Tools)
    doi_idx     <- get_doi_idx(database$Tools)
    repo_idx    <- get_repo_idx(database$Tools)
    ignored_idx <- get_ignored_idx(database$Tools)
    cat_idx     <- get_cat_idx(database$Tools)

    references <- database$References %>%
        dplyr::group_by(DOI) %>%
        dplyr::filter(dplyr::row_number(dplyr::desc(Timestamp)) == 1) %>%
        dplyr::arrange(DOI)

    repositories <- database$Repositories %>%
        dplyr::distinct() %>%
        dplyr::arrange(Repository)

    readr::write_tsv(tools,                 fs::path(dir, "tools.tsv"))
    readr::write_tsv(database$References,   fs::path(dir, "references.tsv"))
    readr::write_tsv(doi_idx,               fs::path(dir, "doi-idx.tsv"))
    readr::write_tsv(database$Repositories, fs::path(dir, "repositories.tsv"))
    readr::write_tsv(repo_idx,
                     fs::path(dir, "repositories-idx.tsv"))
    readr::write_tsv(ignored_idx,           fs::path(dir, "ignored-idx.tsv"))
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

get_repo_idx <- function(tools_list) {
    repo_idx <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool       = .tool$Tool,
            Repository = .tool$Repositories
        )
    })

    repo_idx <- dplyr::distinct(dplyr::arrange(repo_idx, Tool, Repository))
}

get_ignored_idx <- function(tools_list) {
    ignored_idx <- purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool       = .tool$Tool,
            Repository = .tool$Ignored
        )
    })

    ignored_idx <- dplyr::distinct(
        dplyr::arrange(ignored_idx, Tool, Repository)
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
