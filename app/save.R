save_database <- function(database, dir = "database") {

    fs::dir_create(dir)

    doi_idx     <- get_doi_idx(database$Tools)
    repo_idx    <- get_repo_idx(database$Tools)
    ignored_idx <- get_ignored_idx(database$Tools)
    cat_idx     <- get_cat_idx(database$Tools)

    readr::write_tsv(database$References,   fs::path(dir, "references.tsv"))
    readr::write_tsv(doi_idx,               fs::path(dir, "doi-idx.tsv"))
    readr::write_tsv(database$Repositories, fs::path(dir, "repositories.tsv"))
    readr::write_tsv(repo_idx,
                     fs::path(dir, "repositories-idx.tsv"))
    readr::write_tsv(ignored_idx,           fs::path(dir, "ignored-idx.tsv"))
    readr::write_tsv(database$Categories,   fs::path(dir, "categories.tsv"))
    readr::write_tsv(cat_idx,               fs::path(dir, "categories-idx.tsv"))
}

get_doi_idx <- function(tools_list) {
    purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool = .tool$Tool,
            DOI  = .tool$DOIs
        )
    })
}

get_repo_idx <- function(tools_list) {
    purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool       = .tool$Tool,
            Repository = .tool$Repositories
        )
    })
}

get_ignored_idx <- function(tools_list) {
    purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool       = .tool$Tool,
            Repository = .tool$Ignored
        )
    })
}

get_cat_idx <- function(tools_list) {
    purrr::map_dfr(tools_list, function(.tool) {
        tibble::tibble(
            Tool     = .tool$Tool,
            Category = .tool$Categories
        )
    })
}
