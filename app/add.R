add_tool <- function(database, pkgs_cache) {

    cat("\n")
    usethis::ui_todo("Please enter the details of the new tool to add")
    cat("\n")

    name         <- prompt_name(database)
    platform     <- prompt_platform()
    code         <- prompt_code()
    description  <- prompt_description()
    dois         <- prompt_dois()
    refs         <- get_references(dois)
    dois         <- refs$DOI
    categories   <- prompt_categories(database)
    pkgs_matches <- check_pkgs_cache(name, pkgs_cache)
    repos        <- pkgs_matches$Real
    ignored      <- pkgs_matches$Ignored
    repositories <- pkgs_matches$Repositories
    added        <- lubridate::today()
    updated      <- lubridate::today()

    if (stringr::str_detect(code, "github.com")) {
        gh_name <- stringr::str_remove(code, "https://github.com/")
        gh_repo <- paste(gh_name, "GitHub", sep = "@")
        repos <- c(repos, gh_repo)
        repositories <- dplyr::bind_rows(
            repositories,
            tibble::tibble(
                Repository = gh_repo,
                Type = "GitHub",
                Name = gh_name)
        )
        usethis::ui_done("Found GitHub repository")
    }

    tool <- new_sctool(name, platform, code, description, dois, repos,
                       ignored, categories, added, updated)

    database$Tools[[name]] <- tool
    database$References <- dplyr::bind_rows(database$References, refs)
    database$Repositories <- dplyr::bind_rows(database$Repositories,
                                              repositories)
    usethis::ui_done(glue::glue(
        "Added {usethis::ui_value(name)} to database"
    ))

    cat("\n")
    usethis::ui_todo("Please check the new tool is correct")
    cat("\n")
    print(database$Tools[[name]])

    correct <- prompt_yn("\nIs this correct (y/n)?")

    if (!correct) {
        database <- update_tool(database, pkgs_cache, name)
    }

    return(database)
}
