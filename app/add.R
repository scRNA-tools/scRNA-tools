#' Add tool
#'
#' Add a tool to the scRNA-tools database
#'
#' @param database Database object
#' @param pkgs_cache Packages cached object
#'
#' @return Database with added tool
add_tool <- function(database, pkgs_cache) {

    cat("\n")
    usethis::ui_todo("Please enter the details of the new tool to add")
    cat("\n")

    name         <- prompt_name(database)
    platform     <- prompt_platform()
    code         <- prompt_code()
    license      <- prompt_license()
    description  <- prompt_description()
    dois         <- prompt_dois()
    refs         <- get_references(dois)
    dois         <- refs$DOI
    categories   <- prompt_categories(database)

    tool <- new_sctool(name, platform, code, license, description, dois,
                       categories)

    if (!is.na(code) && stringr::str_detect(code, "github.com")) {
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

    database$Tools[[name]] <- tool
    database <- update_repositories(name, database, pkgs_cache, prompt = FALSE)

    database$References <- dplyr::bind_rows(database$References, refs)

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
