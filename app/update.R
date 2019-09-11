update_tool <- function(database, pkgs_cache, name) {

    cat("\n")
    if (!is.null(name) && !(name %in% names(database$Tools))) {
        usethis::ui_oops(glue::glue(
            "{usethis::ui_value(name)} is not present in the database"
        ))
        name <- NULL
    }

    if (is.null(name)) {
        name <- prompt_string(
            "Which tool do you want to update?:",
            allowed = "A-Za-z0-9-_", values = names(database$Tools)
        )
    }

    cat("\n")
    usethis::ui_todo(
        "The current details for {usethis::ui_value(name)} are:"
    )
    cat("\n")
    print(database$Tools[[name]])
    cat("\n")

    done <- FALSE
    while(!done) {

        field <- prompt_menu(
            "What would you like to update?",
            c("Name", "Platform", "Description", "Code", "DOIs", "Repositories",
              "Ignored", "Categories")
        )

        database <- switch(field,
            Name         = update_name(name, database),
            Platform     = update_platform(name, database),
            Description  = update_description(name, database),
            Code         = update_code(name, database),
            DOIs         = update_dois(name, database),
            Repositories = update_repositories(name, database, pkgs_cache),
            Ignored      = update_ignored(name, database, pkgs_cache),
            Categories   = update_categories(name, database)
        )

        if (field == "Tool") {
            name <- database$Name
            database <- database$Database
        }

        database$Tools[[name]]$Updated <- lubridate::today()
        usethis::ui_done(glue::glue("Updated {usethis::ui_field(field)}"))

        cat("\n")
        usethis::ui_todo(
            "The current details for {usethis::ui_value(name)} are:"
        )
        cat("\n")
        print(database$Tools[[name]])
        cat("\n")

        done <- prompt_yn(glue::glue(
            "Are you finished updating {usethis::ui_value(name)}?:"
        ))
    }

    usethis::ui_done(glue::glue(
        "Successfully updated {usethis::ui_value(name)}"
    ))

    return(database)
}

update_name <- function(name, database) {

    tool <- database$Tools[[name]]
    old_name <- tool$Tool

    usethis::ui_todo(glue::glue(
        "Enter new name. Current name is {usethis::ui_value(old_name)}."
    ))

    new_name <- prompt_name(database)
    tool$Tool <- new_name

    database$Tools[[old_name]] <- NULL
    database$Tools[[new_name]] <- tool

    return(list(Name = new_name, Database = database))
}

update_platform <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new platform. ",
        "Current platform is {usethis::ui_value(tool$Platform)}."
    ))

    tool$Platform <- prompt_platform()

    database$Tools[[name]] <- tool

    return(database)
}

update_code <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new code URL. Current URL is {usethis::ui_value(tool$Code)}."
    ))

    code <- prompt_code()

    is_gh_repo <- stringr::str_detect(tool$Repositories, "@GitHub")
    if (sum(is_gh_repo) > 0) {
        old_gh_repo <- tool$Repositories[is_gh_repo]
        tool$Repositories <- tool$Repositories[!is_gh_repo]
        usethis::ui_done("Removed old GitHub repository")
    }

    if (stringr::str_detect(code, "github.com")) {
        gh_name <- stringr::str_remove(code, "https://github.com/")
        gh_repo <- paste(gh_name, "GitHub", sep = "@")
        tool$Repositories <- c(tool$Repositories, gh_repo)
        database$Repositories <- dplyr::bind_rows(
            database$Repositories,
            tibble::tibble(
                Repository = gh_repo,
                Type = "GitHub",
                Name = gh_name)
        )
        usethis::ui_done("Found new GitHub repository")
    }

    tool$Code <- code
    database$Tools[[name]] <- tool

    return(database)
}

update_dois <- function(name, database) {

    tool <- database$Tools[[name]]
    dois_str <- paste(tool$DOIs, collapse = ", ")

    usethis::ui_todo(glue::glue(
        "Enter new DOIs. Current DOIs {usethis::ui_value(dois_str)}."
    ))

    dois <- prompt_dois()
    refs <- get_references(dois)
    dois <- refs$DOI

    tool$DOIs <- dois
    database$Tools[[name]] <- tool
    database$References <- dplyr::bind_rows(database$References, refs)

    return(database)
}

update_description <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new description. ",
        "Current description is {usethis::ui_value(tool$Description)}."
    ))

    tool$Description <- prompt_description()

    database$Tools[[name]] <- tool

    return(database)
}

update_repositories <- function(name, database, pkgs_cache) {

    tool <- database$Tools[[name]]
    repo_str <- paste(tool$Repositories, collapse = ", ")

    usethis::ui_todo(glue::glue(
        "Enter new repositories. ",
        "Current repositories are {usethis::ui_value(repo_str)}."
    ))

    tool$Repositories <- prompt_vec("Repositories:",
                                    values = pkgs_cache$Repository)

    pkgs_matches <- check_pkgs_cache(tool$Tool, pkgs_cache,
                                     current = tool$Repositories,
                                     ignored = tool$Ignored)
    tool$Repositories <- pkgs_matches$Real
    tool$Ignored      <- pkgs_matches$Ignored
    tool$Ignored      <- setdiff(tool$Ignored, tool$Repositories)
    repositories      <- pkgs_matches$Repositories

    database$Tools[[name]] <- tool
    database$Repositories <- dplyr::bind_rows(
        database$Repositories, repositories
    )

    return(database)
}

update_ignored <- function(name, database, pkgs_cache) {

    tool <- database$Tools[[name]]
    ignored_str <- paste(tool$Ignored, collapse = ", ")

    usethis::ui_todo(glue::glue(
        "Enter new ignored repositories. ",
        "Current ignored repositories are {usethis::ui_value(ignored_str)}."
    ))

    tool$Ignored <- prompt_vec("Ignored:", values = pkgs_cache$Repository)

    pkgs_matches <- check_pkgs_cache(tool$Tool, pkgs_cache,
                                     current = tool$Ignored,
                                     ignored = tool$Repositories)
    tool$Ignored      <- pkgs_matches$Real
    tool$Repositories <- pkgs_matches$Ignored
    tool$Repositories <- setdiff(tool$Repositories, tool$Ignored)
    repositories      <- pkgs_matches$Repositories

    database$Tools[[name]] <- tool
    database$Repositories <- dplyr::bind_rows(
        database$Repositories, repositories
    )

    return(database)
}

update_categories <- function(name, database) {

    tool <- database$Tools[[name]]
    cat_str <- paste(tool$Categories, collapse = ", ")

    usethis::ui_todo(glue::glue(
        "Enter new categories. ",
        "Current categories are {usethis::ui_value(cat_str)}."
    ))

    tool$Categories <- prompt_categories(database)

    database$Tools[[name]] <- tool

    return(database)
}
