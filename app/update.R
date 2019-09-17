#' Update tool
#'
#' Update a tool entry in the scRNA-tools databse
#'
#' @param database Database object
#' @param pkgs_cache Packages cache table
#' @param name Name of the tool to update
#'
#' @return Updated database object
update_tool <- function(database, pkgs_cache, name = NULL) {

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
            c("Name", "Platform", "Description", "Code", "License", "DOIs",
              "Repositories", "Ignored", "Categories")
        )

        database <- switch(field,
            Name         = update_name(name, database),
            Platform     = update_platform(name, database),
            Description  = update_description(name, database),
            Code         = update_code(name, database),
            License      = update_license(name, database),
            DOIs         = update_dois(name, database),
            Repositories = update_repositories(name, database, pkgs_cache),
            Ignored      = update_ignored(name, database, pkgs_cache),
            Categories   = update_categories(name, database)
        )

        if (field == "Tool") {
            name <- database$Name
            database <- database$Database
        }

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

    database <- update_repositories(name, database, pkgs_cache, prompt = FALSE)

    usethis::ui_done(glue::glue(
        "Successfully updated {usethis::ui_value(name)}"
    ))

    return(database)
}

#' Update name
#'
#' Update the name of a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_name <- function(name, database) {

    tool <- database$Tools[[name]]
    old_name <- tool$Tool

    usethis::ui_todo(glue::glue(
        "Enter new name. Current name is {usethis::ui_value(old_name)}."
    ))

    new_name <- prompt_name(database)
    tool$Tool <- new_name
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[old_name]] <- NULL
    database$Tools[[new_name]] <- tool

    return(list(Name = new_name, Database = database))
}

#' Update platform
#'
#' Update the platform of a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_platform <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new platform. ",
        "Current platform is {usethis::ui_value(tool$Platform)}."
    ))

    tool$Platform <- prompt_platform()
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[name]] <- tool

    return(database)
}

#' Update code URL
#'
#' Update the code URL of a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_code <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new code URL. Current URL is {usethis::ui_value(tool$Code)}."
    ))

    code <- prompt_code()

    if (!is.na(code)) {
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
    }

    tool$Code <- code
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[name]] <- tool

    return(database)
}

#' Update license
#'
#' Update the software license of a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_license <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new license. ",
        "Current license is {usethis::ui_value(tool$License)}."
    ))

    tool$License <- prompt_license()
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[name]] <- tool

    return(database)
}

#' Update DOIs
#'
#' Update the DOIs assoiciated of a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
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
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[name]] <- tool
    database$References <- dplyr::bind_rows(database$References, refs)

    return(database)
}

#' Update description
#'
#' Update the description of a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_description <- function(name, database) {

    tool <- database$Tools[[name]]

    usethis::ui_todo(glue::glue(
        "Enter new description. ",
        "Current description is {usethis::ui_value(tool$Description)}."
    ))

    tool$Description <- prompt_description()
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[name]] <- tool

    return(database)
}

#' Update repositories
#'
#' Update the repositories of a tool.
#'
#' @param name Name of the tool to update
#' @param database Database object
#' @param pkgs_cache Packages cache table
#' @param prompt Whether to prompt the user
#'
#' @details
#' If `prompt` is `TRUE` the user can select which repository they want to
#' update. If `prompt` is `FALSE` only automatic checks against names in the
#' packages cache are performed.
#'
#' @return Updated database object
update_repositories <- function(name, database, pkgs_cache, prompt = TRUE) {

    `%>%` <- magrittr::`%>%`

    tool <- database$Tools[[name]]

    if (prompt) {
        type <- prompt_menu(
            "Which repository do you want to update?:",
            c("Bioc", "CRAN", "PyPI", "Conda")
        )

        usethis::ui_todo(glue::glue(
            "Enter new repository. ",
            "Current {usethis::ui_value(type)} repository is ",
            "{usethis::ui_value(tool$Repositories[type])}."
        ))

        repo <- prompt_string(paste(type, "repository:"),
                              allowed = "A-Za-z0-9_-")
        tool$Repositories[type] <- repo
    }

    matches <- tolower(pkgs_cache$Name) == tolower(tool$Tool)
    matches <- matches & !(pkgs_cache$Repository %in% tool$Ignored)

    if (length(matches) > 0) {
        pkg_matches <- pkgs_cache[matches, ]

        for (idx in seq_len(nrow(pkg_matches))) {
            pkg_name <- pkg_matches$Name[idx]
            pkg_type <- pkg_matches$Type[idx]
            current  <- tool$Repositories[pkg_type]

            if (!is.na(current) && pkg_name == current) {
                next
            }

            cat("\n")
            is_real <- prompt_yn(glue::glue(
                "Is {usethis::ui_value(pkg_name)} a matching ",
                "{usethis::ui_value(pkg_type)} package for ",
                "{usethis::ui_value(name)} (y/n)?:"
            ))

            if (is_real) {
                tool$Repositories[pkg_type] <- pkg_name
            } else {
                tool$Ignored <- c(tool$Ignored, pkg_matches$Repository[idx])
            }
        }
    }

    database$Tools[[name]] <- tool

    return(database)
}

# update_ignored <- function(name, database, pkgs_cache) {
#
#     tool <- database$Tools[[name]]
#     old_repos <- sort(tool$Repositories)
#
#     ignored_str <- paste(tool$Ignored, collapse = ", ")
#     usethis::ui_todo(glue::glue(
#         "Enter new ignored repositories. ",
#         "Current ignored repositories are {usethis::ui_value(ignored_str)}."
#     ))
#
#     tool$Ignored <- prompt_vec("Ignored:", values = pkgs_cache$Repository)
#
#     pkgs_matches <- check_pkgs_cache(tool$Tool, pkgs_cache,
#                                      current = tool$Ignored,
#                                      ignored = tool$Repositories)
#     tool$Ignored      <- pkgs_matches$Real
#     tool$Repositories <- pkgs_matches$Ignored
#     tool$Repositories <- setdiff(tool$Repositories, tool$Ignored)
#
#     if (!identical(sort(tool$Repositories), old_repos)) {
#         tool$Updated <- lubridate::today()
#     }
#
#     database$Tools[[name]] <- tool
#     database$Repositories <- dplyr::bind_rows(
#         database$Repositories, pkgs_matches$Repositories
#     )
#
#     return(database)
# }

#' Update categories
#'
#' Update the categories for a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_categories <- function(name, database) {

    tool <- database$Tools[[name]]
    cat_str <- paste(tool$Categories, collapse = ", ")

    usethis::ui_todo(glue::glue(
        "Enter new categories. ",
        "Current categories are {usethis::ui_value(cat_str)}."
    ))

    tool$Categories <- prompt_categories(database)
    tool$Updated <- lubridate::today("UTC")

    database$Tools[[name]] <- tool

    return(database)
}
