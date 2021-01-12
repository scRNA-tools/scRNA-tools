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
    while (!done) {

        field <- prompt_menu(
            "What would you like to update?",
            c("Name", "Platform", "Description", "Code", "License", "DOIs",
              "Repositories", "Ignored", "Categories")
        )

        database <- switch (field,
            Name         = update_name(name, database),
            Platform     = update_platform(name, database),
            Description  = update_description(name, database),
            Code         = update_code(name, database),
            License      = update_license(name, database),
            DOIs         = update_dois(name, database),
            Repositories = update_repositories(name, database, pkgs_cache),
            Ignored      = update_ignored(name, database),
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

    usethis::ui_done(glue::glue(
        "Successfully updated {usethis::ui_value(name)}"
    ))

    set_gitmessage_update(name)

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

    tool$Code <- code
    tool$Updated <- lubridate::today("UTC")
    tool <- update_github(tool)

    database$Tools[[name]] <- tool

    return(database)
}

update_github <- function(tool) {

    code <- tool$Code
    old_gh <- tool$Repositories["GitHub"]
    has_old_gh <- !is.na(old_gh)

    new_gh <- NA
    has_new_gh <- FALSE
    if (!is.na(code) && stringr::str_detect(code, "github.com")) {
        new_gh <- stringr::str_remove(code, "https://github.com/")
        has_new_gh <- TRUE
    }

    same_gh <- FALSE
    if (has_old_gh && has_new_gh) {
        same_gh <- new_gh == old_gh
    }

    if (has_old_gh && !same_gh) {
        tool$Repositories["GitHub"] <- NA
        usethis::ui_done(
            "Removed old GitHub repository {usethis::ui_field(old_gh)}"
        )
    }

    if (has_new_gh && !same_gh) {
        tool$Repositories["GitHub"] <- new_gh
        usethis::ui_done(
            "Found new GitHub repository {usethis::ui_field(new_gh)}"
        )
    }

    return(tool)
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
        tool$Updated <- lubridate::today("UTC")
    }

    # Avoid checking packages that aren't relevant or are already set
    if (!stringr::str_detect(tool$Platform, "Python")) {
        pkgs_cache <- dplyr::filter(pkgs_cache, Type != "PyPI")
    }

    if (!is.na(tool$Repositories["PyPI"])) {
        pkgs_cache <- dplyr::filter(pkgs_cache, Type != "PyPI")
    }

    if (!stringr::str_detect(tool$Platform, "R")) {
        pkgs_cache <- dplyr::filter(pkgs_cache, !(Type %in% c("Bioc", "CRAN")))
    }

    if (!is.na(tool$Repositories["Bioc"])) {
        pkgs_cache <- dplyr::filter(pkgs_cache, Type != "Bioc")
    }

    if (!is.na(tool$Repositories["CRAN"])) {
        pkgs_cache <- dplyr::filter(pkgs_cache, Type != "CRAN")
    }

    matches <- stringdist::stringsim(
        tolower(tool$Tool),
        tolower(pkgs_cache$Name),
        method = "jw", p = 0.1
    ) > 0.9
    matches <- matches & !(pkgs_cache$Repository %in% tool$Ignored)

    if (sum(matches) > 0) {

        cat("\n")
        usethis::ui_todo(glue::glue(
            "{usethis::ui_value(sum(matches))} ",
            "potential packages found for {usethis::ui_value(tool$Tool)}"
        ))

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
                tool$Updated <- lubridate::today("UTC")
            } else {
                tool$Ignored <- c(tool$Ignored, pkg_matches$Repository[idx])
            }
        }
    }

    database$Tools[[name]] <- tool

    return(database)
}

#' Update ignored
#'
#' Update the ignored repositories for a tool
#'
#' @param name Name of the tool to update
#' @param database Database object
#'
#' @return Updated database object
update_ignored <- function(name, database) {

    tool <- database$Tools[[name]]
    ignored <- tool$Ignored

    if (length(ignored) == 0) {
        usethis::ui_done(glue::glue(
            "There are no repositories in the ignored list for ",
            "{usethis::ui_value(name)}"
        ))

        return(database)
    }

    remove <- prompt_menu(
        "Which repository should be removed from the ignore list?:",
        ignored
    )

    tool$Ignored <- ignored[ignored != remove]

    database$Tools[[name]] <- tool

    return(database)
}

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
