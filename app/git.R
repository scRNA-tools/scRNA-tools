#' Check git status
#'
#' Check if there are unstaged or uncommited files and warn the user
check_status <- function() {

    status <- git2r::status()

    if (length(status$staged) > 0 || length(status$unstaged) > 0) {
        usethis::ui_warn(glue::glue(
            "There are uncommited files. ",
            "You should commit these before continuing."
        ))

        cat("\n")
        end <- prompt_yn("Do you want to quit (y/n)?:")

        if (end) {
            quit()
        }
    }
}

#' Set add git message
#'
#' Set the template git commit message for adding a new tool
#'
#' @param name Name of the new tool
#' @param description Description of the new tool
set_gitmessage_add <- function(name, description) {

    msg <- glue::glue(
        "New tool: {name}",
        "\n\n",
        "# Edit the description so it is suitable for Twitter\n",
        "{description}",
        "\n\n",
        "https://www.scrna-tools.org/tools#{name}"
    )

    readr::write_lines(msg, ".gitmessage")
}

#' Set update git message
#'
#' Set the template git commit message for updating a tool
#'
#' @param name Name of the updated tool
set_gitmessage_update <- function(name) {

    msg <- glue::glue(
        "# Describe the update you have made, for example:\n",
        "# Update: New TOOL publication\n",
        "Update: DESCRIPTION OF UPDATE TO {name}",
        "\n\n",
        "https://www.scrna-tools.org/tools#{name}"
    )

    readr::write_lines(msg, ".gitmessage")
}

#' Set check git message
#'
#' Set the template git commit message for checking tool repositories
#'
#' @param name Name of the updated tool
set_gitmessage_check <- function(name) {
    
    msg <- glue::glue(
        "# Enter the name of the repository (e.g. CRAN) you have updated\n",
        "Update: New REPO repository for {name}",
        "\n\n",
        "https://www.scrna-tools.org/tools#{name}"
    )
    
    readr::write_lines(msg, ".gitmessage")
}

#' Set check done git message
#'
#' Set the template git commit message for completing repository checking (to
#' capture additional ignored repositories)
set_gitmessage_checkdone <- function(name) {
    
    msg <- paste("Completed repository checks at", lubridate::now("UTC"))
    
    readr::write_lines(msg, ".gitmessage")
}

#' Commit database
#'
#' Commit the database directory using the template commit message
#'
#' @param dir Database directory
commit_database <- function(dir) {

    git2r::add(path = dir)

    err_code <- system2("git",  c("commit",  "--template", ".gitmessage"))

    if (err_code == 0) {
        usethis::ui_done("Successfully commited database!")
    } else {
        usethis::ui_oops(glue::glue(
            "Database commit failed with status code ",
            "{usethis::ui_value(err_code)}"
        ))
    }

    fs::file_delete(".gitmessage")
}
