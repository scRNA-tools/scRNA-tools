#' Check git status
#'
#' Check if there are unstaged or uncommiyted files and warn the user
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

#' Set check repository git message
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

#' Set check GitHub git message
#'
#' Set the template git commit message for checking GitHub repositories
#'
#' @param name Name of the updated tool
set_gitmessage_gh_check <- function(name) {
    
    msg <- glue::glue(
        "# Enter the new URL\n",
        "Changed code URL for {name} to URL after check",
    )
    
    readr::write_lines(msg, ".gitmessage")
}

#' Set check removal git message
#'
#' Set the template git commit message for removing tool repositories
#'
#' @param name Name of the updated tool
set_gitmessage_checkremove <- function(name) {

    msg <- glue::glue(
        "# Enter the name of the repository (e.g. CRAN) you have removed\n",
        "Remove deprecated REPO repository for {name}",
    )

    readr::write_lines(msg, ".gitmessage")
}

#' Set check licenses git message
#'
#' Set the template git commit message for checking licenses
set_gitmessage_license_check <- function() {
    
    msg <- paste(
        "# Delete time before committing\n",
        "Completed license checks", lubridate::now("UTC")
    )
    
    readr::write_lines(msg, ".gitmessage")
}

#' Set check done git message
#'
#' Set the template git commit message for completing repository checking (to
#' capture additional ignored repositories)
set_gitmessage_checkdone <- function() {

    msg <- paste(
        "# Delete time before committing\n",
        "Completed checks", lubridate::now("UTC")
    )

    readr::write_lines(msg, ".gitmessage")
}

#' Set add category git message
#'
#' Set the template git commit message for adding a new category
#' 
#' @param name Name of the new category
#' @param description Description of the new category
set_gitmessage_addcategory <- function(name, description) {
    
    msg <- glue::glue(
        "# Check the new category description:\n",
        "Update: New {name} category",
        "\n\n",
        "{description}",
        "\n\n",
        "https://www.scrna-tools.org/tools?sort=name&cats={name}"
    )
    
    readr::write_lines(msg, ".gitmessage")
}

#' Commit database
#'
#' Commit the database directory using the template commit message
#'
#' @param dir Database directory
#' 
#' @return Invisibly whether or not the database was committed
commit_database <- function(dir) {

    status <- git2r::status()
    changes <- purrr::map_dbl(status, function(.x) {
        sum(stringr::str_detect(.x, dir))
    })
    
    if (sum(changes) == 0) {
        usethis::ui_info("No database changes, commit skipped")
        if (fs::file_exists(".gitmessage")) {
            fs::file_delete(".gitmessage")
        }
        return(invisible(FALSE))
    }
    
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

    if (fs::file_exists(".gitmessage")) {
        fs::file_delete(".gitmessage")
    }
    
    invisible(TRUE)
}
