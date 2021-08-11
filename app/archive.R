#' Archive tool
#'
#' Archive a tool in the scRNA-tools databse
#'
#' @param database Database object
#' @param dir Path to directory containing database
#' @param name Name of the tool to archive
#'
#' @return Updated database object
archive_tool <- function(database, dir, name = NULL) {

    cat("\n")
    if (!is.null(name) && !(name %in% names(database$Tools))) {
        usethis::ui_oops(glue::glue(
            "{usethis::ui_value(name)} is not present in the database"
        ))
        name <- NULL
    }

    if (is.null(name)) {
        name <- prompt_string(
            "Which tool do you want to archive?:",
            allowed = "A-Za-z0-9-_", values = names(database$Tools)
        )
    }

    cat("\n")
    usethis::ui_todo(
        "The details for {usethis::ui_value(name)} are:"
    )
    cat("\n")
    print(database$Tools[[name]])
    cat("\n")

    usethis::ui_info(glue::glue(
        "Archiving {usethis::ui_value(name)} ",
        "will remove it from the database. The details will be stored but ",
        "there is no guarantee they can be restored."
    ))
    cat("\n")

    sure <- prompt_yn(glue::glue(
        "Are you sure you want to archive {usethis::ui_value(name)}?:"
    ))

    if (sure) {
        sure <- prompt_yn("Are you absolutely sure?:")
    }

    if (sure) {

        cat("\n")

        tool <- database$Tools[[name]]
        archive_refs <- dplyr::filter(database$References, DOI %in% tool$DOIs)
        archive_ref_links <- dplyr::filter(
            database$RefLinks,
            (Preprint %in% tool$DOIs) | (Publication %in% tool$DOIs)
        )
        
        archive_data <- list(
            Tools      = database$Tools[name],
            References = archive_refs,
            RefLinks   = archive_ref_links,
            Categories = database$Categories
        )

        archive_dir <- fs::path(dir, "archive",
                                paste(lubridate::today("UTC"), name, sep = "-"))
        save_database(archive_data, archive_dir, cache = FALSE)

        database$Tools[[name]] <- NULL

        usethis::ui_done(glue::glue(
            "Archived {usethis::ui_value(name)} to ",
            "{usethis::ui_path(archive_dir)}"
        ))

    } else {
        usethis::ui_done("{usethis::ui_value(name)} was not archived")
    }

    return(database)
}

#' Restore tool
#'
#' Restore an archived tool
#'
#' @param database Database object
#' @param dir Path to directory containing database
#' @param archive Name of the archive to restore
#'
#' @return Updated database object
restore_archive <- function(database, dir, archive = NULL) {

    archive_dir <- fs::path(dir, "archive")

    if (!fs::dir_exists(archive_dir)) {
        usethis::ui_stop(glue::glue(
            "Archive directory does not exist at ",
            "{usethis::ui_path(archive_dir)}"
        ))
    }

    archive_names <- fs::path_file(fs::dir_ls(archive_dir, type = "directory"))

    cat("\n")
    if (!is.null(archive) && !(archive %in% archive_names)) {
        usethis::ui_oops(glue::glue(
            "{usethis::ui_value(archive)} is not an archive"
        ))
        archive <- NULL
    }

    if (is.null(archive)) {
        archive <- prompt_string(
            "Which archive do you want to restore?:",
            values = archive_names
        )
    }

    archive_data <- load_database(fs::path(archive_dir, archive))
    name <- archive_data$Tools[[1]]$Tool

    cat("\n")
    usethis::ui_todo(
        "The details for the archived tool {usethis::ui_value(name)} are:"
    )
    cat("\n")
    print(archive_data$Tools[[name]])
    cat("\n")

    if (name %in% names(database$Tools)) {
        usethis::ui_stop(glue::glue(
            "{usethis::ui_value(name)} already exists in the database, ",
            "this archive cannot be restored"
        ))
    }

    usethis::ui_info(glue::glue(
        "Restoring {usethis::ui_value(name)} ",
        "will add it to the database. This may fail if the archive is old."
    ))
    cat("\n")

    sure <- prompt_yn(glue::glue(
        "Are you sure you want to restore {usethis::ui_value(name)}?:"
    ))

    if (sure) {
        sure <- prompt_yn("Are you absolutely sure?:")
    }

    if (sure) {

        cat("\n")

        database$Tools[[name]] <- archive_data$Tools[[name]]
        database$References <- dplyr::bind_rows(
            database$References,
            archive_data$References
        )
        if ("RefLinks" %in% names(archive_data)) {
            database$RefLinks <- dplyr::bind_rows(
                database$RefLinks,
                archive_data$RefLinks
            )
        }
        database$Categories <- dplyr::bind_rows(
            database$Categories,
            dplyr::filter(
                archive_data$Categories,
                !(Category %in% database$Categories$Category)
            )
        )

        usethis::ui_done(glue::glue(
            "Restored {usethis::ui_value(name)} to the database"
        ))

        fs::dir_delete(fs::path(archive_dir, archive))

        usethis::ui_done(glue::glue(
            "Deleted {usethis::ui_path(fs::path(archive_dir, archive))}"
        ))

    } else {
        usethis::ui_done("{usethis::ui_value(name)} was not restored")
    }

    return(database)
}
