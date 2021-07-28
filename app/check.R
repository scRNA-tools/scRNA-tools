#' Check database
#'
#' Perform automatic database checks
#'
#' @param database Database object
#' @param pkgs_cache Packages cache table
#' @param dir Database directory
#' @param all Whether to check all repositories
#' @param links Whether to check for preprint-publication links
#'
#' @return Updated database object
check <- function(database, pkgs_cache, dir, all, links) {

    database <- check_repositories(database, pkgs_cache, dir, all)
    database <- check_github(database, dir)
    database <- check_licenses(database, dir)
    
    if (links) {
        database <- check_preprint_links(database, dir)
    } else {
        usethis::ui_info(paste(
            "Skipping preprint-publication links checks.",
            "Maybe you should do this if you haven't in a while?"
        ))
    }
    
    set_gitmessage_checkdone()
    
    return(database)
}

#' Check repositories
#'
#' Check for new package repository matches
#'
#' @param database Database object
#' @param pkgs_cache Packages cache table
#' @param dir Database directory
#' @param all Whether to check all repositories
#'
#' @return Updated database object
check_repositories <- function(database, pkgs_cache, dir, all) {

    if (!all) {
        last_week <- lubridate::today("UTC") - 7

        all_repos <- pkgs_cache$Repository
        pkgs_cache <- dplyr::filter(pkgs_cache, Added > last_week)

        if (nrow(pkgs_cache) == 0) {
            usethis::ui_done("No new repositories to check")
            return(database)
        }
    }

    usethis::ui_todo(glue::glue(
        "Checking {usethis::ui_value(nrow(pkgs_cache))} ",
        "package repositories..."
    ))

    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = length(database$Tools),
        clear = FALSE
    )
    pb$tick(0)
    gh_idx <- which(names(database$Tools[[1]]$Repositories) == "GitHub")
    for (name in names(database$Tools)) {
        pb$tick()
        tool <- database$Tools[[name]]

        existing <- tool$Repositories[-gh_idx]
        existing <- existing[!is.na(existing)]

        for (type in names(existing)) {
            repo <- paste(existing[type], type, sep = "@")
            if (!(repo %in% all_repos)) {
                cat("\n\n")
                usethis::ui_todo(glue::glue(
                    "{usethis::ui_value(repo)} repository for ",
                    "{usethis::ui_value(tool$Tool)} not found in packages cache"
                ))
                cat("\n")
                remove <- prompt_yn(glue::glue(
                    "Do you want to remove {usethis::ui_value(repo)} ",
                    "from {usethis::ui_value(tool$Tool)} (y/n)?:"
                ))
                if (remove) {
                    tool$Repositories[[type]] <- NA
                    database$Tools[[name]] <- tool
                    usethis::ui_todo("Commiting removed repository...")
                    save_database(database, dir)
                    set_gitmessage_checkremove(name)
                    commit_database(dir)
                }
            }
        }

        database <- update_repositories(name, database, pkgs_cache,
                                        prompt = FALSE)
        checked_tool <- database$Tools[[name]]
        new_repo <- !identical(tool$Repositories, checked_tool$Repositories)
        if (new_repo) {
            usethis::ui_todo("Commiting new repository...")
            save_database(database, dir)
            set_gitmessage_check(name)
            commit_database(dir)
        }
    }
    usethis::ui_done("Package repositories updated")
    
    save_database(database, dir)
    set_gitmessage_check_ignored()
    commit_database(dir)

    return(database)
}

#' Check GitHub repositories
#'
#' Check GitHub repositories still exist
#'
#' @param database Database object
#' @param dir Database directory
#'
#' @return Updated database object
check_github <- function(database, dir) {

    usethis::ui_todo(glue::glue(
        "Checking GitHub repositories..."
    ))
    
    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = length(database$Tools),
        clear = FALSE
    )

    pb$tick(0)
    for (name in names(database$Tools)) {
        pb$tick()
        tool <- database$Tools[[name]]
        
        github <- tool$Repositories["GitHub"]
        if (is.na(github)) {
            next
        }
        
        exists <- ping_gh_repo(github, newline = TRUE)
        if (!exists) {
            cat("\n")
            database <- update_code(name, database)
            usethis::ui_todo("Commiting new code URL...")
            save_database(database, dir)
            set_gitmessage_gh_check(name)
            commit_database(dir)
        }
    }
    
    usethis::ui_done("GitHub repositories updated")
    return(database)
}

check_licenses <- function(database, dir) {
    
    licenses <- get_tools(database$Tools)$License
    spdx_licenses <- load_spdx_licenses()
    
    usethis::ui_todo(glue::glue(
        "Checking GitHub licenses..."
    ))
    
    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = length(database$Tools),
        clear = FALSE
    )
    
    pb$tick(0)
    for (name in names(database$Tools)) {
        pb$tick()
        tool <- database$Tools[[name]]
        
        github <- tool$Repositories["GitHub"]
        if (is.na(github)) {
            next
        }
        
        license <- tool$License
        gh_license <- get_gh_license(github)
        
        if (is.na(gh_license) || gh_license == "NOASSERTION") {
            next
        }
        
        if (is.na(license) || gh_license != license) {
            cat("\n\n")
            usethis::ui_info(glue::glue(
                "Found new GitHub license for {usethis::ui_value(name)} at ", 
                "{usethis::ui_value(tool$Code)}:"
            ))
            usethis::ui_line(glue::glue(
                "{usethis::ui_field('Current license: ')}",
                "{usethis::ui_value(license)}"
            ))
            usethis::ui_line(glue::glue(
                "{usethis::ui_field('GitHub license: ')}",
                "{usethis::ui_value(gh_license)}"
            ))
            update <- prompt_yn("Do you want to use the GitHub license?:")
            
            if (update) {
                tool <- database$Tools[[name]]
                
                tool$License <- gh_license
                tool$Updated <- lubridate::today("UTC")
                
                database$Tools[[name]] <- tool
            }
        }
    }
    
    save_database(database, dir)
    set_gitmessage_license_check()
    commit_database(dir)
    
    usethis::ui_done("Licenses updated")
    return(database)
}

#' Check preprint links
#'
#' Check for new linked publications for existing preprints
#'
#' @param database Database object
#' @param dir Database directory
#'
#' @return Updated database object
check_preprint_links <- function(database, dir) {
    
    `%>%` <- magrittr::`%>%`
    
    usethis::ui_todo(glue::glue(
        "Checking for preprint-publication links..."
    ))
    
    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = length(database$Tools),
        clear = FALSE
    )
    
    pb$tick(0)
    for (name in names(database$Tools)) {
        pb$tick()
        tool <- database$Tools[[name]]
        
        preprints_to_check <- database$References %>%
            dplyr::filter(
                DOI %in% tool$DOIs,
                Preprint,
                !arXiv
            ) %>%
            dplyr::filter(
                !(DOI %in% database$RefLinks$Preprint)
            )
        
        if (nrow(preprints_to_check) == 0) {
            next
        }
        
        for (idx in seq_len(nrow(preprints_to_check))) {
            linked_ref <- get_linked_ref(
                preprints_to_check[idx, ],
                print_query = TRUE
            )
            
            if (nrow(linked_ref) > 0) {
                database$References <- dplyr::bind_rows(
                    database$References,
                    linked_ref
                )
                
                database$RefLinks <- dplyr::bind_rows(
                    database$RefLinks,
                    attr(linked_ref, "Links")
                )
                
                tool$DOIs <- c(tool$DOIs, linked_ref$DOI)
                tool$Updated <- lubridate::today("UTC")
                
                database$Tools[[name]] <- tool
                
                usethis::ui_todo("Commiting new reference...")
                save_database(database, dir)
                set_gitmessage_update(name)
                commit_database(dir)
            }
            
        }
    }
    
    usethis::ui_done("Preprint linking complete")
    return(database)
}
