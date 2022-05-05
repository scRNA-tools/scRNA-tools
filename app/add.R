#' Add tool
#'
#' Add a tool to the scRNA-tools database
#'
#' @param database Database object
#' @param pkgs_cache Packages cached object
#'
#' @return Database with added tool
add_tool <- function(database, pkgs_cache) {

    licenses <- get_tools(database$Tools)$License
    spdx_licenses <- load_spdx_licenses()
    
    cat("\n")
    usethis::ui_todo("Please enter the details of the new tool to add")
    cat("\n")

    name <- prompt_name(database)
    matches <- search_name(database, name)

    if (length(matches) > 1) {
        usethis::ui_info(glue::glue(
            "These tools with similar names already exist in the database: ",
            "{usethis::ui_value(matches)}"
        ))
        continue <- prompt_yn("Do you want to continue (y/n)?:")

        if (!continue) {
            return(database)
        }
    }
    
    retry_code <- TRUE
    while (retry_code) {
        code       <- prompt_code()
        retry_code <- FALSE
        has_gh     <- FALSE
        
        if (!is.na(code) && stringr::str_detect(code, "github.com")) {
            gh <- stringr::str_remove(code, "https://github.com/")
            has_gh <- ping_gh_repo(gh)
            if (has_gh) {
                usethis::ui_done(
                    "Found new GitHub repository {usethis::ui_value(gh)}"
                )
                repos <- get_repositories(database$Tools)
                all_gh <- repos$GitHub
                names(all_gh) <- repos$Tool
                all_gh <- all_gh[!is.na(all_gh)]
                if (gh %in% all_gh) {
                    matching_gh <- names(all_gh[all_gh == gh])
                    usethis::ui_info(glue::glue(
                        "The {usethis::ui_value(gh)} GitHub repository ",
                        "is already used by these tools: ",
                        "{usethis::ui_value(matching_gh)}"
                    ))
                    retry_code <- prompt_yn(
                        "Do you want to enter a new code URL (y/n)?:"
                    )
                }
            } else {
                retry_code <- prompt_yn(
                    "Do you want to enter a new code URL (y/n)?:"
                )
            }
        }
    }
    
    platform <- prompt_platform()
    
    if (has_gh) {
        license <- get_gh_license(gh)
        
        if (is.na(license) || license == "NOASSERTION") {
            license <- prompt_license(licenses, spdx_licenses)
        } else {
            usethis::ui_done(glue::glue(
                "Found GitHub license for {usethis::ui_value(name)} at ", 
                "{usethis::ui_value(code)}"
            ))
            use <- prompt_yn(glue::glue(
                "Do you want to use the {usethis::ui_value(license)}",
                "license (y/n)?:"
            ))
            if (!use) {
                license <- prompt_license(licenses, spdx_licenses)
            }
        }
    } else {
        usethis::ui_oops(glue::glue(
            "No GitHub license for {usethis::ui_value(name)} at ", 
            "{usethis::ui_value(code)}}"
        ))
        license <- prompt_license(licenses, spdx_licenses)
    }
    
    dois         <- prompt_dois()
    refs         <- get_references(dois, database$RefLinks)
    dois         <- refs$DOI
    description  <- prompt_description()
    categories   <- prompt_categories(database)

    tool <- new_sctool(name, platform, code, license, description, dois,
                       categories)

    if (has_gh) {
        tool$Repositories["GitHub"] <- gh
    }
    
    database$Tools[[name]] <- tool
    database <- update_repositories(name, database, pkgs_cache, prompt = FALSE)

    database$References <- dplyr::bind_rows(database$References, refs)
    database$RefLinks <- dplyr::bind_rows(
        database$RefLinks,
        attr(refs, "Links")
    )

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
        tool <- database$Tools[[name]]
    }

    set_gitmessage_add(tool$Tool, tool$Description)

    return(database)
}
