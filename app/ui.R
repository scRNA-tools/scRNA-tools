#' UI prompt
#'
#' Print a prompt and wait for user input. Correct input method is selected
#' passed on whether the session is interactive or not.
#'
#' @param prompt Prompt string
#'
#' @return response string
ui_prompt <- function(prompt) {

    prompt <- glue::glue(usethis::ui_field(prompt), " ")

    if (interactive()) {
        response <- readline(prompt)
    } else {
        cat(prompt)
        response <- readLines("stdin", n = 1)
    }

    response <- stringr::str_trim(response)

    return(response)
}

#' UI pair
#'
#' Print a field-value pair with padding between
#'
#' @param field Field string
#' @param value Field string
#' @param width Minimum width of the field plus padding. Padding is adjusted
#' based on the length of the value so that values are right aligned.
ui_pair <- function(field, value, width) {

    value_len <- nchar(as.character(value))
    width <- width - value_len
    field <- glue::glue(field, ":")
    field <- stringr::str_pad(field, width, side = "right")

    usethis::ui_line(glue::glue(
        usethis::ui_field(field),
        usethis::ui_value(value)
    ))
}

#' UI pairs list
#'
#' Print a (nested) list of field value pairs
#'
#' @param pairs_list List of field-value pairs. Names are fields and items are
#' values. Nested lists are called recursively with indenting.
#' @param indent Number of tabs to indent list.
ui_pairs_list <- function(pairs_list, indent = 0) {

    is_sublist <- purrr::map_lgl(pairs_list, is.list)

    max_field_len <- max(nchar(names(pairs_list[!is_sublist])))
    max_value_len <- max(nchar(pairs_list[!is_sublist]))
    width <- max_field_len + 2 + max_value_len

    purrr::iwalk(pairs_list, function(value, field) {
        if (is.list(value)) {
            usethis::ui_line(usethis::ui_field(glue::glue(field, ":")))
            ui_pairs_list(value, indent + 1)
        } else {
            ui_pair(glue::glue(strrep("\t", indent), field), value, width)
        }
    })
}

#' Prompt string
#'
#' Prompt the user for a single string response. User will continue to be
#' prompted until their response passes any checks.
#'
#' @param prompt Prompt string
#' @param ... Extra arguments passed to `check_input`
#'
#' @return response string
prompt_string <- function(prompt, ...) {

    success <- FALSE

    while(!success) {
        string <- ui_prompt(prompt)
        success <- check_input(string, ...)
    }

    if (string == "") {
        string <- NA
    }

    return(string)
}

#' Prompt vector
#'
#' Prompt the user for a vector of responses. User will continue to be
#' prompted until their reponse passes any checks.
#'
#' @param prompt Prompt string
#' @param sep Separator for items in the vector
#' @param min Minimum length of response vector
#' @param dups Whether to check for duplicate items in vector
#' @param ... Extra arguments passed to `check_input`
#'
#' @return response string
prompt_vec <- function(prompt, sep = ",", min = 0, dups = TRUE, ...) {

    success <- FALSE

    while(!success) {
        string <- ui_prompt(prompt)
        string <- stringr::str_remove(string, paste0(sep, "$"))

        if (string == "") {
            vec <- character()
        } else {
            vec <- stringr::str_split(string, sep, simplify = TRUE)
            vec <- stringr::str_trim(vec)
        }

        if (length(vec) < min) {
            usethis::ui_oops("Must be at least one value, please try again")
            next
        }

        if (dups && !identical(vec, unique(vec))) {
            usethis::ui_oops("Values must be unique, please try again")
            next
        }

        success <- check_input(vec, ...)
    }

    return(vec)
}

#' Prompt yes/no
#'
#' Prompt the user for a yes or no response.
#'
#' @param prompt Prompt string
#'
#' @return `TRUE` or `FALSE`
prompt_yn <- function(prompt) {

    response <- ui_prompt(prompt)

    if (response %in% c("y", "n")) {
        yn <- ifelse(response == "y", TRUE, FALSE)
    } else {
        message("Please enter 'y' or 'n'")
        yn <- prompt_yn(prompt)
    }

    return(yn)
}

#' Prompt menu
#'
#' Prompt the user to select from a menu.
#'
#' @param prompt Prompt string
#' @param options Vector of options for the menu
#'
#' @return selected option
prompt_menu <- function(prompt, options) {

    usethis::ui_line(usethis::ui_field(prompt))
    usethis::ui_line(usethis::ui_field("Provide a number to select an item"))
    cat("\n")

    for (i in seq_along(options)) {
        cat(
            glue::glue(
                "{usethis::ui_field(i)}: {usethis::ui_value(options[i])}"
            ),
            "\n"
        )
    }
    cat("\n")

    success <- FALSE

    while(!success) {
        string <- ui_prompt("Selection:")

        if (!(string %in% seq_along(options))) {
            usethis::ui_oops("Please select an item from the menu")
            next
        }

        success <- TRUE
    }

    selected <- options[as.numeric(string)]

    return(selected)
}

#' Prompt name
#'
#' Prompt for the name of a tool. Checks the name isn't already in the
#' database.
#'
#' @param database Database object
#'
#' @return Name string
prompt_name <- function(database) {
    prompt_string("Name:", allowed = "A-Za-z0-9-_",
                  values = names(database$Tools), not = TRUE)
}

#' Prompt platform
#'
#' Prompt for the platform of a tool
#'
#' @return Platform string
prompt_platform <- function() {
    prompt_string("Platform(s) (separate with '/'):",
                  allowed = "A-Za-z/+")
}

#' Prompt code
#'
#' Prompt for the code URL of a tool
#'
#' @return URL string
prompt_code <- function() {
    code <- prompt_string("Code URL (including protocol):", exact = url_re(),
                          empty = FALSE)
    stringr::str_remove(code, "/$")
}

#' Prompt license
#'
#' Prompt for the license of a tool. User can choose from a list of most common
#' licenses or enter another license.
#' 
#' @param licenses Vector of licenses for tools
#' @param spdx_license data.frame of SPDX licenses
#'
#' @return License string
prompt_license <- function(licenses, spdx_licenses) {
    
    options <- names(sort(table(licenses), decreasing = TRUE)[1:6])
    options <- c(options, "Custom", "Other", "None")
    
    license <- prompt_menu("License:", options)
    
    if (license == "Other") {
        license <- prompt_other_license(spdx_licenses)
    }
    
    if (license == "None") {
        license <- NA
    }
    
    return(license)
}

#' Prompt other license
#'
#' Prompt for another license. Input is compared to the SPDX license list and
#' user is provided a list of valid licenses to select from.
#'
#' @param spdx_licenses data.frame of SPDX licenses
#'
#' @return License string
prompt_other_license <- function(spdx_licenses) {
    
    success <- FALSE
    
    while (!success) {
        license <- prompt_string(
            "Search for a license:",
            allowed    = "-A-Za-z0-9>=.() ",
            empty      = FALSE,
            whitespace = FALSE
        )
        
        if (license %in% c(spdx_licenses$License, "Custom")) {
            success <- TRUE
        } else {
            matches <- stringdist::stringsim(
                tolower(license),
                tolower(spdx_licenses$License),
                method = "jw", p = 0.1
            ) > 0.75
            
            if (any(matches)) {
                usethis::ui_info(glue::glue(
                    "{usethis::ui_value(sum(matches))} similar licenses found"
                ))
                license <- prompt_menu(
                    "Select a license:",
                    c(spdx_licenses$License[matches], "None of these")
                )
                if (!(license == "None of these")) {
                    success <- TRUE
                }
            } else {
                usethis::ui_oops("No similar licenses found!")
            }
        }
    }
    
    return(license)
}

#' Prompt description
#'
#' Prompt for the description of a tool
#'
#' @return Description
prompt_description <- function() {
    prompt_string("Description:", allowed = NULL, start = NULL)
}

#' Prompt DOIs
#'
#' Prompt for a vector of DOIs
#'
#' @return DOI vector
prompt_dois <- function() {
    prompt_vec("DOIs (comma separated):", exact = doi_re())
}

#' Prompt categories
#'
#' Prompt for the categories of a tool. Only categories in the database are
#' allowed and there must be at least one.
#'
#' @param database Database object
#'
#' @return vector of categories
prompt_categories <- function(database) {
    prompt_vec("Categories (comma separated):",
               min = 1, values = database$Categories$Category)
}

#' Prompt tools
#'
#' Prompt for the tools associated with a category. Tools not in the database
#' will be ignored. There must be at least one present tool.
#'
#' @param database Database object
#'
#' @return vector of tools
prompt_tools <- function(database) {
    tools <- character()
    
    while (length(tools) < 1) {
        tools <- prompt_vec("Tools (comma separated):", min = 1)
        
        not_present <- !(tools %in% names(database$Tools))
        
        if (sum(not_present > 0)) {
            usethis::ui_oops(glue::glue(
                "The following tools are not in the database and will be ",
                "ignored: {usethis::ui_value(tools[not_present])}"
            ))
        }
        
        tools <- tools[!not_present]
        
        if (length(tools) == 0) {
            usethis::ui_oops("There must be at least one tool in the database")
        }
    }
    
    return(tools)
}

#' Check input
#'
#' Check the input provided by a user
#'
#' @param string The input string
#' @param allowed Set of characters that are allowed
#' @param start Set of characters the input is allowed to start with
#' @param whitespace Whether to check for whitespace in the input
#' @param empty Whether to check if the input is empty
#' @param exact Exact regular expression that input must match
#' @param values Vector of values
#' @param not If `TRUE` items in `values` are not allowed, if `FALSE` items in
#' `values` are the ONLY options that are allowed
#'
#' @return `TRUE` or `FALSE`
check_input <- function(string, allowed = "A-Za-z0-9", start = "A-Za-z",
                        whitespace = TRUE, empty = TRUE, exact = NULL,
                        values = NULL, not = FALSE) {

    if (any(string == "")) {
        if (!empty) {
            return(TRUE)
        } else {
            usethis::ui_oops("Cannot be empty, please try again")
            return(FALSE)
        }
    }

    if (!is.null(exact)) {
        if (any(!stringr::str_detect(string, exact))) {
            usethis::ui_oops("Format is not correct, please try again")
            return(FALSE)
        } else {
            return(TRUE)
        }
    }

    if (!is.null(values) && not) {
        if (any(string %in% values)) {
            usethis::ui_oops("That value already exists, please try again")
            return(FALSE)
        }
    }

    if (!is.null(values) && !not) {
        if (any(!(string %in% values))) {
            if (length(values) < 50) {
                usethis::ui_oops(
                    "Some values are not correct, allowed values are:"
                )
                usethis::ui_line(usethis::ui_value(values))
            } else {
                usethis::ui_oops(
                    "Some values are not correct, please try again"
                )
            }
            return(FALSE)
        } else {
            return(TRUE)
        }
    }

    if (!is.null(allowed)) {
        if (whitespace && any(stringr::str_detect(string, "[[:space:]]"))) {
            usethis::ui_oops("Cannot contain whitespace, please try again")
            return(FALSE)
        }

        if (!is.null(start)) {
            start <- paste0("^[", start, "]")
            if (any(!stringr::str_detect(string, start))) {
                usethis::ui_oops(
                    "Starts with incorrect character, please try again"
                )
                return(FALSE)
            }
        }

        allowed <- paste0("^[", allowed, "]+$")
        if (any(!stringr::str_detect(string, allowed))) {
            usethis::ui_oops("Some characters not allowed, please try again")
            return(FALSE)
        }
    }

    return(TRUE)
}
