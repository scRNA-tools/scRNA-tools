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

prompt_string <- function(prompt, ...) {

    success <- FALSE

    while(!success) {
        string <- ui_prompt(prompt)
        success <- check_string(string, ...)
    }

    if (string == "") {
        string <- NA
    }

    return(string)
}

prompt_vec <- function(prompt, sep = ",", min = 0, dups = TRUE, ...) {

    success <- FALSE

    while(!success) {
        string <- ui_prompt(prompt)

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

        success <- check_string(vec, ...)
    }

    return(vec)
}

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

prompt_name <- function(database) {
    prompt_string("Name:", allowed = "A-Za-z0-9-_",
                  values = names(database$Tools), not = TRUE)
}

prompt_platform <- function() {
    prompt_string("Platform(s) (separate with '/'):",
                  allowed = "A-Za-z/+")
}

prompt_code <- function() {
    code <- prompt_string("Code URL (including protocol):", exact = url_re(),
                          empty = FALSE)
    stringr::str_remove(code, "/$")
}

prompt_license <- function() {
    prompt_string("License:", allowed = "-A-Za-z0-9>=.() ", empty = FALSE,
                  whitespace = FALSE)
}

prompt_description <- function() {
    prompt_string("Description:", allowed = NULL, start = NULL)
}

prompt_dois <- function() {
    prompt_vec("DOIs (comma separated):", exact = doi_re())
}

prompt_categories <- function(database) {
    prompt_vec("Categories (comma separated):",
               min = 1, values = database$Categories$Category)
}

check_string <- function(string, allowed = "A-Za-z0-9", start = "A-Za-z",
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
        } else {
            return(TRUE)
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
            start   <- paste0("^[", start, "]")
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
