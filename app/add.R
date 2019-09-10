add_tool <- function(dir) {

    database <- load_database(dir)
    pkgs_cache <- load_pkgs_cache(dir)

    message("Please enter the details of the new tool to add")
    name <- prompt_string("Name:", allowed = "A-Za-z0-9-_",
                          not = names(database$Tools))
    platform <- prompt_string("Platform(s) (separate with '/'):",
                              allowed = "A-Za-z/+")
    code <- prompt_string("Code URL (including protocol):", exact = url_re())
    code <- stringr::str_remove(code, "/$")
    description <- ui_prompt("Description:")
    dois <- prompt_vec("DOIs (comma separated):", exact = doi_re())
    categories <- prompt_vec("Categories (comma separated):",
                             min = 1,
                             values = database$Categories$Category)

    pkgs_matches <- check_pkgs_cache(name, pkgs_cache)

    repositories <- pkgs_matches$Real
    ignored <- pkgs_matches$Ignored

    if (stringr::str_detect(code, "github.com")) {
        gh_repo <- stringr::str_remove(code, "https://github.com/")
        gh_repo <- paste(gh_repo, "GitHub", sep = "@")
        repositories <- c(repositories, gh_repo)
    }

    added <- Sys.Date()
    updated <- Sys.Date()

    tool <- new_sctool(name, platform, code, description, dois, repositories,
                       ignored, categories, added, updated)

    message("\nDetails of the new tool:\n")

    print(tool)

    correct <- prompt_yn("\nIs this correct (y/n)?")

    if (correct) {
        database$Tools[[name]] <- tool
        save_database(database, dir)
        cat("\n")
        message("Successfully added ", usethis::ui_value(name),
                " to the database!")
    } else {
        cat("\n")
        add_tool()
    }
}

prompt_string <- function(prompt, ...) {

    success <- FALSE

    while(!success) {
        string <- ui_prompt(prompt)
        success <- check_string(string, ...)
    }

    return(string)
}

prompt_vec <- function(prompt, sep = ",", min = 0, values = NULL, ...) {

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

        if (!is.null(values)) {
            if (any(!(vec %in% values))) {
                usethis::ui_oops(
                    "Some values are not correct, allowed values are:"
                )
                usethis::ui_line(usethis::ui_value(values))
                next
            }
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

check_string <- function(string, allowed = "A-Za-z0-9", whitespace = TRUE,
                         start = "A-Za-z", exact = NULL, not = NULL) {

    allowed <- paste0("^[", allowed, "]+$")
    start   <- paste0("^[", start, "]")

    if (!is.null(exact)) {
        if (any(!stringr::str_detect(string, exact))) {
            usethis::ui_oops("Format is not correct, please try again")
            return(FALSE)
        } else {
            return(TRUE)
        }
    }

    if (whitespace && any(stringr::str_detect(string, "[[:space:]]"))) {
        usethis::ui_oops("Cannot contain whitespace, please try again")
        return(FALSE)
    }

    if (any(!stringr::str_detect(string, start))) {
        usethis::ui_oops("Starts with incorrect character, please try again")
        return(FALSE)
    }

    if (any(!stringr::str_detect(string, allowed))) {
        usethis::ui_oops("Some characters not allowed, please try again")
        return(FALSE)
    }

    if (any(string %in% not)) {
        usethis::ui_oops("That value already exists, please try again")
        return(FALSE)
    }

    return(TRUE)
}


url_re <- function() {

    valid_chars <- rex::rex(except_some_of(".", "/", " ", "-"))

    rex::rex(
        start,

        # protocol identifier (optional) + //
        rex:::group(list("http", rex:::maybe("s")), "://"),

        # host name
        rex:::group(
            rex:::zero_or_more(valid_chars, rex:::zero_or_more("-")),
            rex:::one_or_more(valid_chars)
        ),

        # domain name
        rex:::zero_or_more(
            ".",
            rex:::zero_or_more(valid_chars, rex:::zero_or_more("-")),
            rex:::one_or_more(valid_chars)
        ),

        # TLD identifier
        rex:::group(".", valid_chars %>% rex:::at_least(2)),

        # resource path (optional)
        rex:::maybe("/", non_space %>% rex:::zero_or_more()),

        end
    )
}

doi_re <- function() {
    rex::rex(
        start,

        rex:::or(
            # DOI
            rex::rex(
                rex:::group("10."),
                rex:::between(digit, 4, 9),
                rex:::group("/"),
                rex:::some_of(alnum, "-", "_", ".", ":", ";", "(", ")", "/")
            ),

            # arXiv
            rex::rex(
                rex:::group("arxiv/"),
                rex:::n_times(digit, 4),
                rex:::group("."),
                rex:::between(digit, 4, 5)
            )
        ),

        end
    )
}
