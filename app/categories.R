#' Print categories
#'
#' Print categories in the scRNA-tools database
#'
#' @param database Database object
show_categories <- function(database) {

    categories <- database$Categories

    usethis::ui_info(
        "There are currently {usethis::ui_value(nrow(categories))} categories"
    )
    cat("\n")

    max_length <- max(nchar(categories$Category))

    cat_header  <- crayon::bold(
        usethis::ui_field(
            stringr::str_pad("Category", max_length + 4, side = "right")
        )
    )
    desc_header <- crayon::bold(usethis::ui_value("Description"))

    print(glue::glue("{cat_header}{desc_header}"))

    purrr::walk2(
        categories$Category, categories$Description,
        function(.category, .description) {
            padded_cat <- stringr::str_pad(
                .category,
                max_length + 4,
                side = "right"
            )

            formatted_cat  <- usethis::ui_field(padded_cat)
            formatted_desc <- usethis::ui_value(.description)

            print(glue::glue("{formatted_cat}{formatted_desc}"))
        }
    )
}

#' Add category
#'
#' Add a new category to the scRNA-tools database
#'
#' @param database Database object\
#'
#' @return database with new category
add_category <- function(database) {

    cat("\n")
    usethis::ui_todo("Please enter the details of the new category to add")
    cat("\n")

    name <- prompt_string(
        "Name:",
        allowed = "A-Za-z",
        values  = database$Categories$Category,
        not     = TRUE
    )

    description <- prompt_description()
    tools <- prompt_tools(database)

    correct <- FALSE

    while (!correct) {
        cat("\n")
        usethis::ui_todo("Please check the new category is correct")
        cat("\n")

        usethis::ui_line(
            "{usethis::ui_field('Name:')} {usethis::ui_value(name)}"
        )
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('Description:')} ",
            "{usethis::ui_value(description)}"
        ))
        usethis::ui_line(
            "{usethis::ui_field('Tools:')} {usethis::ui_value(tools)}"
        )

        correct <- prompt_yn("\nIs this correct (y/n)?")

        if (!correct) {

            cat("\n")
            field <- prompt_menu(
                "What would you like to update?",
                c("Name", "Description", "Tools")
            )

            switch (field,
                Name = {
                    usethis::ui_todo(glue::glue(
                        "Enter new name. ",
                        "Current name is {usethis::ui_value(name)}."
                    ))
                    name <- prompt_string(
                        "Name:",
                        allowed = "A-Za-z",
                        values  = database$Categories$Category,
                        not     = TRUE
                    )
                },
                Description = {
                    usethis::ui_todo(glue::glue(
                        "Enter new description. ",
                        "Current description is ",
                        "{usethis::ui_value(description)}."
                    ))
                    description <- prompt_description()
                },
                Tools = {
                    usethis::ui_todo(glue::glue(
                        "Enter new tools list. ",
                        "Current tools are: {usethis::ui_value(tools)}."
                    ))
                    tools <- prompt_tools(database)
                }
            )

        }
    }

    cat("\n")
    usethis::ui_todo("Adding new category to database...")
    new_category <- tibble::tibble(
        Category    = name,
        Phase       = "Other",
        Description = description
    )
    database$Categories <- dplyr::bind_rows(database$Categories, new_category)

    usethis::ui_todo("Adding new category to tools...")
    for (tool in tools) {
        database$Tools[[tool]]$Categories <- c(
            database$Tools[[tool]]$Categories,
            name
        )
        database$Tools[[tool]]$Updated <- lubridate::today("UTC")
        usethis::ui_done("Added category to {usethis::ui_value(tool)}")
    }

    set_gitmessage_addcategory(name, description)

    return(database)
}
