new_sctool <- function(name, platform, code, description, dois, repositories,
                       ignored, categories, added, updated) {

    tool <- list(
        Tool         = name,
        Platform     = platform,
        Code         = code,
        Description  = description,
        DOIs         = dois,
        Repositories = repositories,
        Ignored      = ignored,
        Categories   = categories,
        Added        = added,
        Updated      = updated
    )

    structure(tool, class = "sctool")
}

print.sctool <- function(x) {

    cat(usethis::ui_field(x$Tool), "\n\n")
    cat(usethis::ui_value(x$Description), "\n\n")
    cat(usethis::ui_field("Platform:"), usethis::ui_value(x$Platform), "\n")
    cat(usethis::ui_field("Code:"), usethis::ui_value(x$Code), "\n")
    cat(usethis::ui_field("DOIs:"), usethis::ui_value(x$DOIs), "\n")
    cat(usethis::ui_field("Repositories:"),
        usethis::ui_value(x$Repositories), "\n")
    cat(usethis::ui_field("Ignored:"), usethis::ui_value(x$Ignored), "\n")
    cat(usethis::ui_field("Categories:"), usethis::ui_value(x$Categories), "\n")
    cat(usethis::ui_field("Added:"), usethis::ui_value(x$Added),
        "\t",
        usethis::ui_field("Updated:"), usethis::ui_value(x$Updated), "\n")

}
