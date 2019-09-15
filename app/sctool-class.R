new_sctool <- function(name, platform, code, license, description, dois,
                       bioc, cran, pypi, conda, github, ignored, categories,
                       added, updated) {

    tool <- list(
        Tool         = name,
        Platform     = platform,
        Code         = code,
        License      = license,
        Description  = description,
        DOIs         = dois,
        Repositories = c(
            Bioc   = bioc,
            CRAN   = cran,
            PyPI   = pypi,
            Conda  = conda,
            GitHub = github
        ),
        Ignored      = ignored,
        Categories   = categories,
        Added        = added,
        Updated      = updated
    )

    structure(tool, class = "sctool")
}

print.sctool <- function(x) {

    repos <- x$Repositories[!is.na(x$Repositories)]
    print(repos)

    cat(usethis::ui_field(x$Tool), "\n\n")
    cat(usethis::ui_value(x$Description), "\n\n")
    cat(usethis::ui_field("Platform:"), usethis::ui_value(x$Platform), "\n")
    cat(usethis::ui_field("Code:"), usethis::ui_value(x$Code), "\n")
    cat(usethis::ui_field("License:"), usethis::ui_value(x$License), "\n")
    cat(usethis::ui_field("DOIs:"), usethis::ui_value(x$DOIs), "\n")
    cat(usethis::ui_field("Repositories:"), "\n")
    for (idx in seq_along(repos)) {
        cat("\t")
        cat(glue::glue(
                "{usethis::ui_field(names(repos[idx]))}: ",
                "{usethis::ui_value(repos[idx])}"
        ))
        cat("\n")
    }
    if (length(x$Ignored) > 0) {
        cat("\t")
        cat(usethis::ui_field("Ignored:"), usethis::ui_value(x$Ignored), "\n")
    }
    cat(usethis::ui_field("Categories:"), usethis::ui_value(x$Categories), "\n")
    cat(usethis::ui_field("Added:"), usethis::ui_value(x$Added),
        "\t",
        usethis::ui_field("Updated:"), usethis::ui_value(x$Updated), "\n")

}
