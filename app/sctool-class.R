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

    cat(x$Tool, "\n\n")
    cat(x$Description, "\n\n")
    cat("Platform:", x$Platform, "\n")
    cat("Code:", x$Code, "\n")
    cat("DOIs:", paste(x$DOIs, collapse = ", "), "\n")
    cat("Repositories:", paste(x$Repositories, collapse = ", "), "\n")
    cat("Ignored:", paste(x$Ignored, collapse = ", "), "\n")
    cat("Categories:", paste(x$Categories, collapse = ", "), "\n")
    cat("Added:", as.character(x$Added),
        "\tUpdated:", as.character(x$Updated), "\n")

}
