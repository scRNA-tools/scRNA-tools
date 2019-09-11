#!/usr/bin/env Rscript

#### DOCOPT ####

"
Usage:
    scrnatools add
    scrnatools update [<name>]
    scrnatools -h | --help
    scrnatools --version

Options:
  name          Name of a tool in the database
  -h --help     Show this help message.
  --version     Show version.
" -> DOCOPT

#### PACKAGES ####

suppressPackageStartupMessages({
    library(docopt)
})

#### SOURCE ####

source("app/ui.R")
source("app/sctool-class.R")
source("app/regex.R")
source("app/load.R")
source("app/add.R")
source("app/update.R")
source("app/pkgs-cache.R")
source("app/references.R")
source("app/save.R")

#### MAIN CODE ####

opts <- docopt(DOCOPT, version = "0.0.0.9000")
dir <- "TEST"

database <- load_database(dir)
pkgs_cache <- load_pkgs_cache(dir)

if (opts$add) {
    database <- add_tool(database, pkgs_cache)
}

if (opts$update) {
    database <- update_tool(database, pkgs_cache, opts$name)
}

save_database(database, dir)
