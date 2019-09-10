#!/usr/bin/env Rscript

#### DOCOPT ####

"
Usage:
    scrnatools add
    scrnatools -h | --help
    scrnatools --version

Options:
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
source("app/load.R")
source("app/add.R")
source("app/pkgs-cache.R")
source("app/save.R")

#### MAIN CODE ####

opts <- docopt(DOCOPT, version = "0.0.0.9000")
dir <- "TEST"

if (opts$add) {
    add_tool(dir)
}
