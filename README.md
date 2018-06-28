![scRNA-tools](docs/img/banner.png)

# scRNA-tools

[![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

A database of software tools for the analysis of single-cell RNA-seq data. To
make it into the database software must be available for download and public use
somewhere (CRAN, Bioconductor, PyPI, Conda, GitHub, Bitbucket, a private website
etc). To view the database head to https://www.scRNA-tools.org.

## Purpose

This database is designed to be an overview of the currently available scRNA-seq
analysis software, it is unlikely to be 100% complete or accurate but will be
updated as new software becomes available. If you notice a problem or would like
to add something please make a pull request or open an issue.

## Citation

If you find the scRNA-tools database useful for your work please cite our
preprint: Zappia L, Phipson B, Oshlack A. ["Exploring the single-cell RNA-seq
analysis landscape with the scRNA-tools
database"](https://doi.org/10.1371/journal.pcbi.1006245), PLOS Computational
Biology (2018), DOI: 10.1371/journal.pcbi.1006245"

## Structure

The main tools table has the following columns:

* **Name**
* **Platform** - Programming language or platform where it can be used
* **DOIs** - Publication DOIs separated by semi-colons
* **PubDates** - Publication dates separated with semi-colons. Preprints are
  marked with PREPRINT and will be updated when published.
* **Code** - URL for publicly available code.
* **Description**
* **License** - Software license
* ***FUNCTION COLUMNS*** (Described below)
* **Added** - Date when the entry added.
* **Updated** - Date when the entry was last updated.

### Function columns

The function columns are TRUE/FALSE columns indicating if the software has a
particular function. These are designed to be used as filters, for example when
looking for software to accomplish a particular task. They are also the most
likely to be inaccurate as software is frequently updated and it is hard to
judge all the functions a package has without making significant use of it. The
function columns ask the following questions of the software:

* **Assembly** - Can it perform assembly?
* **Alignment** - Does it align reads to a reference?
* **UMIs** - Does it handle Unique Molecular Identifiers?
* **Quantification** - Does it quantify expression from reads?
* **QualityControl** - Does it perform some type of quality control of cells?
* **Normalisation** - Does it perform some type of normalisation?
* **Imputation** - Can it impute missing dropout values?
* **Integration** - Does it combine scRNA-seq datasets or other single-cell data
  types?
* **GeneFiltering** - Does it perform some filtering of genes?
* **Clustering** - Does it perform clustering of cells?
* **Classification** - Does it classify cells based on a reference dataset?
* **Ordering** - Does it order cells along a (pseudotime) trajectory?
* **DifferentialExpression** - Does it do some kind of differential expression?
* **MarkerGenes** - Does it identify or mark use of cell type markers?
* **ExpressionPatterns** - Can it find genes with interesting patterns over
  (psuedo) time?
* **VariableGenes** - Does it identify highly variable genes?
* **GeneSets** - Does it test or make use of annotated gene sets?
* **GeneNetworks** - Does it find co-regulated gene networks?
* **CellCycle** - Does it identify or correct for the cell cycle or cell cycle
  (or similar) genes?
* **DimensionalityReduction** - Can it perform some type of dimensionality
  reduction?
* **Transformation** - Does it transform between expression values and some over
  measure?
* **Modality** - Does it identify or make use of modality in expression?
* **AlternativeSplicing** - Does it identify alternatice splicing?
* **RareCells** - Does it identify rare cells types?
* **StemCells** - Does it identify stem cells in a population?
* **Variants** - Does it detect or make use of variants?
* **Haplotypes** - Does it make use of haplotypes or perform phasing?
* **AlleleSpecific** - Does it detect allele specific expression?
* **Visualisation** - Does it do some kind of visualisation? (showing how to
  make a plot using `ggplot` or `matplotlib` doesn't count)
* **Interactive** - Does it have some kind of interactive component or a GUI?
* **Simulation** - Does it have functions for simulating scRNA-seq data?

## Contributors

Thank you to everyone who has contributed to scRNA-tools! Your efforts to build
and improve this resource for the community are greatly appreciated!

The following people have made significant contributions to the scRNA-tools
database or website:

* [@seandavi](https://github.com/seandavi) - Wrote the first processing script
  and maintains the
  [awesome-single-cell](https://github.com/seandavi/awesome-single-cell) page
* [@breons](https://github.com/breons) - Helped build a prototype of the website
  (and answered many questions)
* [@daniel-wells](https://github.com/daniel-wells) - Added the licence field and
  updated code URLs
* [@katrinleinweber](https://github.com/katrinleinweber) - Switch to preferred
  resolver for DOI links

These people have added new tools or updated existing entries in the database:

[@nsalomonis](https://github.com/nsalomonis),
[@TomSmithCGAT](https://github.com/TomSmithCGAT),
[@PeteHaitch](https://github.com/PeteHaitch),
[@lucapinello](https://github.com/lucapinello)
