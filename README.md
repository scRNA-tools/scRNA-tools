# Single-cell software

A table of software for the analysis of single-cell RNA-seq data. To make it
into the table software must be available for download and public use somewhere
(CRAN, Bioconductor, PyPI, Conda, Github, Bitbucket, a private website etc). If
you just want to view the table or do some basic sorting/filtering a live
version is available [here](https://goo.gl/4wcVwn).

## Purpose

This table is designed to be an overview of the currently available software, it
is unlikely to be 100% complete or accurate but will be updated as new software
becomes available. If you notice a problem or would like to add something please
make a pull request or open a issue.

## Structure

The table has the following columns:

* **Name**
* **Platform** - Programming language or plaform where it can be used
* **DOI** - Publication DOI
* **Pub Date** - Publication date. Preprints are marked with PREPRINT and will
  be updated when published.
* **Code** - URL for publically available code.
* **Description**
* ***FUNCTION COLUMNS*** (Described below)
* **Added** - Date when the entry added.
* **Updated** - Date when the entry was last updated.

### Function columns

The function columns are TRUE/FALSE columns indicating if the software has a
particular function. These are designed to be used as filters, for example when
looking for software to accomplish a particular task. They are also the most
likely to be inaccurate as software is frequently updated and it is hard to
judge all the functions without making significant use of it. The function
columns ask the following questions of the software:

* **Quantification** - Does it quantify expression from reads?
* **QC** - Does it perform some type of quality control of cells?
* **Normalisation** - Does it perform some type of normalisation?
* **Imputation** - Can it impute missing dropout values?
* **Gene Filtering** - Does it perform some filtering of genes?
* **Clustering** - Does it perform clustering of cells?
* **Pseudotime** - Does it perform some pseudotime/lineage tracing/trajectory of
  cells?
* **DE** - Does it do some kind of differential expression?
* **Marker Genes** - Does it identify or mark use of cell type markers?
* **Expression Patterns** - Can it find genes with interesting patterns over
  (psuedo) time?
* **Variable Genes** - Does it identify highly variable genes?
* **Gene Sets** - Does it test or make use of annotated gene sets?
* **Gene Networks** - Does it find co-regulated gene networks?
* **Cell Cycle** - Does it identify or correct for the cell cycle or cell cycle
  (or similar) genes?
* **Dimensionality Reduction** - Can it perform some type of dimensionality
  reduction?
* **Transformation** - Does it transform between expression values and some over
  measure?
* **Modality** - Does it identify or make use of modality in expression?
* **Alternative Splicing** - Does it identify alternatice splicing?
* **Rare Cells** - Does it identify rare cells types?
* **Stem Cells** - Does it identify stem cells in a population?
* **Haplotypes** - Does it make use of haplotypes or perform phasing?
* **Visualisation** - Does it do some kind of visualisation? (showing how to
  make a plot using `ggplot` or `matplotlib` doesn't count)
* **Interactive** - Does it have some kind of interactive component or a GUI?
* **Simulation** - Does it have functions for simulating scRNA-seq data?

