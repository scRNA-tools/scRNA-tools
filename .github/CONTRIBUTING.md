Contributions and suggestions for scRNA-tools.org are welcome!

If you would like to add or update a tool you have three options:

1. Submit a [new issue](https://github.com/Oshlack/scRNA-tools/issues/new)
2. Fill out the [submission form](https://www.scrna-tools.org/submit) on our
   website
3. Fork the repository and submit a pull request

If you would like to submit a pull request there are a few more details you
need to know.

All of scRNA-tools.org is based around the table in `single_cell_software.csv`.
This is the master table with an entry for each tool. A script
(`build_scRNAtools.R`) is then run on this file to produce the JSON files used
to build the website. In most cases you only need to edit
`single_cell_software.csv`, either to add a new row or update an existing row.
After editing `single_cell_software.csv` please make sure the tools are sorted
in alphabetical order and dates are in the correct format (YYYY-MM-DD).

Running `build_scRNAtools.R` is not compulsory but will make sure your tools is
added to the website as soon as possible. Please do not edit the JSON files
manually as any changes you make will be overwritten the next time
`build_scRNAtools.R` is run.

If you have any improvements/suggestions for other parts of scRNA-tools.org
(website etc.) we would love to hear those as well. Please submit an issue/pull
request in the normal way.

Thanks again for taking the time to help improve scRNA-tools.org!

