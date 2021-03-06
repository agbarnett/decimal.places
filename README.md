# decimal.places
How many decimal places are used in abstracts? Code for paper in F1000: "Missing the point: are journals using the ideal number of decimal places?", published here: https://f1000research.com/articles/7-450/.

The key files are:

1. `make.data.R`, finds eligible articles from specified journal lists, then reads the abstracts from Pubmed and calculates the number of decimal places making a dataset ready for analysis.

2. `Analysis.Ready.RData`, `Analysis.Ready.xlsx` and `Analysis.Ready.txt`, the main dataset for analysis produced by `make.data.R` in R, Excel and tab-delimited format, respectively. 

3. `decimal.places.stats.Rmd`, Rmarkdown file to create the analyses shown in the paper. Was run using R version 3.4.4.

4. `MultinomialCIsBayes.R`, calculates multinomial Dirichlet confidence intervals for the three categories of percents (too few, just right, too many) using WinBUGS. Requires WinBUGS (version 1.4) and the R package R2WinBUGS.

5. `decimalplaces.R`, calculates the number of decimal places and significant figures.

6. `journal.meta.txt` & `journal.meta.ii.txt`, tab-delimited lists of the eligible articles searched for percents. Split into two because of the time needed to make the data. `journal.meta.RData` & `journal.meta.ii.RData` are R-data versions of same.

The code requires the following R packages:
* diagram
* R2WinBUGS
* rentrez
* stringr
* XML

To replicate the results in the paper, use: `decimal.places.stats.Rmd` combined with `Multi.results.RData` and `Analysis.Ready.RData`. If `Multi.results.RData` does not exist, it will be created by the Rmarkdown code.

To recreate the entire analysis data set use: `make.data.R` optionally combined with `journal.meta.RData` and `journal.meta.ii.RData` to skip the step to create the lists of papers.

Archived data and code as at time of initial publication (version 1.0): http://doi.org/10.5281/zenodo.1213574 and after comments from peer reviewers (version 1.1): http://doi.org/10.5281/zenodo.1300056.
