# decimal.places
How many decimal places are used in abstracts? Code for paper in F1000: "Missing the point: are journals using the ideal number of decimal places?"

The files are:

1. make.data.R, reads the abstracts from Pubmed and calculates the number of decimal places making a dataset ready for analysis.

2. decimal.places.stats.Rmd, Rmarkdown file to create the analyses

3. MultinomialCIsBayes.R, calculates multinomial Dirichlet confidence intervals for the three categories of percents (too few, just right, too many) using WinBUGS. Requires WinBUGS (version 1.4) and the R package R2WinBUGS.

4. decimalplaces.R, calculates the number of decimal places and significant figures.

The code requires the following R packages:
* diagram
* R2WinBUGS
* rentrez
* stringr
* XML
