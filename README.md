# decimal.places
How many decimal places are used in abstracts? Code for paper in F1000: "Missing the point: are journals using the ideal number of decimal places?", published here: https://f1000research.com/articles/7-450/

The key files are:

1. make.data.R, finds eligible articles from specified journal lists, then reads the abstracts from Pubmed and calculates the number of decimal places making a dataset ready for analysis.

2. decimal.places.stats.Rmd, Rmarkdown file to create the analyses shown in the paper. Was run using R version 3.4.4.

3. MultinomialCIsBayes.R, calculates multinomial Dirichlet confidence intervals for the three categories of percents (too few, just right, too many) using WinBUGS. Requires WinBUGS (version 1.4) and the R package R2WinBUGS.

4. decimalplaces.R, calculates the number of decimal places and significant figures.

5. journal.meta.ii.txt and journal.meta.txt, tab-delimited lists of the eligible articles searched for percents.

The code requires the following R packages:
* diagram
* R2WinBUGS
* rentrez
* stringr
* XML

To replicate the results in the paper, use: decimal.places.stats.Rmd combined with x and y

To recreate the entire analysis data set use: make.data.R combined with x and z

