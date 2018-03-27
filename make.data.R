# make.data.R
# get data from abstracts that have percents
# abstract code adapted from here https://gist.github.com/dwinter/5983728
# for guidelines on dps see here  http://adc.bmj.com/content/100/7/608.long
# March 2018
library(rentrez)
library(XML)
library(stringr)
source('decimalplaces.R')
small.range = 0.1 # "Use two or more decimal places only if the range of values is less than 0.1%" - seems a bit harsh

## all possible patterns of percents to use in later search
percent.pattern = NULL
max.integers = 5
for (pre in 0:max.integers){ # pre-decimal place integers
  min.post = 0
  if(pre==0){min.post==1}
  for(post in min.post:max.integers){ # post-decimal place integers
    if(pre>0){pre.nums = paste(rep('[0-9]', pre), collapse = '')}
    if(pre==0){pre.nums=' '} # space to capture, for example 'was .9%' 
    if(post>0){post.nums = paste(rep('[0-9]', post), collapse = '')}
    if(post==0){post.nums=''}
    if(post==0){
      this.pattern = paste(c(pre.nums, '%'), collapse = '')
      this.pattern.space = paste(c(pre.nums, ' %'), collapse = '') # version with space between number and %
    }
    if(post>0){
      this.pattern = paste(c(pre.nums, '\\.', post.nums, '%'), collapse = '')
      this.pattern.space = paste(c(pre.nums, '\\.', post.nums, ' %'), collapse = '')
    }
    if(pre+post>0){percent.pattern = paste(c(percent.pattern, this.pattern, this.pattern.space), collapse='|')} # add to patterns using 'OR'
  }
}

# Confidence interval patterns (with spaces and all cases)
levels = c(80,90,95,99) # currently commmon numbers, could replace with two integers [0-9][0-9]?
# ci and pi with space after (plus plurals) to avoid picking up words that start with these two letters
# typo 'uncertainly' from 29171811
# 'uncertainty range' from 28045934
ci.phrases = c(
  'UR','CrI','Cl','CL','CI','UI','UR','PI','CIs','UIs','PIs','CI:','UI:','PI:','CI,','UI,','PI,','CL\\.','CI\\.','UI\\.','PI\\.','CLs\\.','CIs\\.','UIs\\.','PIs\\.',
  'cl','ci','ui','pi','cls','cis','uis','pis','cl:','ci:','ui:','ur:','pi:','ci,','ui,','ur,','pi,','cl\\.','ci\\.','ui\\.','ur\\.','pi\\.','cls\\.','cis\\.','uis\\.','urs\\.','pis\\.',
  'Confidence Interval','Posterior Interval','Credibility Interval','Prediction Interval','Credible Interval','Bayesian Credible Interval','Bayesian Interval','Confidence Limit','Uncertainty Interval','Uncertainly Interval','Uncertainty Range', # both capitals
  'Range','Confidence interval','Posterior interval','Credibility interval','Prediction interval','Credible interval','Bayesian credible interval','Bayesian interval','Confidence limit','Uncertainty interval','Uncertainly interval', 'Uncertainty range', # first capital
  'range','confidence interval','posterior interval','credibility interval','prediction interval','credible interval','bayesian credible interval','bayesian interval','confidence limit','uncertainty interval','uncertainly interval', 'uncertainty range') # no capitals
ci.pattern.spaces = NULL
for (l in levels){ # 
  for (p in ci.phrases){ # 
    this.pattern = paste(l, '%', p, collapse = '', sep=' ') # space before and after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') #
    this.pattern = paste(l, '% ', p, collapse = ' ', sep='')  # space after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') # add to patterns using 'OR'
    this.pattern = paste(l, '%', p, collapse = ' ', sep='')  # no space after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') # add to patterns using 'OR'
  }
}
ci.pattern.spaces = paste(c(ci.pattern.spaces, 'CI 95%', 'CI95%'), collapse='|') # reversed wording from 28228447 and 29176802
ci.pattern.spaces = paste(c(ci.pattern.spaces, '95% Cl'), collapse='|') # L instead of I from 28665786 
ci.pattern.spaces = paste(c(ci.pattern.spaces, '95%-CI'), collapse='|') # with dash from 29155891 
ci.pattern.spaces = paste(c(ci.pattern.spaces, 'IC 95%', 'IC95%', '95% IC'), collapse='|') # ! from 28222175 and 28245252



# Confidence interval patterns (no spaces and just lower case)
# ci and pi with space after (plus plurals) to avoid picking up words that start with these two letters
ci.phrases = c('ci','ui','pi','cis','uis','pis','ci:','ui:','pi:','ci,','ui,','pi,','ci\\.','ui\\.','pi\\.','cis\\.','uis\\.','pis\\.',
               'range','confidence interval','posterior interval','credibility interval','prediction interval','credible interval','bayesian credible interval','bayesian interval','confidence limit','uncertainty interval') # lower case; uncertainty interval from 29253411[pmid], credibility from 29307388[pmid] confidence limit from 29108797[pmid] 
ci.phrases = gsub(' ', '', ci.phrases) # remove spaces - simplifies search
ci.pattern = NULL
for (l in levels){ # 
  for (p in ci.phrases){ # 
    this.pattern = paste(l, '%', p, collapse = '', sep='')
    ci.pattern = paste(c(ci.pattern, this.pattern), collapse='|') # add to patterns using 'OR'
  }
}
# significance level
levels = c(1,5,10)
sl.phrases = c('significance','statistical significance','alpha level', 'p-value') # lower case
sl.phrases = gsub(' ', '', sl.phrases) # remove spaces - simplifies search
sl.pattern = NULL
for (l in levels){ # 
  for (p in sl.phrases){ # 
    this.pattern = paste(l, '%', p, collapse = '', sep='')
    sl.pattern = paste(c(sl.pattern, this.pattern), collapse='|') # add to patterns using 'OR'
  }
}
# paste two patterns together
ci.sl.pattern = paste(ci.pattern, sl.pattern, sep="|")

# get Pubmed IDs from selected journals in 2017
# restrict to journal articles and reviews
journals = c('BMJ','Lancet','BMJ Open','MJA','JAMA','NEJM','EHP')
journals2 = c('Nature','F1000Research','PLOS ONE','PLOS Medicine')
journals.search = paste(paste(journals, '[SO]', sep=''), collapse=' OR ', sep='')
types = c('Journal Article','Clinical Trial','Meta-Analysis','Review','Randomized Controlled Trial','Multicenter Study')
types.search = paste(paste(types, '[PT]', sep=''), collapse=' OR ', sep='')
query = paste('(', journals.search, ') AND 2017[PDAT] AND (', types.search , ')', sep='')
journal.search = entrez_search(db='pubmed', term=query, retmax=50000)
n.from.search = journal.search$count # store for flow diagram

# get meta data (loop through smaller numbers)
already.saved = T
if(already.saved==F){ # if not already saved then generate data
nums.per.loop = 20 # search for 20 papers per loop
loops = floor(journal.search$count/nums.per.loop)+1 # number of loops
meta = NULL
for (k in 1:loops){
  start = ((k-1)*nums.per.loop)+1
  stop = k*nums.per.loop
  stop = min(c(stop, journal.search$count))
  ids = journal.search$ids[start:stop]
  meta.data <- entrez_summary(db="pubmed", id=ids, always_return_list=T)
  type = paste(extract_from_esummary(meta.data, 'pubtype'), sep=' ') # can have multiple types
  j = unlist(extract_from_esummary(meta.data, 'fulljournalname'))
  frame = data.frame(type)
  frame$journal = j
  frame$pubmed = names(j)
  row.names(frame) = NULL
  meta = rbind(meta, frame)
}

# remove errata, comments, etc (small number)
index = grep('Erratum|Lectures|Conference|Comment|Biography|Guideline|Historical|Editorial|Corrected|Retraction|Retracted|News|Letter|list()', meta$type, invert=T)
dropped = nrow(meta) - length(index)
cat('Number of articles dropped=',dropped,' out of ',nrow(meta),'.\n',sep='')
meta = meta[index,]

# save for re-use
original = nrow(meta)
save(meta, query, original, dropped, n.from.search, file='journal.meta.ii.RData')
} # end of already.saved=F
if(already.saved==T){ # one or other
  #load('journal.meta.RData') # Lancet, BMJ
  #ofile = 'interim'
  load('journal.meta.ii.RData') # F1000, PLOS, Nature
  ofile = 'interim.ii'
}

# quick look at study type
tab = table(meta$type)
tab[tab>0]

# now loop one article at a time and extract data from abstract
# tricky abstracts = 29237605, 28377391
data = NULL
for (a in 1:9000){ ######## should be 1:nrow(meta) for full run
  rec <- parse_pubmed_xml(entrez_fetch(db="pubmed", id=meta$pubmed[a], rettype="xml"))
  abstract = paste(rec$abstract, collapse=' ') # one bunch of text
  abstract = gsub('Â±', ',', abstract) # replace plus/minus symbol with comma
  Encoding(abstract) = 'UTF-8' # change encoding because of things like non-separating spaces 
  abstract = gsub('·', '.', abstract) # use a consitent decimal place
  abstract = iconv(abstract, from="UTF-8", to="ASCII", sub=' ') # now convert to ASCII and replace any codes with a space
  abstract = gsub('  ',' ', abstract) # replace double spaces x 3 to be sure
  abstract = gsub('  ',' ', abstract) # replace double spaces
  abstract = gsub('  ',' ', abstract) # replace double spaces
  # make sure there's punctuation between two numbers, or full-stop space and number
  where.nums = str_locate_all(pattern='[0-9] [0-9]|\\. [0-9]', abstract)[[1]]
  if(length(where.nums)>0){
    for (i in 1:nrow(where.nums)){
      start = str_sub(abstract, 1, where.nums[i,1])
      end = str_sub(abstract, where.nums[i,2], nchar(abstract))
      abstract = paste(c(start, ',', end), sep='', collapse='') # replace space with comma
    }
  }
  # look for CIs
  abstract = str_replace_all(pattern=ci.pattern.spaces, replacement=',95% ci', string=abstract) # add comma prior to CI, otherwise it can run into numbers when spaces are removed
  abstract = gsub(' ','', abstract) # replace all spaces
  if(abstract!=''){ # if abstract is not missing
    # extract percents whilst removing confidence intervals
    percents = NA
    if(stringr::str_detect(pattern='%', string=abstract)==T){ # some mention of percents
      percents.places = str_locate_all(pattern=percent.pattern, abstract)[[1]][,1]
      ci.places = str_locate_all(pattern=ci.sl.pattern, tolower(abstract))[[1]][,1] # lower-case abstract here for matching
      full.stops = str_locate_all(pattern='\\.[A-Z]|\\.$', abstract)[[1]][,1] # full-stops followed by capital letter or last character, does not pick up ". 8" eg, 29273670
      # work out sentence number
      snums = rep(1:length(full.stops), diff(c(0,full.stops)))
      sentences = snums[percents.places] # get sentence numbers for percents
      # remove confidence intervals
      ci.match = percents.places %in% ci.places
      percents = str_extract_all(pattern=percent.pattern, abstract)[[1]] # get the percents
      percents = percents[ci.match == F] # remove 95% CIs, etc
      sentences = sentences[ci.match == F] # also remove them from sentences
      # change characters into a number
      percents = gsub('%', '', percents)
      if(length(grep('[a-z]|[A-Z]', percents))>0){ # check for text in percent
        cat('something wrong for pubmed=', meta$pubmed[a],'.\n', sep='')
      }
      dps = decimalplaces(percents) # observed decimal places, before conversion to a number
      sfs= significant.figures(percents) # observed significant figures, before conversion to a number
      percents = as.numeric(percents)
      if(length(percents)==0){percents = NA} # revert to missing if all percents are removed
    }

    ## calculate the observed difference in range
    if(is.na(percents[1])==F){ # if any percents after above processing
      # calculate difference in range in same sentences, only if more than one percent
      range.diff = Inf # start with infinite range
      if(length(percents) > 1){ # calculate difference in range of percents if there's more than one percent
        range.diff = diff(range(percents)) # overall range
        range.diff = rep(range.diff, length(percents)) # allocate to each percent
        # range per sentence (if applicable)
        dups = unique(sentences[duplicated(sentences)==T]) # sentences with more than one percent
        for (d in dups){
          index = sentences == d
          range.diff[index] = diff(range(percents[index]))
        }
      } 
      # calculate the correct number of decimal places
      correct = rep(0, length(dps)) # start with zero decimal places as correct
      correct[percents < 10] = 1 # allow one decimal place for small decimals
      correct[percents > 90 & percents < 100] = 1 # allow one decimal place for compliment (90 to 100%)
      correct[percents < 0.1] = 2 # allow two decimal places for very small numbers
      correct[percents < 0.01] = 3 # allow three decimal places for VERY small numbers
      correct[percents < 0.001] = 4 # allow four decimal places for extremely small numbers
      correct[range.diff < small.range] = 2 # allow two decimal places where range is small
      # frame of scores with meta data
      frame = data.frame(pubmed=meta[a,]$pubmed, journal=meta[a,]$journal, percents=percents, sentence.number=sentences, observed.dps=dps, observed.sfs=sfs, ideal.dps=correct, range.diff=range.diff)
      # add to final data
      data = rbind(data, frame)
    } # end of length(percents)>0
  } # end of any abstract data
  
  if(a%%100==0){cat('Up to ', a, '.\n', sep='')} ## update
  if(a%%500==0){ # step to save memory and speed up loop
    outfile = paste(ofile, a, '.RData', sep='')
    # look at difference between ideal and observed
    data$diff = sign(data$observed.dps - data$ideal.dps)
    save(data, file=outfile)
    data = NULL # blank again 
  } 
} # end of loop

# save last data set
date.extracted = as.Date(Sys.time())
outfile = paste(ofile, a, '.RData', sep='')
save(data, small.range, date.extracted, file=outfile)

## TO DO, add marker of file name
# check for doubles, check what journals appear in what files

# put data back together
files = dir(pattern='interim')
all = NULL
for (f in files){
  load(f)
  data$file = f
  data$diff = data$observed.dps - data$ideal.dps
  all = rbind(all, data)
}
data = all # rename

## small number of hand edits
# 28957347 (only one percent)
index = data$pubmed=='28957347'
data$percents[index] = 0.038; # code was foxed because text is terrible: "(-0.038 95% p = 0.027)" so no "CI"
data$observed.dps[index] = 3;
data$observed.sfs[index] = 2;
data$diff[index] = data$observed.dps[index] - data$ideal.dps[index];
# 28771524
index = data$pubmed=='28771524' & data$percents == 95
data$percents[index] = 0.053; # code was foxed because text is terrible: "(0.053 95% -0.017-0.12)" so no "CI"
data$observed.dps[index] = 3;
data$observed.sfs[index] = 2;
data$diff[index] = data$observed.dps[index] - data$ideal.dps[index];
# 28222122 - no punctuation between numbers, but works with new coding ... 
# 28926630, remove two 95%, change 2% because of comman
index = data$pubmed=='28926630' & data$percents == 95
data = data[!index,] # remove because CIs were written "95%IC"
index = data$pubmed=='28926630' & data$percents == 2
data$percents[index] = 83.2; # because '83,2'
data$observed.dps[index] = 1;
data$observed.sfs[index] = 3;
data$diff[index] = data$observed.dps[index] - data$ideal.dps[index];
# 28732014, remove all 95% because it was "(95%)" to indicate confidence interval
index = data$pubmed=='28732014' & data$percents == 95
data = data[!index,] # remove because CIs were written "95%IC"

# tidy and save
data$journal = gsub('&amp;','&', data$journal)
# last fix found by random check (zero was 4 for ideal decimal places)
index = data$percents==0
data$ideal.dps[index] = 1
save(date.extracted, small.range, data, file='Analysis.Ready.RData')
