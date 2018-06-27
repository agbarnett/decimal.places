# decimalplaces.R
# count the number of decimal places

decimalplaces <- function(x) {
  if(any(str_detect(pattern='\\.', string=x))==T){ #
    res = nchar(str_split(x, pattern = '\\.', simplify = T)[,2])
  }
  if(any(str_detect(pattern='\\.', string=x))==F){ # no decimal places
    res = rep(0, length(x))
  }
  return(res)
}

# significant figures
significant.figures <- function(x) {
  # if just a zero 
  if(x==0){
    res = 1 
    return(res)
  }
  # remove decimal point
  x = gsub('\\.', '', x)
  # remove up to five leading zeros
  for (k in 1:5){
    x = gsub('^0', '', x)
  }
  # count remaining digits
  res = str_count(pattern='[0-9]', x)
  return(res)
}
