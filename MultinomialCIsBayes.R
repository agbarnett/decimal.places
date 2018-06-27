# MultinomialCIsBayes.R
# Multinomial CIs
# March 2018
library(R2WinBUGS)

multiCIs = function(nums, thin = 3, MCMC = 5000){
  N = length(nums)

# create external text file with bugs model
model.file = 'multi.bugs.txt'
bugs = file(model.file , 'w')
cat('model{
    nums[1:N] ~ dmulti(p[1:N],1)
    p[1:N] ~ ddirch(alpha[1:N])
    }', file=bugs)
close(bugs)

# prepare the random data
bdata = list(N = N, nums=as.numeric(nums), alpha=rep(1, N))
inits = list(P=rep(1/N, N)) # initial values

# run BUGS
parms = c('p')
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file,
                    n.chains=1, n.iter=MCMC*thin, debug=F, DIC=F,
                    bugs.directory="c:/Program Files/WinBUGS14")
# save results
to.return = data.frame(n=as.numeric(nums), bugs.results$summary[,c(1,3,7)])
names(to.return) = c('n','mean','lower','upper')
return(to.return)

}
